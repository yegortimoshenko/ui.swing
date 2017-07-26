(ns yegortimoshenko.ui.swing
  (:refer-clojure :exclude [get list])
  (:require (clojure [set :as set] [string :as str]))
  (:import (java.awt Component Dimension Insets Window)
           (javax.swing AbstractButton JButton JCheckBox JComboBox JComponent JEditorPane
                        JFrame JLabel JList JPanel JPasswordField JProgressBar JScrollPane JSlider
                        JSpinner JTable JTextArea JTextField DefaultComboBoxModel DefaultListCellRenderer
                        SwingConstants SwingUtilities)
           (javax.swing.table DefaultTableModel DefaultTableCellRenderer TableCellEditor)
           (javax.swing.text JTextComponent GapContent PlainDocument)
           (net.miginfocom.swing MigLayout)))

(alias 'this *ns*)

(defn ^:private deep-merge [& maps]
  (if (every? map? maps)
    (apply merge-with deep-merge maps)
    (last maps)))

(def ^:dynamic *frame-visible?* true)
(def ^:private not-false? (complement false?))

(defn event->window [e]
  (let [source (.getSource e)]
    (if (instance? Window source)
      (identity source)
      (SwingUtilities/getWindowAncestor source))))

(declare finalize)

(defn frame
  ([child] (frame {} child))
  ([{:keys [resizable? title] :as params} child]
   (doto (JFrame.)
     (.add ^Component child)
     (.pack)
     (.setDefaultCloseOperation JFrame/DO_NOTHING_ON_CLOSE)
     (.setResizable (not-false? resizable?))
     (.setTitle title)
     (finalize (deep-merge {:listeners {:window-closing (fn [e] (.dispose (event->window e)))}
                            :visible? *frame-visible?*} params)))))

(defn panel
  ([children] (panel {} children))
  ([{:keys [layout column row] :as params} children]
   (let [this (JPanel. (MigLayout. layout column row))]
     (doseq [child children]
       (let [[component spec] (if (sequential? child) child [child])]
         (.add this ^Component component ^Object (str spec))))
     (finalize this params))))

(defn scrollable [c]
  (JScrollPane. c))

(defprotocol Place
  (get [this])
  (set! [this v]))

(defmacro invoke
  "Ensures that body is invoked on Swing EDT thread"
  [& body]
  `(if (SwingUtilities/isEventDispatchThread)
     (do ~@body)
     (let [a# (atom nil)]
       (SwingUtilities/invokeAndWait #(reset! a# (do ~@body)))
       (deref a#))))

(defn as-atom [place]
  (reify
    clojure.lang.IDeref
    (deref [_] (invoke (this/get place)))
    clojure.lang.IAtom
    (compareAndSet [_ old new]
      (invoke (let [same? (= old (this/get place))]
                (if same? (this/set! place new)) same?)))
    (reset [_ new]
      (invoke (this/set! place new) (this/get place)))
    (swap [this f & args]
      (loop [] (let [old (deref this)
                     new (apply f old args)]
                 (or (invoke (when (= old (this/get place))
                               (this/set! place new)
                               (this/get place)))
                     (recur)))))))

(defn ^:private from-model [^DefaultComboBoxModel m]
  (vec (for [i (range (.getSize m))] (.getElementAt m i))))

(defn ^:private to-model ^DefaultComboBoxModel [xs]
  (let [m (DefaultComboBoxModel.)]
    (doseq [x xs] (.addElement m x)) m))

(defn- from-table-model [^DefaultTableModel model]
  (mapv vec (.getDataVector model)))

(defn- to-table-model ^DefaultTableModel [rows columns]
  (DefaultTableModel. (to-array-2d rows) (to-array columns)))

(defprotocol Field
  (field ^Place [this]))

(definterface Resettable
  (^void reset [^chars cs]))

(defn ^:private positions [pred coll]
  (for [[idx elt] (map-indexed vector coll) :when (pred elt)] idx))

(extend-protocol Field
  JPasswordField
  (field [this]
    (reify Place
      (get [_] (.getPassword this))
      ^{:doc "https://bugs.openjdk.java.net/browse/JDK-8075513"}
      (set! [_ cs1]
        (let [content
              (proxy [GapContent Resettable] []
                (reset [^chars cs2]
                  (.replace ^GapContent this 0 0 cs2 ^int (count cs2))))
              document (PlainDocument. content)]
          (.reset content ^chars cs1)
          (.setDocument this document)))))
  JTextComponent
  (field [this]
    (reify Place
      (get [_] (.getText this))
      (set! [_ s] (.setText this s))))
  JCheckBox
  (field [this]
    (reify Place
      (get [_] (.getSelected this))
      (set! [_ v] (.setSelected this v))))
  JComboBox
  (field [this]
    (reify Place
      (get [_] (.getSelectedItem this))
      (set! [_ v] (.setSelectedItem this v))))
  JList
  (field [this]
    (reify Place
      (get [_] (set (.getSelectedValuesList this)))
      (set! [_ s]
        (.setSelectedIndices this (int-array (positions s (from-model (.getModel this))))))))
  JTable
  (field [this]
    (reify Place
      (get [_]
        (let [m (from-table-model (.getModel this))]
          (set (map (partial nth m) (.getSelectedRows this)))))
      (set! [_ ys]
        (.clearSelection this)
        (doseq [idx (positions (set ys) (from-table-model (.getModel this)))]
          (.addRowSelectionInterval this idx idx)))))
  JProgressBar
  (field [this]
    (reify Place
      (get [_] (.getValue this))
      (set! [_ x] (.setValue this ^long x))))
  JSlider
  (field [this]
    (reify Place
      (get [_] (.getValue this))
      (set! [_ x] (.setValue this x))))
  JSpinner
  (field [this]
    (reify Place
      (get [_] (.getValue this))
      (set! [_ x] (.setValue this x)))))

(defn password-field
  ([] (JPasswordField.))
  ([params] (finalize (JPasswordField.) params)))

(defn editor-pane
  ([] (JEditorPane.))
  ([params] (finalize (JEditorPane.) params)))

(defn text-area
  ([] (JTextArea.))
  ([{:keys [wrap?] :as params}]
   (let [this (JTextArea.)]
     (when wrap?
       (.setLineWrap this true)
       (.setWrapStyleWord this true))
     (finalize this params))))

(defn text-field
  ([] (JTextField.))
  ([params] (finalize (JTextField.) params)))

(defprotocol View
  (view ^Place [this]))

(extend-protocol View
  AbstractButton
  (view [this]
    (reify Place
      (get [_] (.getText this))
      (set! [_ s] (.setText this s))))
  JFrame
  (view [this]
    (reify Place
      (get [_] (.getTitle this))
      (set! [_ s] (.setTitle this s))))
  JLabel
  (view [this]
    (reify Place
      (get [_] (.getText this))
      (set! [_ s] (.setText this s)))))

(defn button
  ([] (JButton.))
  ([^String s] (JButton. s))
  ([params ^String s] (finalize (JButton. s) params)))

(defn label
  ([] (JLabel.))
  ([^String s] (JLabel. s))
  ([params ^String s]
   (finalize (JLabel. s) params)))

(defprotocol Listener
  (attach [this component]))

(extend-protocol Listener
  java.awt.event.ActionListener
  (attach [this component] (.addActionListener component this))
  javax.swing.event.AncestorListener
  (attach [this component] (.addAncestorListener ^JComponent component this))
  javax.swing.event.ChangeListener
  (attach [this component] (.addChangeListener component this))
  java.awt.event.ItemListener
  (attach [this component] (.addItemListener component this))
  java.awt.event.MouseListener
  (attach [this component] (.addMouseListener ^Component component this))
  java.awt.event.WindowListener
  (attach [this component] (.addWindowListener ^Window component this))
  java.awt.event.WindowFocusListener
  (attach [this component] (.addWindowFocusListener ^Window component this))
  java.awt.event.WindowStateListener
  (attach [this component] (.addWindowFocusListener ^Window component this)))

(def ^:private nop (fn [& _]))

(defn ^:private camel->kebab [s]
  (str/replace s #"[A-Z]" #(str \- (str/lower-case %))))

(defmacro listener [interface]
  (let [methods (map (fn [^java.lang.reflect.Method m] (symbol (.getName m)))
                     (.getDeclaredMethods ^Class (resolve interface)))
        keywords (mapv #(-> % name camel->kebab keyword) methods)
        keywords->methods (zipmap keywords methods)
        handlers (gensym)]
    `(fn [component# handlers#]
       (let [~handlers (set/rename-keys (select-keys handlers# ~keywords) (quote ~keywords->methods))]
         (when-not (empty? ~handlers)
           (attach (reify ~interface
                     ~@(for [method methods]
                         `(~method [_# e#] ((or (~handlers (quote ~method)) ~nop) e#)))) component#))))))

(defn ^:private class->symbol [^Class k]
  (symbol (.getName k)))

(defn ^:private listeners [interfaces]
  (->> (map class->symbol interfaces)
       (map (fn [i] `(listener ~i)))
       (cons 'juxt)
       (eval)))

(listeners (extenders Listener))

(def ^:private listeners-memo (memoize listeners))

(defn listen [component handlers]
  ((listeners-memo (extenders Listener)) component handlers))

(def ^:private alignment
  {:center SwingConstants/CENTER
   :left SwingConstants/LEFT
   :right SwingConstants/RIGHT})

(defprotocol Color
  (as-color [this]))

(extend-protocol Color
  java.awt.Color
  (as-color [this] this)
  java.util.List
  (as-color [[^int r ^int g ^int b a]]
    (if a
      (java.awt.Color. r g b ^int a)
      (java.awt.Color. r g b))))

(defprotocol Font
  (as-font [this]))

(extend-protocol Font
  java.awt.Font
  (as-font [this] this))

(defprotocol Point
  (as-point [this]))

(extend-protocol Point
  java.awt.Point
  (as-point [this] this)
  java.awt.Window
  (as-point [this] (.getLocationOnScreen this))
  java.util.List
  (as-point [[x y]] (java.awt.Point. x y)))

(defn ->dimension
  ([w h]
   (->dimension nil w h))
  ([^Component c w h]
   (Dimension. (or w (.getWidth c))
               (or h (.getHeight c)))))

(defmacro ^:private sizer [m]
  `(fn [^JComponent c# ^Dimension d#] (~m c# d#)))

(def ^:private sizes
  [[(sizer .setSize) [:width :height]]
   [(sizer .setMinimumSize) [:min-width :min-height]]
   [(sizer .setMaximumSize) [:max-width :max-height]]
   [(sizer .setPreferredSize) [:pref-width :pref-height]]])

(defn ^:private resize [^Component c params]
  (doseq [[f [kw kh]] sizes]
    (let [w (params kw)
          h (params kh)]
      (if (or w h) (f c (->dimension c w h))))))

(defn finalize [^Component c params]
  (resize c params)
  (doseq [[k v] params]
    (case k
      :align (.setHorizontalAlignment c (v alignment))
      :background (.setBackground c (as-color v))
      :foreground (.setForeground c (as-color v))
      :default (this/set! (field c) v)
      :enabled? (.setEnabled c ^Boolean v)
      :font (.setFont c (as-font v))
      :listeners (listen c v)
      :name (.setName c ^String v)
      :opaque? (.setOpaque ^JComponent c ^Boolean v)
      :tooltip (.setToolTipText ^JComponent c ^String v)
      :visible? (.setVisible c ^Boolean v)
      :default)) c)

(defprotocol Renderable
  (render ^Component [this ctx]))

(extend-protocol Renderable
  Component
  (render [this _] this))

(defmacro ^:private renderer [klass method binding]
  (let [binding (into (array-map) (map (juxt (comp keyword name) gensym) binding))
        result (gensym)]
    `(proxy [~klass] []
       (~method ~(vec (vals binding))
         (if (satisfies? Renderable ~(:this binding))
           (let [~result (render ~(:this binding) ~(dissoc binding :this))]
             (if (instance? Component ~result) ~result (proxy-super ~method ~@(vals (assoc binding :this result)))))
           (proxy-super ~method ~@(vals binding)))))))

(def ^:private list-cell-renderer
  (renderer DefaultListCellRenderer
            getListCellRendererComponent
            [parent this row selected? focus?]))

(defn combo-box
  ([] (combo-box {} []))
  ([xs] (combo-box {} xs))
  ([params xs]
   (doto (JComboBox. (to-model xs))
     (.setRenderer list-cell-renderer))))

(defn list
  ([] (list {} []))
  ([xs] (list {} xs))
  ([params xs]
   (doto (JList. (to-model xs))
     (.setCellRenderer list-cell-renderer))))

(defn table
  ([rows] (table {} rows))
  ([{:keys [columns editable?] :or {columns (repeat (count (first rows)) (str))} :as params} rows]
   (let [this (JTable. (to-table-model rows columns))]
     (if (not editable?) (.setDefaultEditor this Object nil))
     (doto this
       (.setDefaultRenderer
        Object
        (renderer DefaultTableCellRenderer
                  getTableCellRendererComponent
                  [parent this selected? focus? row column]))
       (finalize params)))))

(defn checkbox
  ([] (JCheckBox.))
  ([^String s] (JCheckBox. s))
  ([params ^String s] (finalize (JCheckBox. s) params)))

(defn progress-bar
  ([] (doto (JProgressBar.) (.setIndeterminate true)))
  ([end] (JProgressBar. 0 end))
  ([start end] (JProgressBar. start end))
  ([params start end]
   (finalize (JProgressBar. start end) params)))

(defn slider
  ([end] (JSlider. 0 end))
  ([start end] (JSlider. start end))
  ([{:keys [minor major] :as params} start end]
   (let [this (JSlider. start end)]
     (when (or major minor)
       (.setPaintTicks this true)
       (if major
         (doto this
           (.setLabelTable (.createStandardLabels this major))
           (.setPaintLabels true)
           (.setMajorTickSpacing major)))
       (if minor
         (doto this
           (.setMinorTickSpacing minor)
           (.setSnapToTicks true))))
     (finalize this params))))
