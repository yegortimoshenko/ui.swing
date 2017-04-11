(ns yegortimoshenko.swing
  (:refer-clojure :exclude [list])
  (:require [clojure.set :as set]
            [clojure.string :as str])
  (:import [clojure.lang IAtom IDeref IMeta Seqable]
           [net.miginfocom.layout AC CC LC]
           [net.miginfocom.swing MigLayout]
           [java.awt Color Component Dimension Font Insets]
           [java.awt.event ActionListener FocusListener ItemListener
            WindowListener WindowFocusListener WindowStateListener]
           [java.util List]
           [javax.swing JButton JCheckBox JComboBox JComponent JFileChooser JFrame JLabel
            JList JPanel JPasswordField JProgressBar JScrollPane JSlider JTable JTextArea
            JTextField DefaultComboBoxModel DefaultListModel DefaultListCellRenderer
            SwingConstants SwingUtilities]
           [javax.swing.event AncestorListener ChangeListener]
           [javax.swing.table DefaultTableModel DefaultTableCellRenderer TableCellEditor]
           [javax.swing.text JTextComponent]))

(defprotocol IColor
  (to-color ^Color [this]))

(extend-protocol IColor
  Color
  (to-color [this] this)
  List
  (to-color [[r g b a]]
    (if a
      (Color. r g b a)
      (Color. r g b))))

(defn to-dimension
  ([w h]
   (to-dimension nil w h))
  ([^Component c w h]
   (Dimension. ^int (or w (.getWidth c))
               ^int (or h (.getHeight c)))))

(defprotocol IFont
  (to-font [this]))

(extend-protocol IFont
  Font
  (to-font [this] this))

(defmacro ^:private sizer [m]
  `(fn [^JComponent c# ^Dimension d#] (~m c# d#)))

(def ^:private sizes
  [[(sizer .setSize) :width :height]
   [(sizer .setMinimumSize) :min-width :min-height]
   [(sizer .setMaximumSize) :max-width :max-height]])

(defn- set-sizes! [^Component c opts]
  (doseq [[f kw kh] sizes]
    (let [w (opts kw)
          h (opts kh)]
      (if (or w h) (f c (to-dimension c w h))))))

(def ^:private nop (fn [& _]))

(defmacro ^:private listener [add i ms]
  (let [handlers (gensym)
        ks (mapv (fn [m] (-> m name (str/replace #"[A-Z]" #(str \- (str/lower-case %))) keyword)) ms)
        ks->ms (zipmap ks ms)]
    `(fn [c# handlers#]
       (let [~handlers (set/rename-keys (select-keys handlers# ~ks) (quote ~ks->ms))]
         (when-not (empty? ~handlers)
           (~add c# (reify ~i ~@(for [m ms] `(~m [_# e#] ((get ~handlers (quote ~m) ~nop) e#))))))))))

(defmacro ^:private listeners [& body]
  (cons 'juxt (for [form body] (cons 'listener form))))

(def ^:private listen
  (listeners
   [.addActionListener ActionListener [actionPerformed]]
   [.addAncestorListener AncestorListener [ancestorAdded ancestorMoved ancestorRemoved]]
   [.addChangeListener ChangeListener [stateChanged]]
   [.addFocusListener FocusListener [focusGained focusLost]]
   [.addItemListener ItemListener [itemStateChanged]]
   [.addWindowListener WindowListener
    [windowActivated windowDeactivated windowClosed windowClosing windowOpened]]
   [.addWindowFocusListener WindowFocusListener [windowGainedFocus windowLostFocus]]
   [.addWindowStateListener WindowStateListener [windowStateChanged]]))

(def ^:private alignment
  {:center SwingConstants/CENTER
   :left SwingConstants/LEFT
   :right SwingConstants/RIGHT})

(defn- style! [^Component c opts]
  (set-sizes! c opts)
  (doseq [[k v] opts]
    (case k
      :align (.setHorizontalAlignment c (v alignment))
      :background (.setBackground c (to-color v))
      :foreground (.setForeground c (to-color v))
      :default (reset! ^IAtom c v)      
      :enabled? (.setEnabled c ^Boolean v)
      :font (.setFont c (to-font v))
      :listeners (listen c v)
      :name (.setName c ^String v)
      :opaque? (.setOpaque ^JComponent c ^Boolean v)
      :tooltip (.setToolTipText ^JComponent c ^String v)
      :visible? (.setVisible c ^Boolean v)
      :default)))

(def ^:dynamic *frame-visible?* true)

(defn- frame-defaults [params]
  (let [close #(-> % .getSource .dispose)]
    (->> (merge {:visible? *frame-visible?*} params)
         (merge-with merge {:listeners {:window-closing close}}))))

(defn frame
  ([c] (frame {} c))
  ([{:keys [resizable? title] :as opts} c]
   (doto (JFrame. ^String title)
     (.add ^Component c)
     (.pack)
     (.setDefaultCloseOperation JFrame/DO_NOTHING_ON_CLOSE)
     (.setResizable (boolean resizable?))
     (style! (frame-defaults opts)))))

(defn scrollable [c]
  (JScrollPane. c))

(defmacro invoke [& body]
  `(if (SwingUtilities/isEventDispatchThread)
     (do ~@body)
     (let [a# (atom nil)]
       (SwingUtilities/invokeAndWait #(reset! a# (do ~@body)))
       (deref a#))))

(defmacro component [proto args g s]
  (let [this (with-meta 'this {:tag proto})]
    `(proxy [~proto IDeref IAtom] [~@args]
       (deref [] (invoke (~g ~this)))
       (compareAndSet [old# new#]
         (invoke (let [same?# (= old# (~g ~this))]
                   (if same?# (~s ~this new#)) same?#)))
       (reset [new#]
         (invoke (~s ~this new#) (~g ~this)))
       (swap [f# & args#]
         (loop [] (let [old# (deref ~this)
                        new# (apply f# old# args#)]
                    (if-let [res# (invoke (when (= old# (~g ~this))
                                            (~s ~this new#)
                                            (~g ~this)))] res# (recur))))))))

(defn panel
  ([cs] (panel {} cs))
  ([{:keys [layout column row] :as params} cs]
   (let [p (JPanel. (MigLayout. (str layout) (str column) (str row)))]
     (doseq [c cs]
       (let [[c spec] (if (sequential? c) c [c])]
         (.add p ^Component c ^Object (str spec))))
     (doto p (style! params)))))

(defn file-chooser
  ([] (file-chooser {}))
  ([{:keys [parent path save?]}]
   (component JFileChooser []
              (fn [^JFileChooser this]
                (when (= JFileChooser/APPROVE_OPTION
                         (if save?
                           (.showSaveDialog this parent)
                           (.showOpenDialog this parent)))
                  (.getSelectedFile this)))
              .setSelectedFile)))

(defn ^:private from-model [^DefaultComboBoxModel m]
  (vec (for [i (range (.getSize m))] (.getElementAt m i))))

(defn ^:private to-model ^DefaultComboBoxModel [xs]
  (let [m (DefaultComboBoxModel.)]
    (doseq [x xs] (.addElement m x)) m))

(defn combo-box
  ([] (combo-box []))
  ([xs] (combo-box {} xs))
  ([params xs]
   (doto
     (component JComboBox [(to-array xs)] .getSelectedItem .setSelectedItem)
     (style! params))))

(defprotocol IRender
  (to-component [this ctx]))

(extend-protocol IRender
  Component
  (to-component [this _] this))

(defn ^:private positions [pred coll]
  (for [[idx elt] (map-indexed vector coll) :when (pred elt)] idx))

(defn list
  ([] (list []))
  ([xs] (list {} xs))
  ([params xs]
   (doto (component JList [^DefaultListModel (to-model xs)]
                    (fn [^JList this]
                      (set (.getSelectedValues this)))
                    (fn [^JList this ys]
                      (.setSelectedIndices this (int-array (positions (set ys) (from-model (.getModel this)))))))
     (.setCellRenderer
      (proxy [DefaultListCellRenderer] []
        (getListCellRendererComponent [parent v row selected? focus?]
          (if (satisfies? IRender v)
            (to-component v {:parent parent :row row :selected? selected? :focus? focus?})
            (proxy-super getListCellRendererComponent parent v row selected? focus?)))))
     (style! params))))

(defn- from-table-model [^DefaultTableModel model]
  (mapv vec (.getDataVector model)))

(defn- to-table-model ^DefaultTableModel [rows columns]
  (DefaultTableModel. (to-array-2d rows) (to-array columns)))

(defn table
  ([rows] (table rows (repeat (count (first rows)) (str))))
  ([rows columns] (table {} rows columns))
  ([{:keys [editable?] :as params} rows columns]
   (let [c (component JTable [(to-table-model rows columns)]
                      (fn [^JTable this]
                        (let [m (from-table-model (.getModel this))]
                          (set (map (partial nth m) (.getSelectedRows this)))))
                      (fn [^JTable this ys]
                        (.clearSelection this)
                        (doseq [idx (positions (set ys) (from-table-model (.getModel this)))]
                          (.addRowSelectionInterval this idx idx))))]
     (if (not editable?) (.setDefaultEditor c Object nil))
     (doto c
       (.setDefaultRenderer
        Object
        (proxy [DefaultTableCellRenderer] []
          (getTableCellRendererComponent [parent v selected? focus? row column]
            (if (satisfies? IRender v)
              (to-component v {:parent parent :selected? selected? :focus? focus? :row row :column column})
              (proxy-super getTableCellRendererComponent parent v selected? focus? row column)))))
       (style! params)))))

(defn button
  ([] (button ""))
  ([s] (button {} s))
  ([params s]
   (doto
     (JButton. ^String s)
     (style! params))))

(defn checkbox
  ([] (checkbox ""))
  ([s] (checkbox {} s))
  ([params s]
   (doto
     (component JCheckBox [^String s] .isSelected .setSelected)
     (style! params))))

(defn label
  ([] (label ""))
  ([s] (label {} s))
  ([params s]
   (doto
     (JLabel. ^String s)
     (style! params))))

(defn progress-bar
  ([] (progress-bar 100))
  ([end] (progress-bar 0 end))
  ([start end] (progress-bar {} start end))
  ([params start end]
   (doto
     (component JProgressBar [start end] .getValue .setValue)
     (style! params))))

(defn slider
  ([end] (slider 0 end))
  ([start end] (slider {} start end))
  ([{:keys [minor major] :as params} start end]
   (let [c (component JSlider [start end] .getValue .setValue)]
     (when (or major minor)
       (.setPaintTicks c true)
       (if major
         (doto c
           (.setLabelTable (.createStandardLabels c major))
           (.setPaintLabels true)
           (.setMajorTickSpacing major)))
       (if minor
         (doto c
           (.setMinorTickSpacing minor)
           (.setSnapToTicks true))))
     (doto c (style! params)))))

(defn password-field
  ([] (password-field {}))
  ([params]
   (doto (component JPasswordField []
                    .getPassword
                    (fn [^JPasswordField this s] (.setText this (apply str s))))
     (style! params))))

(defn text-area
  ([] (text-area {}))
  ([params]
   (doto
     (component JTextArea [] .getText .setText)
     (style! params))))

(defn text-field
  ([] (text-field {}))
  ([params]
   (doto
     (component JTextField [] .getText .setText)
     (style! params))))
