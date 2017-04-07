(ns yegortimoshenko.swing
  (:refer-clojure :exclude [list])
  (:import [clojure.lang IAtom IDeref IMeta Seqable]
           [net.miginfocom.layout AC CC LC]
           net.miginfocom.swing.MigLayout
           [java.awt Color Component Dimension Font Insets]
           java.awt.event.ActionListener
           java.util.List
           [javax.swing JButton JCheckBox JComboBox JComponent JFileChooser JFrame JLabel
            JList JPanel JPasswordField JProgressBar JScrollPane JSlider JTable
            JTextArea JTextField DefaultComboBoxModel DefaultListModel
            SwingConstants SwingUtilities]
           javax.swing.table.DefaultTableModel
           javax.swing.text.JTextComponent))

(defn panel
  ([components] (panel {} components))
  ([{:keys [layout column row]} components]
   (let [p (JPanel.)]
     (.setLayout p (MigLayout. (str layout) (str column) (str row)))
     (doseq [c components]
       (let [[c spec] (if (sequential? c) c [c])]
         (.add p ^Component c ^Object (str spec)))) p)))

(defmacro invoke [& body]
  `(let [a# (atom nil)]
     (SwingUtilities/invokeAndWait #(reset! a# (do ~@body)))
     (deref a#)))

(defmacro component [proto args g & [s]]
  (let [this (with-meta 'this {:tag proto})]
    `(proxy [~proto ~@(if g `[IDeref]) ~@(if s `[IAtom])] [~@args]
       ~@(if g `[(deref [] (invoke (~g ~this)))])
       ~@(if s `[(compareAndSet [old# new#] (invoke (let [same?# (= old# (~g ~this))]
                                                      (if same?# (~s ~this new#)) same?#)))
                 (reset [new#] (invoke (~s ~this new#) (~g ~this)))
                 (swap [f# & args#] (loop [] (let [old# (deref ~this)
                                                   new# (apply f# old# args#)]
                                               (if-let [res# (invoke (when (= old# (~g ~this))
                                                                       (~s ~this new#)
                                                                       (~g ~this)))] res# (recur)))))]))))

(defprotocol IColor
  (to-color ^Color [this]))

(extend Color
  IColor
  {:to-color (fn [this] this)})

(extend List
  IColor
  {:to-color (fn [[r g b a]]
               (if a
                 (Color. r g b a)
                 (Color. r g b)))})

(defn to-dimension
  ([w h]
   (to-dimension nil w h))
  ([^Component c w h]
   (Dimension. ^int (or w (.getWidth c))
               ^int (or h (.getHeight c)))))

(defprotocol IFont
  (to-font [this]))

(extend Font
  IFont
  {:to-font (fn [this] this)})

(defmacro ^:private sizer [m]
  `(fn [^JComponent c# ^Dimension d#] (~m c# d#)))

(defn- style
  [^Component c
   {:keys [width height
           pref-width pref-height
           min-width min-height
           max-width max-height] :as opts}]
  (doseq [[f w h] [[(sizer .setSize) width height]
                   [(sizer .setPreferredSize) pref-width pref-height]
                   [(sizer .setMinimumSize) min-width min-height]
                   [(sizer .setMaximumSize) max-width max-height]]]
    (if (or w h)
      (f c (to-dimension c w h))))
  (doseq [[k v] opts]
    (case k
      :background (.setBackground c (to-color v))
      :foreground (.setForeground c (to-color v))
      :enabled? (.setEnabled c ^Boolean v)
      :font (.setFont c (to-font v))
      :opaque? (.setOpaque ^JComponent c ^Boolean v)
      :tooltip (.setToolTipText ^JComponent c ^String v)
      :visible? (.setVisible c ^Boolean v)
      :default)))

(defn file-chooser
  ([] (file-chooser {}))
  ([{:keys [parent path save?]}]
   (component JFileChooser []
              (fn [^JFileChooser this]
                (when (= JFileChooser/APPROVE_OPTION
                         (if save?
                           (.showSaveDialog this parent)
                           (.showOpenDialog this parent)))
                  (.getSelectedFile this))))))

(defn frame
  ([c] (frame {} c))
  ([{:keys [resizable? submit title visible?] :or {} :as opts} c]
   (let [f (JFrame. ^String title)]
     (if resizable? (.setResizable f resizable?))
     (if submit (-> f .getRootPane (.setDefaultButton submit)))
     (if c (.add f ^Component c))
     (doto f .pack (style (conj opts (when (nil? visible?) [:visible? true])))))))

(defn scrollable [c]
  (JScrollPane. c))

(defn- from-model [^DefaultComboBoxModel m]
  (vec (for [i (range (.getSize m))] (.getElementAt m i))))

(defn- to-model ^DefaultComboBoxModel [xs]
  (let [m (DefaultComboBoxModel.)]
    (doseq [x xs] (.addElement m x)) m))

(defn combo-box
  ([] (combo-box []))
  ([xs] (combo-box {} xs))
  ([opts xs]
   (let [c (component JComboBox [(to-model xs)]
                      (fn [^JComboBox this] (from-model (.getModel this)))
                      (fn [^JComboBox this ys] (.setModel this (to-model ys))))]
     (doto c (style opts)))))

(defn list
  ([] (list []))
  ([xs] (list {} xs))
  ([opts xs]
   (let [c (component JList [(to-model xs)]
                      (fn [^JList this] (from-model (.getModel this)))
                      (fn [^JList this ys] (.setModel this (to-model ys))))]
     (doto c (style opts)))))

(defn- from-table-model [^DefaultTableModel model]
  (let [columns (for [i (range (.getColumnCount model))]
                  (.getColumnName model i))]
    (into [(vec columns)] (.getDataVector model))))

(defn- to-table-model [xs]
  (DefaultTableModel. (to-array-2d (rest xs)) (to-array (first xs))))

(defn table
  ([] (table []))
  ([xs] (table {} xs))
  ([{:keys [sort?] :as opts} xs]
   (let [c (component JTable [^DefaultTableModel (to-table-model xs)]
                      (fn [^JTable this] (from-table-model (.getModel this)))
                      (fn [^JTable this ys] (.setModel this (to-table-model ys))))]
     (if sort? (.setAutoCreateRowSorter c true))
     (doto c (style opts)))))

(def ^:private alignment
  {:center SwingConstants/CENTER
   :left SwingConstants/LEFT
   :right SwingConstants/RIGHT})

(defn button
  ([] (button ""))
  ([s] (button {} s))
  ([{:keys [align] :as opts} s]
   (let [c (component JButton [^String s] .getText .setText)]
     (if align
       (.setHorizontalAlignment ^JButton c (align alignment)))
     (doto c (style opts)))))

(defn checkbox
  ([] (checkbox ""))
  ([s] (checkbox s false))
  ([s b] (checkbox {} s b))
  ([opts s b]
   (doto (component JCheckBox [^String s ^Boolean b] .isSelected .setSelected) (style opts))))

(defn label
  ([] (label ""))
  ([s] (label {} s))
  ([{:keys [align] :as opts} s]
   (doto (component JLabel [^String s ^int ((or align :left) alignment)] .getText .setText) (style opts))))

(defn progress-bar
  ([] (progress-bar 100))
  ([end] (progress-bar 0 end))
  ([start end] (progress-bar {} start end))
  ([opts start end] (doto (component JProgressBar [start end] .getValue .setValue) (style opts))))

(defn slider
  ([end] (slider 0 end))
  ([start end] (slider start end (quot (- end start) 2)))
  ([start end val] (slider {} start end val))
  ([{:keys [minor major] :as opts} start end val]
   (let [c (component JSlider [start end val] .getValue .setValue)]
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
     (doto c (style opts)))))

(defn password-field
  ([] (password-field ""))
  ([s] (password-field {} s))
  ([opts s] (doto (component JPasswordField [^String s] .getPassword .setText) (style opts))))

(defn text-area
  ([] (text-area ""))
  ([s] (text-area {} s))
  ([opts s] (doto (component JTextArea [^String s] .getText .setText) (style opts))))

(defn text-field
  ([] (text-field ""))
  ([s] (text-field {} s))
  ([opts s] (doto (component JTextField [^String s] .getText .setText) (style opts))))
