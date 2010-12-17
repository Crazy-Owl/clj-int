(ns clj-int.widgets
  (:import [javax.swing
	    JFrame
	    JButton
	    JPanel
	    JTextField
	    JTextArea
	    JLabel
	    ImageIcon]
	   [java.io File]
	   [java.awt.event
	    ActionListener]))

;; Test usage: (use 'clj-int.widgets) (test-frame)

;function-to-listener proxy
(defn create-listener [f]
  (proxy [ActionListener]
      []
    (actionPerformed [e] (f e))))

;Widget struct, :properties-map is a map holding various properties
(defstruct widget :type :properties-map)

;Multimethod to make widgets
(defmulti make-widget :type)

;Frame method
(defmethod make-widget :frame [s]
	   (let [frame (JFrame.)
		 props (:properties-map s)]
	     (if-let [name (:name props)]
	       (.setTitle frame name))
	     (if-let [contents (:contents props)] ;`aer        jjjjjjjjjjjj gggggggggggggggggm     mc
	       (let [panel (JPanel.)]
		 (if-let [layout (:layout props)]
		   (.setLayout panel layout))
		 (doseq [wid contents]
		   (if (map? wid)
		     (.add panel (:obj wid) (:layout wid))
		     (.add panel wid)))
		 (.add frame panel)))
	     (if-let [[xs ys] (:size props)]
	       (.setSize frame xs ys))
	     (if-let [close-op (:on-close props)]
	       (.setDefaultCloseOperation frame close-op)
	       (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE))
	     frame))

;Label widget
(defmethod make-widget :label [s]
	   (let [label (JLabel.)
		 props (:properties-map s)]
	     (if-let [text (:text props)]
	       (.setText label text))
	     (if-let [icon (:icon props)]
	       (.setIcon label icon))
	     (if-let [align (:align props)]
	       (.setAlign label align))
	     label))

;;Panel widget
(defmethod make-widget :panel [s]
	   (let [panel (JPanel.)
		 props (:properties-map s)]
	     (if-let [layout (:layout props)]
	       (.setLayout panel layout))
	     (if-let [contents (:contents props)]
	       (doseq [wid contents]
		 (if (map? wid)
		   (.add panel (:obj wid) (:layout wid))
		   (.add panel wid))))
	     panel))

;Button widget, has :callback property, which holds a lambda
(defmethod make-widget :button [s]
	   (let [button (JButton.)
		 props (:properties-map s)]
	     (if-let [callback (:callback props)]
	       (.addActionListener button (create-listener callback)))
	     (if-let [text (:text props)]
	       (.setText button text))
	     (if-let [icon (:icon props)]
	       (.setIcon button icon))
	     button))

;;Text field widget
(defmethod make-widget :text-field [s]
	   (let [field (JTextField.)
		 props (:properties-map s)]
	     (let [editable (:editable props)]
	       (if (not (nil? editable))
		 (.setEditable field editable)))
	     (if-let [text (:initial props)]
	       (.setText field text))
	     field))

;;Text area widget
(defmethod make-widget :text-area [s]
	   (let [area (JTextArea.)
		 props (:properties-map s)]
	     (let [editable (:editable props)]
	       (if (not (nil? editable))
		 (.setEditable area editable)))
	     (if-let [text (:initial props)]
	       (.setText area text))
	     (if-let [[cols rows] (:size props)]
	       (do
		 (.setColumns area cols)
		 (.setRows area rows)))
	     (if-let [wrap (:wrap props)]
	       (.setLineWrap area wrap))
	     (if-let [wrap-words (:wrap-words props)]
	       (.setWrapStyleForward area wrap-words))
	     area))

;Default implementation adds a label instead of not implemented control
(defmethod make-widget :default [s]
	   (make-widget (struct widget :label {:text "Unimplemented yet"})))

