(ns clj-int.example
  (:use [clj-int.widgets])
  (:import [java.awt
	    BorderLayout
	    GridLayout]
	   [javax.swing
	    JScrollPane
	    JFrame]))

;Demo frame.
(defn test-frame []
  (let [test-label (make-widget (struct widget :label {:text "test label"}))
	test-label-2 (make-widget (struct widget :label {:text "Text widget #2"}))
	test-text-input (make-widget (struct widget :text-field {:initial "Enter text here"
								 :editable true}))
	test-text-area (make-widget (struct widget :text-area {:initial "Text will also appear here:"
							       :editable false
							       :size [30 20]
							       :wrap true}))
	test-scroll (JScrollPane.)
	test-input-panel (make-widget (struct widget :panel {:layout (BorderLayout.)
							     :contents [{:obj test-text-area
									 :layout BorderLayout/CENTER}
									{:obj test-scroll
									 :layout BorderLayout/EAST}
									{:obj test-text-input
									 :layout BorderLayout/SOUTH}]}))
	;; a button that writes entered text to label and textarea
	test-button (make-widget (struct widget :button {:text "test button"
							 :callback (fn [e] (let [txt (.getText test-text-input)]
									     (.setText test-label txt)
									     (.append test-text-area (str \newline txt))))}))
	test-panel (make-widget (struct widget :panel {:layout (GridLayout. 2 1)
						       :contents [test-label
								  test-button]}))
	test-frame (make-widget (struct widget :frame {:name "clj-int test frame"
						       :layout (BorderLayout.)
						       :size [350 550]
						       :contents [{:obj test-label-2
								   :layout BorderLayout/NORTH}
								  {:obj test-input-panel
								   :layout BorderLayout/CENTER}
								  {:obj test-panel 
								   :layout BorderLayout/SOUTH}]
						       :on-close JFrame/HIDE_ON_CLOSE}))]
    (doto test-scroll
      (.setViewportView test-text-area)) ;;had to add this for scroll finally to work 
    (doto test-frame
      (.pack)
      (.setVisible true))
    test-frame))