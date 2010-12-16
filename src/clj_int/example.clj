(ns clj-int.example
  (:use [clj-int.widgets])
  (:import [java.awt
	    BorderLayout
	    GridLayout]
	   [javax.swing JFrame]))

;Demo frame.
(defn test-frame []
  (let [test-label (make-widget (struct widget :label {:text "test label"}))
	test-label-2 (make-widget (struct widget :label {:text "Text widget #2"}))
	;; a button that writes a text to label
	test-button (make-widget (struct widget :button {:text "test button"
							 :callback (fn [e] (let [txt (.getText test-label)]
									     (.setText test-label (str txt 1))))}))
	test-layout (GridLayout. 2 1) ;layout
	test-panel (make-widget (struct widget :panel {:layout test-layout
						       :contents [test-label
								  test-button]}))
	test-frame (make-widget (struct widget :frame {:name "Test frame"
						       :layout (BorderLayout.)
						       :contents [{:obj test-label-2
								   :layout BorderLayout/NORTH}
								  {:obj test-panel 
								   :layout BorderLayout/SOUTH}]
						       :size [500 500]
						       :on-close JFrame/HIDE_ON_CLOSE}))]
    (.setVisible test-frame true)
    test-frame))