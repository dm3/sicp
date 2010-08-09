(ns util.painter
  (:import (javax.swing JFrame JPanel JLabel JMenu JMenuBar JMenuItem)
           (java.awt.event ActionListener MouseListener MouseAdapter)
           (java.awt GridLayout Color)))

(def exit-listener
  (proxy [ActionListener] []
    (actionPerformed [evt] (System/exit 0))))

(def menu
  (doto (JMenuBar.)
    (.add (doto (JMenu. "Menu")
      (.add (doto (JMenuItem. "Close")
        (.addActionListener exit-listener)))))))

(defn- moveSquare [x y]
  (print "moving to " x y))

(defn- add-mouse-listeners [component]
  (doto component
    (.addMouseListener (proxy [MouseAdapter] []
                         (mousePressed [e]
                           (moveSquare (.getX e) (.getY e)))))
    (.addMouseMotionListener (proxy [MouseAdapter] []
                               (mouseDragged [e]
                                 (moveSquare (.getX e) (.getY e)))))))


(defn panel-for [image]
  (add-mouse-listeners
    (doto (proxy [JPanel] [(GridLayout. 0 1)]
      (paintComponent [g]
        (.drawString g "Custom!" 10 20)
        (.setColor g Color/RED)
        (.fillRect g 50 50 20 20)
        (.setColor g Color/BLACK)
        (.drawRect g 50 50 20 20)))
       ;(.drawImage g image 0 0 g))))
      (.setBorder (javax.swing.BorderFactory/createLineBorder Color/BLACK))
      (.setBackground Color/RED)
      (.setPreferredSize (java.awt.Dimension. 200 200))
      (.setOpaque false))))

(defn in-frame [painter]
  (doto (JFrame. "Painter frame")
    (.setContentPane (panel-for nil))
    (.setJMenuBar menu)
    ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.setVisible true)))

