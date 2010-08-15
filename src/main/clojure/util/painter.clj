(ns util.painter
  (:import (javax.swing JFrame JPanel JLabel JMenu JMenuBar JMenuItem JComponent)
           (java.awt.event ActionListener MouseListener MouseAdapter)
           (java.awt GridLayout Color)))

; My first try at swing
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

(defn read-image [path]
  (javax.imageio.ImageIO/read (java.io.File. path)))

(defn transforming-canvas [transform]
  (doto (proxy [JComponent] []
          (paint [g]
            (doto g
              (.setColor Color/WHITE)
              (.fillRect 0 0 20 20)
              (.setTransform transform)
              (.setColor Color/BLACK)
              (.drawRect 50 50 50 50)
              (.fillOval 100 100 100 100))))
    (.setOpaque true)
    (.setDoubleBuffered true)))


(defn panel-for [image]
  (add-mouse-listeners
    (doto (proxy [JPanel] [(GridLayout. 0 1)]
      (paintComponent [g]
        (.drawImage g image 0 0 nil)))
      (.setBorder (javax.swing.BorderFactory/createLineBorder Color/BLACK))
      (.setBackground Color/RED)
      (.setPreferredSize (java.awt.Dimension. 365 280))
      (.setOpaque false))))

(defn in-frame [painter]
  (doto (JFrame. "Painter frame")
    (.setContentPane (panel-for (read-image "src/main/resources/pic.jpg")))
    (.setJMenuBar menu)
    ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.setVisible true)))

