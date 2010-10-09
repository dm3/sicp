(ns util.painter
  (:import (javax.swing JFrame JPanel JLabel JMenu JMenuBar JMenuItem JComponent)
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

(defn read-image [path]
  (javax.imageio.ImageIO/read (java.io.File. path)))

; shape to be drawn inside transforming-canvas
(defn rectangle
  " @param g - Graphics object
    @param dim - Dimension of the parent component "
  [g dim]
  (let [r-width (/ (.width dim) 2)
        r-height (/ (.height dim) 2)
        r-x (/ (.width dim) 4)
        r-y (/ (.height dim) 4)]
  (doto g
    (.drawRect r-x r-y r-width r-height)
    (.setColor Color/BLACK)
    (.fillRect r-x r-y r-width r-height))))

(defn transforming-canvas
  " @param transform - AffineTransform to be applied to the drawn shape
    @param shape - a transformation function :: Graphics -> () "
  [transform shape]
  (doto (proxy [JComponent] []
          (paint [g]
            (let [size (proxy-super getSize)]
              (do
                (doto g ;background
                  (.setColor Color/WHITE)
                  (.fillRect 0 0 (.width size) (.height size))
                  (.setTransform transform))
                (shape g size)))))
    (.setOpaque true)
    (.setDoubleBuffered true)))

(defn panel-for [transform]
  (doto (JPanel. (GridLayout. 0 1))
  ; (doto (proxy [JPanel] [(GridLayout. 0 1)]
  ;   (paintComponent [g]
  ;     (.drawImage g image 0 0 nil)))
    (.setBorder (javax.swing.BorderFactory/createLineBorder Color/BLACK))
    (.setBackground Color/RED)
    (.setPreferredSize (java.awt.Dimension. 365 280))
    (.setOpaque false)
    (.add (transforming-canvas transform rectangle))))

(defn in-frame [painter]
  (doto (JFrame. "Painter frame")
    (.setContentPane (panel-for painter))
    (.setJMenuBar menu)
    ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.setVisible true)))

;; API
(defn rect [transform]
  (in-frame transform))

(defn ident [transform] transform)
(defn right-split
