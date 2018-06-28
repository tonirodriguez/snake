; -----------------------------------------------------------------
; Snake Example from the book "Programming Clojure 2nd Edition"
; -----------------------------------------------------------------

(ns snake.core
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener))
   (:use snake.import-static))
(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)
; -----------------------------------------------------------------
; Functional Model
; -----------------------------------------------------------------
; Start: constants
(def width 75)
(def height 50)
(def point-size 10)
(def turn-millis 75)
(def win-length 5)
(def dirs { VK_LEFT  [-1  0]
            VK_RIGHT [ 1  0]
            VK_UP    [ 0 -1]
            VK_DOWN  [ 0  1]})
; END: constants

; START: board math
(defn add-points [& pts]
  (vec (apply map + pts)))

(defn point-to-screen-rect [pt]
  (map #(* point-size %)
       [(pt 0) (pt 1) 1 1]))
; END: board math

; START: apple
(defn create-apple []
  {:location [(rand-int width) (rand-int height)]
   :color (Color. 210 50 90)
   :type :apple})
; END: apple

; START: snake
(defn create-snake []
  {:body (list [1 1])
   :dir [1 0]
   :type :snake
   :color (Color. 15 160 70)})
; END: snake

; START: move
(defn move [{:keys [body dir] :as snake} & grow]
  (assoc snake :body (cons (add-points (first body) dir)
                           (if grow body (butlast body)))))
; END: move

; START: turn
(defn turn [snake newdir]
  (assoc snake :dir newdir))
; END: turn

; START: win
(defn win? [{body :body}]
  (>= (count body) win-length))
; END: win

; START: heads-overlap-body
(defn heads-overlaps-body? [{[head & body] :body}]
  (contains? (set body) head))
; END: heads-overlap-body

; START: lose
(def lose? heads-overlaps-body?)
; END: lose

; START: eats-apple
(defn eats? [{[snake-head] :body} {apple :location}]
  (= snake-head apple))
; END: eats-apple

; -----------------------------------------------------------------
; Mutable Model with STM
; -----------------------------------------------------------------
(defn reset-game [snake apple]
  (dosync (ref-set apple (create-apple))
          (ref-set snake (create-snake))))

(defn update-direction [snake newdir]
  (when newdir (dosync (alter snake turn newdir))))

(defn update-positions [snake apple]
  (dosync
    (if (eats? @snake @apple)
      (do (ref-set apple (create-apple))
          (alter snake move :grow))
      (alter snake move)))
  nil)

; -----------------------------------------------------------------
; Snake GUI
; -----------------------------------------------------------------
(defn fill-point [g pt color]
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color)
    (.fillRect g x y width height)))

(defmulti paint (fn [g object & _] (:type object)))

(defmethod paint :apple [g {:keys [location color]}]
  (fill-point g location color))

(defmethod paint :snake [g {:keys [body color]}]
  (doseq [point body]
    (fill-point g point color)))


(defn game-panel [frame snake apple]

  (proxy [JPanel ActionListener KeyListener] []

    (paintComponent [g] ; <label id="code.game-panel.paintComponent"/>
      (proxy-super paintComponent g)
      (paint g @snake)
      (paint g @apple))
    (actionPerformed [e] ; <label id="code.game-panel.actionPerformed"/>
      (update-positions snake apple)
      (when (lose? @snake)
	      (reset-game snake apple)
	      (JOptionPane/showMessageDialog frame "You lose!"))
      (when (win? @snake)
	      (reset-game snake apple)
	      (JOptionPane/showMessageDialog frame "You win!"))
      (.repaint this))

    (keyPressed [e] ; <label id="code.game-panel.keyPressed"/>
      (update-direction snake (dirs (.getKeyCode e))))

    (getPreferredSize []
      (Dimension. (* (inc width) point-size)
		  (* (inc height) point-size)))
    (keyReleased [e])
    (keyTyped [e])))


(defn game []
  (let [snake (ref (create-snake))
        apple (ref (create-apple))
        frame (JFrame. "Snake")
        panel (game-panel frame snake apple)
        timer (Timer. turn-millis panel)]

  (doto panel
    (.setFocusable true)
    (.addKeyListener panel))
  (doto frame
    (.add panel)
    (.pack)
    (.setVisible true))
  (.start timer)
  [snake, apple, timer]))
