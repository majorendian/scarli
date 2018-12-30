
(require :scarli)

(defpackage scarli-test-6
  (:use :cl :scarli :scarli-player :scarli-objects))

(in-package :scarli-test-6)

(defparameter *width* 640)
(defparameter *height* 480)

(defparameter *testscene*
  (make-instance 'scene
                 :layers (list
                           (make-instance 'layer :name "bottom")
                           (make-instance 'layer :name "middle")
                           )
                 :width 640
                 :height 480))

(defparameter *player* (get-default-player 64 64 "test_spritesheet.png"))

(defclass pushable-block (solid-tile)
  ())

(defmethod object-ready ((self pushable-block))
  (<- self 'dir #(0 0))
  (setf (object-movable self) t))



(defmethod object-on-collide ((self pushable-block) (collider object))
  (if (string= (object-name collider) "player")
      (cond
        ((> (object-x collider) (+ (object-x self) (- (object-width self) 4))) (<- self 'dir #(-1 0)))
        ((< (+ (object-x collider) (object-width collider)) (+ (object-x self) 4)) (<- self 'dir #(1 0)))
        ((> (rect-y (object-collision-rect collider)) (+ (object-y self) (- (object-height self) 4))) (<- self 'dir #(0 -1)))
        ((< (+ (rect-y (object-collision-rect collider)) (rect-h (object-collision-rect collider))) (+ (object-y self) 4)) (<- self 'dir #(0 1)))

        )
      ;else just collide and stay in place
      (basic-collision self collider 
                       :top (lambda (self collider)
                              (setf (object-y self) (+ (object-y collider) (object-height collider))))
                       :bottom (lambda (self collider)
                                 (setf (object-y self) (- (object-y collider) (object-height collider))))
                       :left (lambda (self collider)
                               (setf (object-x self) (+ (object-x collider) (object-width collider))))
                       :right (lambda (self collider)
                                (setf (object-x self) (- (object-x collider) (object-width collider))))
                       )
      ))

(defmethod object-update ((self pushable-block) dt)
  (object-move self (* 50 (aref (-> self 'dir) 0)) (* 50 (aref (-> self 'dir) 1)) dt)
  (setf (rect-x (object-collision-rect self)) (object-x self))
  (setf (rect-y (object-collision-rect self)) (object-y self))
  (when (not (object-is-colliding self))
    (<- self 'dir #(0 0)))
  )

(defparameter *pushable-block* (make-tile :tile-sheet-path "tile_sheet.png"
                                          :tile-size 32
                                          :tile-class 'pushable-block
                                          :x (* 10 32)
                                          :y (* 10 32)
                                          :ci 0
                                          :ri 2))

(defparameter *pushable-block-2* (make-tile :tile-sheet-path "tile_sheet.png"
                                          :tile-size 32
                                          :tile-class 'pushable-block
                                          :x (* 12 32)
                                          :y (* 10 32)
                                          :ci 0
                                          :ri 2))

(defparameter *sign* (make-tile :tile-sheet-path "tile_sheet.png"
                                :tile-size 32
                                :tile-class 'interactible
                                :x (* 14 32)
                                :y (* 11 32)
                                :ci 1
                                :ri 2))
(setf (interactible-pages *sign*) (list 
                                    (list "This is a sign. It is old and wooden.")
                                    (list "There seems to be something written on it,"
                                          "but it is impossible to make out.")))

(defparameter *camera* (make-instance 'camera
                                      :w 640
                                      :h 480
                                      :parent *player*))



(add-obj-to-scene *testscene* "middle" *player*)
(add-obj-to-scene *testscene* "middle" *pushable-block*)
(add-obj-to-scene *testscene* "middle" *pushable-block-2*)
(add-obj-to-scene *testscene* "middle" *sign*)
(clear-input-handlers)
(add-input-handler *player*)
;(sb-sprof:start-profiling :max-samples 10000)
(display-tiles *testscene* "level_1.map")
(main *testscene* *camera* *width* *height*)
;(sb-sprof:stop-profiling)
;(sb-sprof:report :type :flat)
