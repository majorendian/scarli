;(require :scarli)

(defpackage scarli-objects
  (:use :cl :scarli)
  (:export interactible
           interactible-pages
           pushable-block
           entrance
           entrance-next-scene
           entrance-func-load
           entrance-next-player-pos))

(in-package :scarli-objects)

(defclass interactible (solid-tile)
  ((pages :accessor interactible-pages :initarg :pages :initform (list (list "Placeholder")))))

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
    (<- self 'dir #(0 0))))

(defclass entrance (tile)
  ((id :accessor entrance-id :initarg :id :initform "no-id")
   (next-scene :accessor entrance-next-scene)
   (func-load :accessor entrance-func-load)
   (next-player-pos :accessor entrance-next-player-pos :initarg :next-player-pos :initform #(0 0))))

(defmethod object-ready ((self entrance))
  (setf (object-collision-enabled self) t)
  (setf (object-movable self) t))

(defmethod object-update ((self entrance) dt)
  ;force collision check
  (object-move self 0 0 dt))

(defmethod object-on-collide ((self entrance) (obj object))
  (when (string= "player" (object-name obj))
    (switch-scene (entrance-next-scene self))
    (funcall (entrance-func-load self))
    (setf (object-x obj) (aref (entrance-next-player-pos self) 0))
    (setf (object-y obj) (aref (entrance-next-player-pos self) 1))
    ))
