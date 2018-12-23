(require :scarli)

(defpackage scarli-test-3
  (:use :cl :scarli))

(in-package scarli-test-3)

(defparameter *width* 640)
(defparameter *height* 480)

(defparameter *player-movement-script*
  (make-instance 
    'script 
    :ready (lambda (self)
             (object-set self 'dir (vector 0 0))
             (object-set self 'moving_up nil)
             (object-set self 'moving_down nil) 
             (object-set self 'moving_left nil) 
             (object-set self 'moving_right nil)
             (object-set self 'speed 100)
             )
    :input (lambda (self scancode pressed)
             (if pressed
                 (cond
                   ((sdl2:scancode= scancode :scancode-w) (object-set self 'moving_up t))
                   ((sdl2:scancode= scancode :scancode-s) (object-set self 'moving_down t))
                   ((sdl2:scancode= scancode :scancode-a) (object-set self 'moving_left t))
                   ((sdl2:scancode= scancode :scancode-d) (object-set self 'moving_right t)))
                 (cond
                   ((sdl2:scancode= scancode :scancode-w) (progn
                                                            (object-set self 'moving_up nil)
                                                            (drawable-set-frame self 1)
                                                            (drawable-set-anim-index self 0)))
                   ((sdl2:scancode= scancode :scancode-s) (progn
                                                            (object-set self 'moving_down nil)
                                                            (drawable-set-frame self 0)
                                                            (drawable-set-anim-index self 0)))
                   ((sdl2:scancode= scancode :scancode-a) (progn
                                                            (object-set self 'moving_left nil)
                                                            (drawable-set-frame self 1)
                                                            (drawable-set-anim-index self 4)))
                   ((sdl2:scancode= scancode :scancode-d) (progn
                                                            (object-set self 'moving_right nil)
                                                            (drawable-set-frame self 0)
                                                            (drawable-set-anim-index self 2)))
                   )))
    :update (lambda (self dt)
              (object-set self 'dir (vector 0 0))
              (when (object-get self 'moving_down)
                (object-set self 'dir (add-2d-vectors (object-get self 'dir) #(0 1)))
                (drawable-set-anim-index self 1))
              (when (object-get self 'moving_up)
                (object-set self 'dir (add-2d-vectors (object-get self 'dir) #(0 -1)))
                (drawable-set-anim-index self 3))
              (when (object-get self 'moving_left)
                (object-set self 'dir (add-2d-vectors (object-get self 'dir) #(-1 0)))
                (drawable-set-anim-index self 4))
              (when (object-get self 'moving_right)
                (object-set self 'dir (add-2d-vectors (object-get self 'dir) #(1 0)))
                (drawable-set-anim-index self 2))
              (object-set self 'dir (normalize-2d-vector (object-get self 'dir)))
              (when (or (object-get self 'moving_down) (object-get self 'moving_up)
                        (object-get self 'moving_left) (object-get self 'moving_right))
                (drawable-animate self dt))


              (object-move self 
                           (* (object-get self 'speed) (aref (object-get self 'dir) 0))
                           (* (object-get self 'speed) (aref (object-get self 'dir) 1))
                           dt)
              (setf (rect-x (object-collision-rect self)) (object-x self))
              (setf (rect-y (object-collision-rect self)) (+ 24 (object-y self))))))

(defparameter *player-collision-script*
  (make-instance 'script
                 :on-collide (lambda (self collider)
                               (cond
                                 ;Y collision
                                 ;from top
                                 ((> (rect-y (object-collision-rect collider)) (- (+ (rect-h (object-collision-rect self)) (rect-y (object-collision-rect self))) 4))
                                  (progn
                                    (setf (object-y self) (- (object-y collider) (+ (rect-h (object-collision-rect self)) (- (object-height self) (rect-h (object-collision-rect self)) ))))))
                                 ;from bottom
                                 ((< (- (+ (rect-y (object-collision-rect collider)) (rect-h (object-collision-rect collider))) 4) (rect-y (object-collision-rect self)))
                                  (progn
                                    (setf (object-y self) (+ (object-y collider) (rect-h (object-collision-rect self))))))
                                 ;X collision
                                 ;from right
                                 ((< (+ (rect-w (object-collision-rect collider)) (rect-x (object-collision-rect collider))) (+ (rect-x (object-collision-rect self)) (rect-w (object-collision-rect self))))
                                  (progn
                                    (setf (object-x self) (+ (object-x collider) (rect-w (object-collision-rect collider))))
                                    ))
                                 ;from left
                                 ((> (+ (rect-x (object-collision-rect collider)) (rect-w (object-collision-rect collider))) (rect-x (object-collision-rect self)))
                                  (progn
                                    (setf (object-x self) (- (object-x collider) (object-width self)))  
                                    ))
                                 ))))

(defparameter *square*
  (make-instance 'scarli:object
                 :name "square_one"
                 :x (/ *width* 2)
                 :y 400
                 :w 32
                 :h 32
                 :collision-enabled t
                 :collision-rect (make-instance 'scarli:rectangle :x (/ *width* 2) :y 400 :w 32 :h 32)
                 :draw (lambda (self dst_surf)
                         (sdl2:fill-rect 
                           dst_surf 
                           (sdl2:make-rect (scarli:object-x self)
                                           (scarli:object-y self)
                                           (scarli:object-width self)
                                           (scarli:object-height self))
                           (sdl2:map-rgb (sdl2:surface-format dst_surf) #xff #xff #xff)
                           ))))

(defparameter *square2*
  (make-instance 'scarli:object
                 :name "square_two"
                 :x (- (/ *width* 2) 32)
                 :y (- 400 32)
                 :w 32
                 :h 32
                 :collision-enabled t
                 :collision-rect (make-instance 'scarli:rectangle :x (- (/ *width* 2) 32) :y (- 400 32) :w 32 :h 32)
                 :draw (lambda (self dst_surf)
                         (sdl2:fill-rect 
                           dst_surf 
                           (sdl2:make-rect (scarli:object-x self)
                                           (scarli:object-y self)
                                           (scarli:object-width self)
                                           (scarli:object-height self))
                           (sdl2:map-rgb (sdl2:surface-format dst_surf) #xff #xff #xff)
                           ))))

(defparameter *instruction-text*
  (make-instance 'text
                 :text "Move character around with WASD."))
(defparameter *instruction-text-2*
  (make-instance 'text
                 :y 16
                 :text "Touch blocks for collision test."))

(defparameter *testobj*
  (make-instance 'drawable
                 :name "player"
                 :x (/ *width* 2)
                 :y (/ *height* 2)
                 :w 32
                 :h 32
                 :image-path "test_spritesheet.png"
                 :image-rect (make-instance 'rectangle :x 0 :y 0 :w 32 :h 32)
                 :anim-index 0
                 :collision-enabled t
                 :collision-rect (make-instance 'rectangle :x (/ *width* 2) :y (/ *height* 2) :w 32 :h 8)
                 :scripts (list
                            *player-collision-script*
                            *player-movement-script*
                            )))


(defparameter *testscene*
  (make-instance 'scarli:scene
                 :layers (list
                           (make-instance 'scarli:layer :name "bottom")
                           (make-instance 'scarli:layer :name "middle")
                           (make-instance 'scarli:layer :name "top"))))
(defparameter *camera* (make-instance 'camera
                                      :x 0
                                      :y 0
                                      :w *width*
                                      :h *height*))
(add-input-handler *testobj*)
(add-obj-to-scene *testscene* "middle" *testobj*)
(add-obj-to-scene *testscene* "middle" *square*)
(add-obj-to-scene *testscene* "middle" *square2*)
(add-obj-to-scene *testscene* "top" *instruction-text*)
(add-obj-to-scene *testscene* "top" *instruction-text-2*)
(main *testscene* *camera* *width* *height*)
