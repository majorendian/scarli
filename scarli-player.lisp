
;(require :scarli)

(defpackage scarli-player
  (:use :cl :scarli)
  (:export get-default-player
           get-movement-script
           get-collision-script))

(in-package scarli-player)

(defun get-movement-script ()
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
              (setf (rect-y (object-collision-rect self)) (+ 24 (object-y self)))))
  )

(defun get-collision-script ()
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
                                 )))
  )

(defun get-default-player (x y spritesheet_path)
  (make-instance 'drawable
                 :name "player"
                 :x x
                 :y y
                 :w 32
                 :h 32
                 :image-path spritesheet_path
                 :image-rect (make-instance 'rectangle :x 0 :y 0 :w 32 :h 32)
                 :anim-index 0
                 :collision-enabled t
                 :collision-rect (make-instance 'rectangle :x x :y y :w 32 :h 8)
                 :scripts (list
                            (get-collision-script)
                            (get-movement-script)
                            ))
  )
