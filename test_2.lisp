(require :scarli)

(defparameter *width* 640)
(defparameter *height* 480)

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

(defparameter *square_two*
  (make-instance 'scarli:object
                 :name "square_two"
                 :x (/ *width* 2)
                 :y 0
                 :w 32
                 :h 32
                 :collision-enabled t
                 :collision-rect (make-instance 'scarli:rectangle :x (/ *width* 2) :y 0 :w 32 :h 32)
                 :draw (lambda (self dst_surf)
                         (sdl2:fill-rect 
                           dst_surf 
                           (sdl2:make-rect (scarli:object-x self)
                                           (scarli:object-y self)
                                           (scarli:object-width self)
                                           (scarli:object-height self))
                           (sdl2:map-rgb (sdl2:surface-format dst_surf) #xff #xff #xff)
                           ))))

(defparameter *testobj*
  (make-instance 'scarli:drawable
                 :x (/ *width* 2)
                 :y 100
                 :w 32
                 :h 32
                 :anim-index 1
                 :image-rect (make-instance 'scarli:rectangle :x 0 :y 0 :w 32 :h 32)
                 :image-path "test_spritesheet.png"
                 :collision-enabled t
                 :collision-rect (make-instance 'scarli:rectangle :x 0 :y 0 :w 32 :h 8)
                 :input (lambda (self scancode pressed)
                          (when (sdl2:scancode= scancode :scancode-escape)
                            (when (not pressed)
                              (sdl2:push-quit-event))))
                 :ready (lambda (self)
                         (scarli:object-set self 'dir (vector 0 1)))
                 :on-collide (lambda (self collider)
                               (when (string= (scarli:object-name collider) "square_one")
                                 (setf (aref (scarli:object-get self 'dir) 1) (* -1 (aref (scarli:object-get self 'dir) 1)))
                                 (scarli:drawable-set-anim-index self 3))
                               (when (string= (scarli:object-name collider) "square_two")
                                 (setf (aref (scarli:object-get self 'dir) 1) (* -1 (aref (scarli:object-get self 'dir) 1)))
                                 (scarli:drawable-set-anim-index self 1) 
                                 ))
                 :update (lambda (self dt)
                           (setf (scarli:object-y self) (round (+ (scarli:object-y self) (* (aref (scarli:object-get self 'dir) 1) (* dt 50) ) )) )
                           ;move collision rect along with object with offset
                           (setf (scarli:rect-x (scarli:object-collision-rect self)) (scarli:object-x self))
                           (setf (scarli:rect-y (scarli:object-collision-rect self)) (+ 24 (scarli:object-y self)))
                           (scarli:drawable-animate self dt)
                           (cond 
                             ((> (+ (scarli:object-y self) (scarli:object-height self) ) *height*)
                              (progn
                                (setf (aref (scarli:object-get self 'dir) 0) -1)
                                (scarli:drawable-set-anim-index self 3)))
                             ((< (scarli:object-y self) 0)
                              (progn
                                (setf (aref (scarli:object-get self 'dir) 0) 1)
                                (scarli:drawable-set-anim-index self 1))))
                           )))




(defparameter *testscene*
  (make-instance 'scarli:scene
                 :layers (list
                           (make-instance 'scarli:layer :name "bottom")
                           (make-instance 'scarli:layer :name "middle")
                           (make-instance 'scarli:layer :name "top"))))

(scarli:add-input-handler *testobj*)
(scarli:add-obj-to-scene *testscene* "middle" *testobj*)
(scarli:add-obj-to-scene *testscene* "middle" *square*)
(scarli:add-obj-to-scene *testscene* "middle" *square_two*)
(scarli:main *testscene* *width* *height*)
