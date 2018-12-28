
(require :scarli)

(defparameter *width* 320)
(defparameter *height* 240)

(defparameter *testobj*
  (make-instance 'scarli:object
                 :x 0
                 :y (/ height 2)
                 :w 30
                 :h 30
                 :ready (lambda (self)
                         (scarli:object-set self 'dir (vector 1 0)))
                 :update (lambda (self dt)
                           (setf (scarli:object-x self) (round (+ (scarli:object-x self) (* (aref (scarli:object-get self 'dir) 0) (* dt 50) ) )) )
                           (cond 
                             ((> (+ (scarli:object-x self) (scarli:object-width self) ) width) (setf (aref (scarli:object-get self 'dir) 0) -1))
                             ((< (scarli:object-x self) 0) (setf (aref (scarli:object-get self 'dir) 0) 1)))
                           )
                 :draw (lambda (self dst_surf)
                         (let ((testrect (sdl2:make-rect (scarli:object-x self) (scarli:object-y self)
                                                         (scarli:object-width self) (scarli:object-height self))))
                           (sdl2:fill-rect dst_surf testrect (sdl2:map-rgb (sdl2:surface-format dst_surf) #x00 #xff #x00))))))


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

(scarli:add-obj-to-scene *testscene* "middle" *testobj*)
(scarli:main *testscene* *camera* *width* *height*)
