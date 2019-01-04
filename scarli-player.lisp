
;(require :scarli)

(defpackage scarli-player
  (:use :cl :scarli :scarli-objects)
  (:export get-default-player
           get-movement-script
           get-collision-script))

(in-package scarli-player)

(defun get-movement-script ()
  (make-instance 
   'script 
   :ready (lambda (self)
	    (<- self 'dir (vector 0 0))
	    (<- self 'look_dir (vector 0 0))
	    (<- self 'process_input t)
	    (<- self 'double_space_input nil)
	    (<- self 'moving_up nil)
	    (<- self 'moving_down nil) 
	    (<- self 'moving_left nil) 
	    (<- self 'moving_right nil)
	    (<- self 'speed 100)
	    )
   :input (lambda (self scancode pressed)
	    (when (-> self 'process_input) 
	      (if pressed
		  (cond
		    ((sdl2:scancode= scancode :scancode-w) (progn
							     (<- self 'look_dir #(0 -1))
							     (object-set self 'moving_up t)))
		    ((sdl2:scancode= scancode :scancode-s) (progn
							     (<- self 'look_dir #(0 1))
							     (object-set self 'moving_down t)))
		    ((sdl2:scancode= scancode :scancode-a) (progn
							     (<- self 'look_dir #(-1 0))
							     (object-set self 'moving_left t)))
		    ((sdl2:scancode= scancode :scancode-d) (progn
							     (<- self 'look_dir #(1 0))
							     (object-set self 'moving_right t))))
					;key released
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
		    ((sdl2:scancode= scancode :scancode-space)
		     (when (not (-> self 'double_space_input)) 
		       (let* ((coord_x (+ (+ (object-x self) (/ (object-width self) 2)) (* 24 (aref (-> self 'look_dir) 0))))
			      (coord_y (+ (+ (object-y self) (/ (object-height self) 2)) (* 24 (aref (-> self 'look_dir) 1))))
			      (obj (get-obj-at-coord self (get-current-scene) (object-layer self) coord_x coord_y)))
			 (when (subtypep (type-of obj) 'interactible)
			   
			   ;;text box needs to be generated here because we need to make sure
			   ;;that the double input thing doesn't occur and we need the signal
			   ;;from the text box
			   (let ((text_box
				   (make-instance 'paged-text
						  :pages (interactible-pages obj))))
			     (add-obj-to-scene *persistent-scene* "top" text_box)
			     (add-input-handler text_box)
			     (<- self 'process_input nil)
			     (object-add-signal-handler text_box 'scarli:finished-paged-text
							(lambda (tbox)
							  (declare (ignore tbox))
							  (<- self 'process_input t)
							  (<- self 'double_space_input t)
							  ))
			     (interactible-on-interact obj self)
			     )))))
		    ))
	      (<- self 'double_space_input nil))
	    )
   :update (lambda (self dt)
	     (object-set self 'dir (vector 0 0))
	     (when (object-get self 'moving_down)
	       (object-set self 'dir (add-2d-vectors (object-get self 'dir) #(0 1)))
	       (<- self 'look_dir #(0 1))
	       (drawable-set-anim-index self 1))
	     (when (object-get self 'moving_up)
	       (object-set self 'dir (add-2d-vectors (object-get self 'dir) #(0 -1)))
	       (<- self 'look_dir #(0 -1))
	       (drawable-set-anim-index self 3))
	     (when (object-get self 'moving_left)
	       (object-set self 'dir (add-2d-vectors (object-get self 'dir) #(-1 0)))
	       (<- self 'look_dir #(-1 0))
	       (drawable-set-anim-index self 4))
	     (when (object-get self 'moving_right)
	       (object-set self 'dir (add-2d-vectors (object-get self 'dir) #(1 0)))
	       (<- self 'look_dir #(1 0))
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
                 :on-collide
                 (lambda (self collider)
                   ;bottom
                   (when (intersect-side (sdl2:make-rect
                                           (+ (rect-x (object-collision-rect self)) 4) 
                                           (rect-y (object-collision-rect self))
                                           (- (rect-w (object-collision-rect self)) 10) 
                                           2)
                                         (sdl2:make-rect
                                           (object-x collider) (- (+ (object-y collider) (object-height collider)) 4)
                                           (object-width collider) 4))
                     (setf (object-y self) (+ (rect-h (object-collision-rect self)) (object-y collider)))
                    ; (format t "intersect from bottom~%")
                    )
                   ;top
                   (when (intersect-side (sdl2:make-rect
                                           (+ (rect-x (object-collision-rect self)) 4) 
                                           (+ (rect-y (object-collision-rect self)) 4)
                                           (- (rect-w (object-collision-rect self)) 10)
                                           4)
                                         (sdl2:make-rect
                                           (object-x collider) (object-y collider)
                                           (object-width collider) 4))
                     (setf (object-y self) (- (object-y collider) (object-height collider)))
                    ; (format t "intersect from top~%")
                    )
                   ;left
                   (when (intersect-side (sdl2:make-rect
                                           (rect-x (object-collision-rect self)) 
                                           (+ (rect-y (object-collision-rect self)) 2)
                                           2 4)
                                         (sdl2:make-rect
                                           (- (+ (object-width collider) (object-x collider)) 4)
                                           (object-y collider)
                                           4 (object-height self)))
                     (setf (object-x self) (+ (object-width collider) (object-x collider)))
                    ; (format t "intersect from left~%")
                    )
                   ;right
                   (when (intersect-side (sdl2:make-rect
                                           (+ (rect-x (object-collision-rect self)) (- (rect-w (object-collision-rect self)) 4))
                                           (+ (rect-y (object-collision-rect self)) 2)
                                           2 4)
                                         (sdl2:make-rect
                                           (object-x collider) (object-y collider)
                                           4 (object-height collider)))
                     (setf (object-x self) (+ 2 (- (object-x collider) (object-width collider))))
                     ;(format t "intersect from right~%")
                     )
                   
                               ))
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
                 :movable t
                 :collision-rect (make-instance 'rectangle :x x :y y :w 32 :h 8)
                 :scripts (list
                            (get-collision-script)
                            (get-movement-script)
                            ))
  )
