
(require :scarli)

(defpackage :prime-garden
  (:use :cl :scarli :scarli-objects :scarli-player))

(in-package :prime-garden)

					;======= Intro scene ======
(defparameter *intro-scene* (make-default-scene))
(defparameter *width* 640) (defparameter *height* 480)
(defparameter *camera* (make-instance 'camera :x 0 :y 0 :w *width* :h *height*))
;;intro image
(defparameter *intro-image* (make-instance 'drawable
					   :image-path "intro_image.png"
					   :image-rect (make-instance 'rectangle :x 0 :y 0 :w *width* :h *height*)))

;;instruction text, tell the player how to play the game
(defparameter *instruction-text* (make-instance 'multiline-text
						:y (/ *height* 2)
						:x 128
						:lines '("INSTRUCTIONS"
							 "WASD to move around"
							 "SPACE to confirm and interact"
							 "Press space to continue")
						:input (lambda (self scancode pressed)
							 (when (and (not pressed) (sdl2:scancode= scancode :scancode-space))
							   (if (not (-> self 'double_input))
							       (progn
								 (switch-scene "level_4.map" (lambda () (play-music "cool_nescaline.mp3")))
								 (remove-input-handler self))
							       (<- self 'double_input nil))))
						:ready (lambda (self)
							 (<- self 'double_input t))))

(defparameter *instruction-scene* (make-default-scene))
(add-obj-to-scene *instruction-scene* "middle" *instruction-text*)
(add-input-handler *instruction-text*)

;;intro text
(defparameter *intro-text* (make-instance 'text
					  :text "<Press space to start the game>"
					  :x 128
					  :y (/ *height* 2)
					  :ready (lambda (self)
						   (add-input-handler self))
					  :input (lambda (self scancode pressed)
						   (when (not pressed)
						     (when (sdl2:scancode= scancode :scancode-space)
						       (goto-scene *instruction-scene*)
						       (remove-input-handler self))))))

(add-obj-to-scene *intro-scene* "bottom" *intro-image*)
(add-obj-to-scene *intro-scene* "middle" *intro-text*)
					;============ Setup player ============
(defparameter *player* (get-default-player 64 64 "player_spritesheet.png"))
(add-input-handler *player*)
;;macro defining levels
(defmacro define-level (sym level)
  `(progn
     (defparameter ,sym (make-default-scene) )
     (add-obj-to-scene ,sym "middle" *player*)
     (register-scene ,sym ,level)))

(define-level *level_1_scene* "level_1.map")
(define-level *level_2_scene* "level_2.map")
(define-level *level_3_scene* "level_3.map")
(define-level *level_4_scene* "level_4.map")
;;=========== level 4 custom interactions =============
(setf (scene-on-load *level_4_scene*)
      (lambda (sc)
	(let ((y_npc (find-object sc "y_in_garden"))
	      (flower (find-object sc "nice_smelling_flower")))
	  (setf (interactible-on-interact-script flower)
		(lambda (self obj)
		  (declare (ignore obj) (ignore self))
		  (setf (interactible-pages y_npc)
			(list
			 (list "What did I tell you? Wonderfull scent they have.")))))
	  )))

(main "Prime Garden" *intro-scene* *camera* *width* *height*)
