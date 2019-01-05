
(require :scarli)
(require :sb-sprof)

(defpackage :prime-garden
  (:use :cl :scarli :scarli-objects :scarli-player)
  (:export run-game))

(in-package :prime-garden)

					;======= Intro scene ======
(defparameter *intro-scene* (make-default-scene))
(defparameter *width* 640) (defparameter *height* 480)
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
								 (switch-scene "level_1.map" (lambda () (play-music "cool_nescaline.mp3") ))
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
(defparameter *camera* (make-instance 'camera :x 0 :y 0 :w *width* :h *height* :parent *player*))
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
(define-level *level_5_scene* "level_5.map")
;;============ level 5 custom ===============
(defun reset-button-func (sc)
  (let ((reset_button (find-object sc "reset_button")))
    (setf (interactible-on-interact-script reset_button)
	  (lambda (self obj)
	    (declare (ignore self) (ignore obj))
	    (reload-scene "level_5.map" (list *player* reset_button))))) )
(setf (scene-on-load *level_5_scene*) #'reset-button-func)
;;============ level 6 =======================
(define-level *level_6_scene* "level_6.map")
(define-level *level_7_scene* "level_7.map")
;;============ level 7 =======================
(setf (scene-width *level_7_scene*) 1024)
(setf (scene-height *level_7_scene*) 768)
(setf (scene-on-load *level_7_scene*)
      (lambda (sc)
	(let ((hidden_switch (find-object sc "hidden_flower_switch")))
	  (setf (interactible-on-interact-script hidden_switch)
		(lambda (self obj)
		  (let ((hidden_stairs (make-tile :tile-sheet-path "tile_sheet.png"
						  :tile-size 32
						  :tile-class 'entrance
						  :x (* 20 32)
						  :y (* 10 32)
						  :ri 0
						  :ci 2)))
		    ;;set hidden_stairs parameters
		    (setf (object-name hidden_stairs) "floor_7_2")
		    (setf (entrance-next-level hidden_stairs) "level_8.map")
		    (setf (entrance-next-player-pos hidden_stairs) #(0 32))
		    (setf (entrance-connected-door-id hidden_stairs) "floor_8_1")
		    ;;add the stairs to scene afterwards
		    (add-obj-to-scene sc "middle" hidden_stairs)))))))
;;(sb-sprof:start-profiling :mode :alloc)
(main "Prime Garden" *intro-scene* *camera* *width* *height*)
;;(sb-sprof:stop-profiling)
;;(sb-sprof:report :type :flat)

