
(require :scarli)
;;quickload :split-sequence)
(require :cl-strings)
(require :split-sequence)

(defpackage scarli-editor
  (:use cl :scarli :scarli-objects)
  (:export highlight-square
           layer-indicator))

(in-package :scarli-editor)
(defparameter *width* 640)
(defparameter *height* 480)
(defparameter *scene* (make-instance 'scene
                                     :layers (list
                                               (make-instance 'layer :name "bottom")
                                               (make-instance 'layer :name "middle")
                                               (make-instance 'layer :name "top")
                                               )))

(defparameter *current-layer* (layer-name (nth 0 (scene-layers *scene*))))
(defparameter *current-tile-class* 'tile)
(defparameter *available-tile-classes* 
  (list 'tile 'solid-tile 'interactible 'pushable-block 'entrance 'npc))

(defun get-save-tile-format (tile)
  (cond
    ((subtypep (type-of tile) 'npc) `(make-instance ',(type-of tile)
						    :image-path ,(drawable-image-path tile)
						    :x ,(object-x tile)
						    :y ,(object-y tile)
						    :w ,(object-width tile)
						    :h ,(object-height tile)
						    :image-rect (make-instance 'rectangle
									       :x ,(rect-x (drawable-image-rect tile))
									       :y ,(rect-y (drawable-image-rect tile))
									       :w ,(rect-w (drawable-image-rect tile))
									       :h ,(rect-h (drawable-image-rect tile))
									       )
						    :collision-rect (make-instance 'rectangle
										   :x ,(rect-x (object-collision-rect tile))
										   :y ,(rect-y (object-collision-rect tile))
										   :w ,(rect-w (object-collision-rect tile))
										   :h ,(rect-h (object-collision-rect tile))
										   )
						    :layer ,(object-layer tile)
						    
						    
						    
						    :pages ',(interactible-pages tile)
						    ))
    ((subtypep (type-of tile) 'entrance) `(make-instance ',(type-of tile)
							 :image-path "tile_sheet.png"
							 :x ,(object-x tile)
							 :y ,(object-y tile)
							 :w ,(object-width tile)
							 :h ,(object-height tile)
							 :image-rect (make-instance 'rectangle
										    :x ,(rect-x (drawable-image-rect tile))
										    :y ,(rect-y (drawable-image-rect tile))
										    :w ,(rect-w (drawable-image-rect tile))
										    :h ,(rect-h (drawable-image-rect tile))
										    )
							 :collision-rect (make-instance 'rectangle
											:x ,(rect-x (object-collision-rect tile))
											:y ,(rect-y (object-collision-rect tile))
											:w ,(rect-w (object-collision-rect tile))
											:h ,(rect-h (object-collision-rect tile))
											)
							 :layer ,(object-layer tile)
							 :name ,(object-name tile)
							 :connected-door-id ,(entrance-connected-door-id tile)
							 :next-player-pos ,(entrance-next-player-pos tile)
							 :next-level ,(entrance-next-level tile)
							 ))
    ((subtypep (type-of tile) 'interactible) `(make-instance ',(type-of tile)
                                                             :image-path "tile_sheet.png"
                                                             :x ,(object-x tile)
                                                             :y ,(object-y tile)
                                                             :w ,(object-width tile)
                                                             :h ,(object-height tile)
                                                             :image-rect (make-instance 'rectangle
                                                                                        :x ,(rect-x (drawable-image-rect tile))
                                                                                        :y ,(rect-y (drawable-image-rect tile))
                                                                                        :w ,(rect-w (drawable-image-rect tile))
                                                                                        :h ,(rect-h (drawable-image-rect tile))
                                                                                        )
                                                             :collision-rect (make-instance 'rectangle
                                                                                            :x ,(rect-x (object-collision-rect tile))
                                                                                            :y ,(rect-y (object-collision-rect tile))
                                                                                            :w ,(rect-w (object-collision-rect tile))
                                                                                            :h ,(rect-h (object-collision-rect tile))
                                                                                            )
                                                             :layer ,(object-layer tile)
                                                             :pages ',(interactible-pages tile)))
    ((subtypep (type-of tile) 'tile) `(make-instance ',(type-of tile)
                                                     :image-path "tile_sheet.png"
                                                     :x ,(object-x tile)
                                                     :y ,(object-y tile)
                                                     :w ,(object-width tile)
                                                     :h ,(object-height tile)
                                                     :image-rect (make-instance 'rectangle
                                                                                :x ,(rect-x (drawable-image-rect tile))
                                                                                :y ,(rect-y (drawable-image-rect tile))
                                                                                :w ,(rect-w (drawable-image-rect tile))
                                                                                :h ,(rect-h (drawable-image-rect tile))
                                                                                )
                                                     :collision-rect (make-instance 'rectangle
                                                                                    :x ,(rect-x (object-collision-rect tile))
                                                                                    :y ,(rect-y (object-collision-rect tile))
                                                                                    :w ,(rect-w (object-collision-rect tile))
                                                                                    :h ,(rect-h (object-collision-rect tile))
                                                                                    )
                                                     :layer ,(object-layer tile)))))

(defun save-tiles (output_filename)
  (let ((r_list (list)))
    (loop for l in (scene-layers *scene*)
          do (loop for obj in (layer-objects l)
                   do (when (subtypep (type-of obj) 'tile)
                        (push (get-save-tile-format obj) r_list))))
    (with-open-file (str output_filename
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format str "(list~%")
      (loop for obj in r_list
            do (progn
                 (write obj :stream str)
                 (terpri str)))
      (format str ")"))))



(defclass highlight-square (object)
  ())

(defmethod object-ready ((self highlight-square))
  (<- self 'modifying_tile nil)
  (<- self 'color_r #x00))

(defmethod object-draw ((self highlight-square) dst_surf)
  (let ((top_line (sdl2:make-rect (object-x self) (object-y self) (object-width self) 2))
        (left_line (sdl2:make-rect (object-x self) (object-y self) 2 (object-height self)))
        (bottom_line (sdl2:make-rect (object-x self) (- (+ (object-height self) (object-y self))2) (object-width self) 2))
        (right_line (sdl2:make-rect (- (+ (object-width self) (object-x self))2) (object-y self) 2 (object-height self))))
    (sdl2:fill-rect dst_surf top_line (sdl2:map-rgb (sdl2:surface-format dst_surf) (-> self 'color_r) #xff #x00))
    (sdl2:fill-rect dst_surf left_line (sdl2:map-rgb (sdl2:surface-format dst_surf) (-> self 'color_r) #xff #x00))
    (sdl2:fill-rect dst_surf bottom_line (sdl2:map-rgb (sdl2:surface-format dst_surf) (-> self 'color_r) #xff #x00))
    (sdl2:fill-rect dst_surf right_line (sdl2:map-rgb (sdl2:surface-format dst_surf) (-> self 'color_r) #xff #x00))
    ))

(defun join-semicolon-list (l &optional (delim ";"))
  (let ((r ""))
    (loop with i = 0 for e in l
         do (progn
              (if (= 0 i)
                (setf r e)
                (setf r (concatenate 'string r delim e)))
              (setf i (+ 1 i))))
    r))

(defun join-lol (lol)
  (let ((r ""))
    (loop with i = 0 for l in lol
	  do (progn
	       (if (= 0 i)
		   (setf r (join-semicolon-list l (format nil "~%")))
		   (when (not (string= (format nil "~%") (join-semicolon-list l (format nil "~%"))))
		     (setf r (concatenate 'string r (format nil ";~%") (join-semicolon-list l (format nil "~%"))))))
	       (setf i (+ 1 i))))
    r))


(defun split-into-lol (multiline_text)
  (let* ((r (list))
	 (tmp_list (cl-strings:split multiline_text ";")))
    (format t "our tmp list:~S~%" tmp_list)
    (loop for multi_string in (reverse tmp_list)
	  do (let ((tmp_list_2 (remove "" (cl-strings:split multi_string "\\n") :test 'equal)))
	       (when tmp_list_2
		 (push tmp_list_2 r))))
    r))

(defmethod object-input ((self highlight-square) scancode pressed)
  (if (not (object-get self 'modifying_tile)) 
      (when (not pressed)
        (cond
          ((sdl2:scancode= scancode :scancode-up) (setf (object-y self) (- (object-y self) (object-height self))))
          ((sdl2:scancode= scancode :scancode-down) (setf (object-y self) (+ (object-y self) (object-height self))))
          ((sdl2:scancode= scancode :scancode-left) (setf (object-x self) (- (object-x self) (object-width self))))
          ((sdl2:scancode= scancode :scancode-right) (setf (object-x self) (+ (object-x self) (object-width self))))
          ((sdl2:scancode= scancode :scancode-space)
           (let ((tile (get-obj-at-pos-in-layer (object-scene self) *current-layer* (object-x self) (object-y self))))
             (if tile
                 (progn
                   (format t "found tile:~S~%" tile)
                   (<- self 'color_r #xff)
                   (<- self 'modifying_tile tile))
                 (progn
                   (format t "creating tile ~S~%" *current-tile-class*)
                   (setf tile (create-tile :tile-sheet-path "tile_sheet.png"
                                           :tile-size 32
                                           :tile-class *current-tile-class*
                                           :x (object-x self)
                                           :y (object-y self)))
                   (add-obj-to-scene (object-scene self) *current-layer* tile)))
             ))
          ((sdl2:scancode= scancode :scancode-delete)
           (let ((tile (get-obj-at-pos-in-layer (object-scene self) *current-layer* (object-x self) (object-y self))))
             (when tile
               (object-remove tile))))
          ))
      (progn
					;modifying tile
        (when (not pressed)
          (cond
            ((sdl2:scancode= scancode :scancode-up) 
             (when (> (rect-y (drawable-image-rect (-> self 'modifying_tile))) 0)
               (setf (rect-y (drawable-image-rect (-> self 'modifying_tile)))
		     (- (rect-y (drawable-image-rect (-> self 'modifying_tile))) (rect-h (drawable-image-rect (-> self 'modifying_tile)))))))
            ((sdl2:scancode= scancode :scancode-down) 
             (when (< (+ (rect-y (drawable-image-rect (-> self 'modifying_tile))) (rect-h (drawable-image-rect (-> self 'modifying_tile))))  (sdl2:surface-height (drawable-image (-> self 'modifying_tile))))
               (setf (rect-y (drawable-image-rect (-> self 'modifying_tile)))
		     (+ (rect-y (drawable-image-rect (-> self 'modifying_tile))) (rect-h (drawable-image-rect (-> self 'modifying_tile)))))))
            ((sdl2:scancode= scancode :scancode-left) 
             (when (> (rect-x (drawable-image-rect (-> self 'modifying_tile))) 0)
               (setf (rect-x (drawable-image-rect (-> self 'modifying_tile)))
		     (- (rect-x (drawable-image-rect (-> self 'modifying_tile))) (rect-w (drawable-image-rect (-> self 'modifying_tile)))))))
            ((sdl2:scancode= scancode :scancode-right) 
             (when (< (+ (rect-x (drawable-image-rect (-> self 'modifying_tile))) (rect-w (drawable-image-rect (-> self 'modifying_tile)))) (sdl2:surface-width (drawable-image (-> self 'modifying_tile))))
               (setf (rect-x (drawable-image-rect (-> self 'modifying_tile)))
		     (+ (rect-x (drawable-image-rect (-> self 'modifying_tile))) (rect-w (drawable-image-rect (-> self 'modifying_tile)))))))
            ((sdl2:scancode= scancode :scancode-space) (progn
                                                         (<- self 'color_r #x00)
                                                         (<- self 'modifying_tile nil)))
            ((sdl2:scancode= scancode :scancode-e)
             (progn
               (format t "type of tile~S~%" (-> self 'modifying_tile))
               #+:linux (progn
                          (cond
			    ;;editing of npc (must come before 'interactible because it extends it)
			    ((subtypep (type-of (-> self 'modifying_tile)) 'npc)
			     (let ((yad_out (make-string-output-stream)))
			       (sb-ext:run-program "/usr/bin/yad"
						   (list "--form"
							 "--field=Spritesheet:CBE"
							 "--field=Text:TXT"
							 (drawable-image-path (-> self 'modifying_tile))
							 (join-lol (interactible-pages (-> self 'modifying_tile))))
						   :output yad_out :error nil)
			       (let* ((raw_output (get-output-stream-string yad_out))
				      (result_list (split-sequence:split-sequence-if (lambda (item) (position item "|"))
										     (string-trim '(#\Newline) raw_output)) ))
				 (format t "raw_output:~S~%" raw_output)
				 (when (> (length result_list) 1)
				   (setf (drawable-image-path (-> self 'modifying_tile)) (nth 0 result_list))
				   (setf (interactible-pages (-> self 'modifying_tile)) (split-into-lol (nth 1 result_list)))))))
			    ;;editing of interactible
                            ((subtypep (type-of (-> self 'modifying_tile)) 'interactible)
                             (let ((zenity_out (make-string-output-stream))) 
                               (sb-ext:run-program "/usr/bin/zenity" (list "--entry" "--width" "640"
                                                                           (concatenate 'string "--entry-text=" (join-semicolon-list (nth 0 (interactible-pages (-> self 'modifying_tile))))))
                                                   :output zenity_out :error nil)
                               (let ((result_list
                                       (list
					(split-sequence:split-sequence-if (lambda (item) (position item ";")) 
									  (string-trim '(#\Newline) (get-output-stream-string zenity_out))))))
                                 (if (> (length (nth 0 (nth 0 result_list))) 0)
                                     (setf (interactible-pages (-> self 'modifying_tile)) result_list)
                                     (format t "text modification canceled~%")))))
			    ;;modifying of entrance
                            ((subtypep (type-of (-> self 'modifying_tile)) 'entrance)
                             (let ((yad_out (make-string-output-stream)))
                               (sb-ext:run-program "/usr/bin/yad"
                                                   (list "--form"
                                                         "--field=ID:CBE"  
                                                         "--field=Connected Door ID:CBE" 
                                                         "--field=Player X:CBE"
                                                         "--field=Player Y:CBE"
							 "--field=Level:CBE"
                                                         (object-name (-> self 'modifying_tile))
                                                         (entrance-connected-door-id (-> self 'modifying_tile))
                                                         (write-to-string (aref (entrance-next-player-pos (-> self 'modifying_tile)) 0))
                                                         (write-to-string (aref (entrance-next-player-pos (-> self 'modifying_tile)) 1))
							 (entrance-next-level (-> self 'modifying_tile))
							 )
                                                   :output yad_out :error nil)
                               (let ((result_list
                                       (split-sequence:split-sequence-if (lambda (item) (position item "|"))
                                                                         (string-trim '(#\Newline) (get-output-stream-string yad_out)))))
                                 (when (> (length result_list) 4)
                                   (setf (object-name (-> self 'modifying_tile)) (nth 0 result_list))
                                   (setf (entrance-connected-door-id (-> self 'modifying_tile)) (nth 1 result_list))
                                   (setf (entrance-next-player-pos (-> self 'modifying_tile)) (vector (parse-integer (nth 2 result_list))
                                                                                                      (parse-integer (nth 3 result_list))))
				   (setf (entrance-next-level (-> self 'modifying_tile)) (nth 4 result_list))))))
                            
			    )
			  
			  )))

            ))
        )))



(defclass disappearing-text (text)
  ())

(defmethod object-ready ((self disappearing-text))
  (<- self 'accum_delta 0))

(defmethod object-update ((self disappearing-text) dt)
  (<- self 'accum_delta (+ dt (-> self 'accum_delta)))
  (when (> (-> self 'accum_delta) 2)
    (object-remove self)))

(defclass layer-indicator (text)
  ())

(defmethod object-ready ((self layer-indicator))
  (format t "layer indicator ready~%")
  (<- self 'layer_index 0)
  )

(defmethod object-update ((self layer-indicator) dt)
  (declare (ignore dt))
  (setf (text-text self) (concatenate 'string "Current layer: " *current-layer*)))

(defmethod object-input ((self layer-indicator) scancode pressed)
  (when (not pressed)
    (cond
      ((sdl2:scancode= scancode :scancode-pageup)
       (when (> (-> self 'layer_index) 0)
         (<- self 'layer_index (- (-> self 'layer_index) 1))
         (setf *current-layer* (layer-name (nth (-> self 'layer_index) (scene-layers *scene*))))
         ))
      ((sdl2:scancode= scancode :scancode-pagedown)
       (when (< (-> self 'layer_index) (- (length (scene-layers *scene*)) 1))
         (<- self 'layer_index (+ 1 (-> self 'layer_index)))
         (setf *current-layer* (layer-name (nth (-> self 'layer_index) (scene-layers *scene*))))
         ))
      ((sdl2:scancode= scancode :scancode-s)
       (save-tiles "editor-output.map")
       (add-obj-to-scene (object-scene self) (object-layer self) (make-instance 'disappearing-text
                                                                                :x 0
                                                                                :y 400
                                                                                :text "Tilemap saved"))
       )
      ((sdl2:scancode= scancode :scancode-l) (progn
                                               (delete-all-tiles-from-scene *scene*)
                                               (display-tiles *scene* "editor-output.map")
                                               (add-obj-to-scene (object-scene self) (object-layer self)
                                                                 (make-instance 'disappearing-text
                                                                                :x 0
                                                                                :y 400
                                                                                :text "Tilemap loaded"))))
      
      )
    ))

(defclass tile-class-indicator (text)
  ())

(defmethod object-ready ((self tile-class-indicator))
  (<- self 'tile_class_index 0))

(defmethod object-update ((self tile-class-indicator) dt)
  (setf (text-text self) (concatenate 'string "Current tile class: " (symbol-name *current-tile-class*))))

(defmethod object-input ((self tile-class-indicator) scancode pressed)
  (when (not pressed)
    (cond
      ((sdl2:scancode= scancode :scancode-t) (progn
                                               (if (< (-> self 'tile_class_index) (- (length *available-tile-classes*) 1))
                                                   (<- self 'tile_class_index (+ 1 (-> self 'tile_class_index)))
                                                   (<- self 'tile_class_index 0))
                                               (setf *current-tile-class* (nth (-> self 'tile_class_index) *available-tile-classes*))))
      ))
  )


(defparameter *user-cursor* (make-instance 'highlight-square :x 0 :y 0 :w 32 :h 32))
(defparameter *camera* (make-instance 'camera :x 0 :y 0 :w *width* :h *height* :parent *user-cursor*))
(defparameter *layer-indicator* (make-instance 'layer-indicator :text "Placeholder"))
(defparameter *tile-class-indicator* (make-instance 'tile-class-indicator 
                                                    :y 16
                                                    :text "Placeholder"))

(add-obj-to-scene *scene* "top" *user-cursor*)
(add-obj-to-scene *persistent-scene* "top" *layer-indicator*)
(add-obj-to-scene *persistent-scene* "top" *tile-class-indicator*)
(clear-input-handlers)
(add-input-handler *user-cursor*)
(add-input-handler *layer-indicator*)
(add-input-handler *tile-class-indicator*)
(main "Scarli Level Editor" *scene* *camera* *width* *height*)
