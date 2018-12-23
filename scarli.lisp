(require :sdl2)
(require :sdl2-ttf)
(require :sdl2-mixer)
(require :sdl2-image)

(defpackage scarli
  (:use :cl)
  (:export scene
           scene-layers
           scene-name
           layer
           layer-name
           layer-objects
           object
           object-name
           object-x
           object-y
           object-z-index
           object-width
           object-height
           object-update
           object-draw
           object-set
           object-get
           object-collision-rect
           object-collision-enabled
           object-collide
           object-is-colliding
           object-move
           object-remove
           object-scene
           text
           progressive-text
           script
           script-update
           script-input
           script-draw
           script-name
           script-ready
           drawable
           drawable-advance-frame
           drawable-animate
           drawable-anim-index
           drawable-set-frame
           drawable-set-anim-index
           drawable-get-frame
           make-tiles
           map-from-size
           map-set-tile
           rectangle
           rect-x
           rect-y
           rect-w
           rect-h
           add-2d-vectors
           sub-2d-vectors
           normalize-2d-vector
           camera
           camera-x
           camera-y
           camera-w
           camera-h
           camera-surface
           camera-parent
           camera-main-surface
           add-obj-to-scene
           add-input-handler
           *persistent-scene*
           main
           ))

(in-package :scarli)


(defparameter *default-font-path* "kongtext.ttf")
(defparameter *default-font* nil)
(defparameter *MAX_FPS* 30)

;list of objects that process input
(defparameter *input-handlers* (list))

;=======================
; Layer and scene class
;=======================

(defclass scene ()
  ((name :accessor scene-name :initarg :name)
   (layers :accessor scene-layers :initarg :layers :initform (list))))



(defclass layer ()
  ((name :accessor layer-name :initarg :name)
   (objects :accessor layer-objects :initarg :objects :initform (list))))

;list of objects that always process regardless of scene
(defparameter *persistent-scene*
  (make-instance 'scene
                 :layers (list
                           (make-instance 'layer :name "bottom")
                           (make-instance 'layer :name "middle")
                           (make-instance 'layer :name "top"))))

;============
; Rectangle
;============

(defclass rectangle ()
  ((x :accessor rect-x :initarg :x :initform 0)
   (y :accessor rect-y :initarg :y :initform 0)
   (w :accessor rect-w :initarg :w :initform 0)
   (h :accessor rect-h :initarg :h :initform 0)))

(defmethod rect-to-sdl2-rect ((rect rectangle))
  (sdl2:make-rect (rect-x rect) (rect-y rect) (rect-w rect) (rect-h rect)))

;================
; Script class
;================

(defclass script ()
  ((name :accessor script-name :initarg :name :initform "DEFAULT_SCRIPT_NAME")
   (ready :accessor script-ready :initarg :ready :initform (lambda (self)
                                                             (declare (ignore self))))
   (update :accessor script-update :initarg :update :initform (lambda (self dt)
                                                                (declare (ignore self) (ignore dt))))
   (input :accessor script-input :initarg :input :initform (lambda (self scancode pressed)
                                                             (declare (ignore self) (ignore scancode) (ignore pressed))))
   (draw :accessor script-draw :initarg :draw :initform (lambda (self dst_surf)
                                                          (declare (ignore self) (ignore dst_surf))))
   (on-collide :accessor script-on-collide :initarg :on-collide :initform (lambda (self collider)
                                                                         (declare (ignore self) (ignore collider))))))
;==================
; Object class
;==================

(defclass object ()
  ((name :accessor object-name :initarg :name :initform "DEFAULT_OBJECT_NAME")
   (x :accessor object-x :initarg :x :initform 0)
   (y :accessor object-y :initarg :y :initform 0)
   (width :accessor object-width :initarg :w :initform 0)
   (height :accessor object-height :initarg :h :initform 0)
   (scene :accessor object-scene :initform nil)
   (collision-rect :accessor object-collision-rect :initarg :collision-rect :initform (make-instance 'rectangle))
   (collision-enabled :accessor object-collision-enabled :initarg :collision-enabled :initform nil)
   (on-collide :accessor object-custom-on-collide :initarg :on-collide :initform (lambda (self collider)
                                                                            (declare (ignore self) (ignore collider))))
   (is-colliding :accessor object-is-colliding :initform nil)
   (z-index :accessor object-z-index :initarg :z-index :initform 0)
   (attributes :accessor object-attributes :initform (make-hash-table))
   (scripts :accessor object-scripts :initarg :scripts :initform (list))
   (ready :accessor object-custom-ready :initarg :ready :initform nil)
   (update :accessor object-custom-update :initarg :update :initform nil)
   (draw :accessor object-custom-draw :initarg :draw :initform nil)
   (input :accessor object-custom-input :initarg :input :initform nil)))

(defgeneric object-init (obj)
  (:documentation "Initializes object before it is added to the scene"))

(defgeneric object-draw (obj dst_surf)
  (:documentation "Draws OBJ onto DST_SURF which is an SDL2 surface object"))

(defgeneric object-update (obj dt)
  (:documentation "Updates OBJ and passes delta time DT as argument"))

(defgeneric object-ready (obj)
  (:documentation "Prepares the object after SDL initialization"))

(defgeneric object-input (obj scancode pressed)
  (:documentation "Fires when added into *input-handlers* this function processes key presses"))

(defgeneric object-on-collide (self obj_b)
  (:documentation "Fires when 2 objects have an intersecting collision rectangle"))

(defmethod object-init ((obj object)))

(defmethod object-update ((self object) (dt float)))

(defmethod object-ready ((self object)))

(defmethod object-input ((self object) scancode pressed))

(defmethod object-on-collide ((self object) (obj_b object)))

(defmethod object-set ((obj object) (sym symbol) (val t))
  (setf (gethash sym (object-attributes obj)) val))

(defmethod object-get ((obj object) (sym symbol))
  (gethash sym (object-attributes obj)))

(defmethod object-move ((obj object) (x number) (y number) (dt float))
  (setf (object-x obj) (round (+ (* x dt) (object-x obj))))
  (setf (object-y obj) (round (+ (* y dt) (object-y obj)))))

(defmethod object-collide ((obj object) (obj_2 object))
  (when (and (object-collision-enabled obj) (object-collision-enabled obj_2))
    (let ((o1_rect (rect-to-sdl2-rect (object-collision-rect obj)))
          (o2_rect (rect-to-sdl2-rect (object-collision-rect obj_2))))
      (let ((is_collision (sdl2:has-intersect o1_rect o2_rect)))
        (setf (object-is-colliding obj) is_collision)
        (setf (object-is-colliding obj_2) is_collision)
        is_collision))))

(defmethod object-remove ((obj object))
  (loop for a_layer in (scene-layers (object-scene obj))
        do (loop for obj_to_remove in (layer-objects a_layer)
                 when (eq obj obj_to_remove)
                 do (progn
                      (format t "removing object ~S~%" obj)
                      (setf (layer-objects a_layer) (remove obj (layer-objects a_layer)))
                      (return)))))

;=================
; Text class
;=================
(defclass text (object)
  ((text :accessor text-text :initarg :text :initform "PLACEHOLDER_TEXT")))

(defmethod object-draw ((obj text) dst_surf)
  (when (> (length (text-text obj)) 0) 
    (let ((text_surf (sdl2-ttf:render-text-solid *default-font*
                                                 (text-text obj)
                                                 255 255 255 0)))
      (sdl2:blit-surface text_surf nil
                         dst_surf
                         (sdl2:make-rect (object-x obj) (object-y obj)
                                         (sdl2:surface-width text_surf) (sdl2:surface-height text_surf)))
      )))

(defclass progressive-text (text)
  ((text-to-render :accessor text-text-to-render :initarg :text :initform "PLACEHOLDER_TEXT")
   (current-text :accessor text-text :initform "")
   ))

(defmethod object-init ((self progressive-text))
  (object-set self 'txt_index 1)
  (object-set self 'accum_delta 0))

(defmethod object-update ((self progressive-text) (dt float))
  (object-set self 'accum_delta (+ (object-get self 'accum_delta) dt))
  (when (> (object-get self 'accum_delta) 0.05)
    (setf (text-text self) (if (<= (object-get self 'txt_index) (length (text-text-to-render self)))
                               (subseq (text-text-to-render self) 0 (object-get self 'txt_index))
                               (text-text-to-render self)))
    (object-set self 'accum_delta 0)
    (object-set self 'txt_index (+ 1 (object-get self 'txt_index)))
    (when (= (length (text-text-to-render self)) (length (text-text self)))
      ;text displaying ends here
      )))

;=================
; Drawable class
;=================

(defclass drawable (object)
  ((name :accessor object-name :initarg :name :initform "DEFAULT_DRAWABLE")
   (image-path :accessor drawable-image-path :initarg :image-path :initform nil)
   (image :accessor drawable-image :initarg :image :initform nil)
   (image-rect :accessor drawable-image-rect :initarg :image-rect :initform (make-instance 'rectangle))
   (anim-index :accessor drawable-anim-index :initarg :anim-index :initform 0)
   (delta-accum :accessor drawable-delta-accum :initform 0)
   ))

(defparameter *image-cache* (make-hash-table :test 'equal))

(defmethod object-init ((obj drawable))
  ;make sure image loads only once per drawable. the image is supposed to be a tileset
  (if (not (gethash (drawable-image-path obj) *image-cache*))
      (progn
        (setf (drawable-image obj) (sdl2-image:load-image (drawable-image-path obj)))
        (setf (gethash (drawable-image-path obj) *image-cache*) (drawable-image obj)))
      (progn
        (setf (drawable-image obj) (gethash (drawable-image-path obj) *image-cache*))
        )))

(defmethod object-on-collide ((self drawable) (obj_b object)))
(defmethod object-on-collide ((self object) (obj_b drawable)))

(defmethod object-draw ((dr drawable) dst_surf)
  (sdl2:blit-surface (drawable-image dr) (rect-to-sdl2-rect (drawable-image-rect dr))
                     dst_surf (sdl2:make-rect (object-x dr) (object-y dr)
                                              (object-width dr) (object-height dr))) )

(defclass tile (drawable)
  ())

(defun make-tiles (sc layer_str tile_size tile_sheet_path tile_map)
  (declare (string layer_str) (string tile_sheet_path) (list tile_map) (number tile_size))
  (loop for row in tile_map
        for ri = 0 then (+ ri 1)
        do (loop for col in row
                 for ci = 0 then (+ ci 1)
                 do (progn
                      (add-obj-to-scene
                        sc
                        layer_str
                        (make-instance 'tile
                                       :x (* ci tile_size)
                                       :y (* ri tile_size)
                                       :image-path tile_sheet_path
                                       :image-rect (make-instance 
                                                     'rectangle 
                                                     :x (* (aref col 0) tile_size) 
                                                     :y (* (aref col 1) tile_size) 
                                                     :w tile_size :h tile_size)))
                      ))))

(defun map-from-size (width height tile_size vec)
  (let ((num_rows (/ height tile_size))
        (num_cols (/ width tile_size))
        (result_list (list)))
    (loop for ri = 0 then (+ 1 ri)
          while (< ri num_rows)
          do (let ((tmp_list (list))) 
               (loop for ci = 0 then (+ 1 ci)
                   while (< ci num_cols)
                   do (push vec tmp_list))
               (push tmp_list result_list))
          )
    result_list))

(defun map-set-tile (amap x y vec)
  (if (and
        (> (length (nth 0 amap)) x)
        (> (length amap) y))
      (progn
        (setf (nth x (nth y amap)) vec))
      (progn
        (format t "Coordinates too big~%")
        )
      ))

(defmethod drawable-set-frame ((obj drawable) (frame number))
  (setf (rect-x (drawable-image-rect obj)) (* frame (rect-w (drawable-image-rect obj)))))

(defmethod drawable-set-anim-index ((obj drawable) (anim_index number))
  (setf (drawable-anim-index obj) anim_index)
  (setf (rect-y (drawable-image-rect obj)) (* (drawable-anim-index obj) (rect-h (drawable-image-rect obj)))))

(defmethod drawable-get-frame ((obj drawable))
  (/ (rect-x (drawable-image-rect obj)) (rect-w (drawable-image-rect obj))))

(defmethod drawable-is-last-frame ((obj drawable))
  (= (rect-x (drawable-image-rect obj)) (sdl2:surface-width (drawable-image obj))))

(defmethod drawable-animate ((obj drawable) (dt float))
  (setf (drawable-delta-accum obj) (+ dt (drawable-delta-accum obj)))
  (drawable-set-anim-index obj (drawable-anim-index obj))
  (when (> (drawable-delta-accum obj) 0.2)
    (setf (drawable-delta-accum obj) 0)
    (drawable-set-frame obj (+ 1 (drawable-get-frame obj))))
  (when (drawable-is-last-frame obj)
    (drawable-set-frame obj 0)))

;==================================
; Camera
;==================================

(defclass camera ()
  ((x :accessor camera-x :initarg :x :initform 0)
   (y :accessor camera-y :initarg :y :initform 0)
   (w :accessor camera-w :initarg :w :initform 0)
   (h :accessor camera-h :initarg :h :initform 0)
   (surface :accessor camera-surface :initarg :surface :initform nil)
   (parent :accessor camera-parent :initarg :parent :initform nil)
   (main-surface :accessor camera-main-surface :initform nil)
   ))

(defmethod camera-init ((c camera) dst_surf)
  (setf (camera-surface c) (sdl2:create-rgb-surface (camera-w c) (camera-h c) 32))
  (setf (camera-main-surface c) dst_surf))

(defmethod camera-center ((c camera))
  (setf (camera-x c) (+ (* -1 (object-x (camera-parent c))) (/ (camera-w c) 2) ) )
  (setf (camera-y c) (+ (* -1 (object-y (camera-parent c))) (/ (camera-h c) 2))))

;==================================
; Utility functions
;==================================

(defmethod add-2d-vectors ((vec_1 vector) (vec_2 vector))
  (vector (+ (aref vec_1 0) (aref vec_2 0)) (+ (aref vec_1 1) (aref vec_2 1))))

(defmethod sub-2d-vectors ((vec_1 vector) (vec_2 vector))
  (vector (- (aref vec_1 0) (aref vec_2 0)) (- (aref vec_1 1) (aref vec_2 1))))

(defmethod normalize-2d-vector ((vec vector))
  (labels ((comp (vec index)
             (cond
               ((> (aref vec index) 0) 1)
               ((< (aref vec index) 0) -1)
               ((= (aref vec index) 0) 0)
               )))
    (vector (comp vec 0) (comp vec 1))))

;==================================
;   Main functions
;==================================

(defmethod add-obj-to-scene ((sc scene) (layer_name string) (obj object))
  (let ((layer (loop for l in (scene-layers sc)
                     when (string-equal (layer-name l) layer_name)
                     do (return l))))
    ;initialize object before pushing it into the layer in the scene
    (setf (object-scene obj) sc)
    (object-init obj)
    (push obj (layer-objects layer))
    ))

(defmethod ready-all-objects ((sc scene))
  (loop for a_layer in (scene-layers sc)
        do (loop for obj in (layer-objects a_layer)
                 do (progn
                      (if (object-custom-ready obj)
                          (funcall (object-custom-ready obj) obj)
                          (object-ready obj))
                      (loop for a_script in (object-scripts obj)
                            do (funcall (script-ready a_script) obj))))))

(defun check-collision (sc obj)
  (loop for a_layer in (scene-layers sc)
        do (loop for obj_b in (layer-objects a_layer)
                 do (progn
                      (if (eq obj obj_b)
                          nil
                          (when (object-collide obj obj_b)
                            (object-on-collide  obj obj_b)
                            (loop for a_script in (object-scripts obj)
                                  do (funcall (script-on-collide a_script) obj obj_b))))))))

(defun update-and-draw-scene (dst_surf sc dt)
  (loop for a_layer in (scene-layers sc)
        do (loop for obj in (sort (layer-objects a_layer) 
                                  (lambda (a b)
                                    (< (object-z-index a) (object-z-index b))))
                 do (progn
                      (if (object-custom-update obj)
                          (funcall (object-custom-update obj) obj dt)
                          (object-update obj dt))
                      (loop for a_script in (object-scripts obj)
                            do (progn
                                 (funcall (script-update a_script) obj dt)
                                 (funcall (script-draw a_script) obj dst_surf)))
                      (when (object-collision-enabled obj)
                        (check-collision sc obj))
                      (if (object-custom-draw obj)
                          (funcall (object-custom-draw obj) obj dst_surf)
                          (object-draw obj dst_surf))
                      ))))

(defun add-input-handler (obj)
  (push obj *input-handlers*))

(defun remove-input-handler (obj)
  (remove obj *input-handlers* :test 'eq))

(defun handle-key-down (scancode)
  (loop for obj in *input-handlers*
        do (progn
             (if (object-custom-input obj)
                 (funcall (object-custom-input obj) obj scancode t)
                 (object-input obj scancode t))
             (loop for a_script in (object-scripts obj)
                   do (funcall (script-input a_script) obj scancode t)))))

(defun handle-key-up (scancode)
  (loop for obj in *input-handlers*
        do (progn
             (if (object-custom-input obj)
                 (funcall (object-custom-input obj) obj scancode nil)
                 (object-input obj scancode nil))
             (loop for a_script in (object-scripts obj)
                   do (funcall (script-input a_script) obj scancode nil)))))

(defun main (sc cam width height)
  (declare (scene sc) (camera cam ) (number width ) (number height ))
  (format t "width is ~S~%" width)
  (format t "height is ~S~%" height)
  (sdl2:with-init (:everything)
    (sdl2-image:init (list :png))
    (sdl2-ttf:init)
    (setf *default-font* (sdl2-ttf:open-font (truename *default-font-path*) 8))
    (sdl2:with-window (win :title "Scarli" :flags (list :shown) :w width :h height)
      ;setup main window surface and variables for calculating delta and limmiting fps
      (let ((main_surface (sdl2:get-window-surface win))
            (time_seconds (/ (sdl2:get-ticks) 1000.0))
            (max_frame_ticks (/ 1000.0 *MAX_FPS*))
            (fps 0)
            (last_ticks (sdl2:get-ticks))
            (sec_surf (sdl2:create-rgb-surface 1024 720 32)))
        (camera-init cam main_surface)
        (ready-all-objects sc)
        (sdl2:with-event-loop (:method :poll)
          (:idle ()
           (setf fps (+ 1 fps))
           ;more variables to calculate delta and setup frame limmiting
           (let* ((new_time (/ (sdl2:get-ticks) 1000.0))
                  (delta (- new_time time_seconds))
                  (target_ticks (+ last_ticks (* fps max_frame_ticks)))
                  (current_ticks (sdl2:get-ticks)))
             (setf time_seconds new_time)
             ;(format t "Target ticks:~S~%" target_ticks)
             (when (< current_ticks target_ticks)
               ;when current ticks is less than target ticks
               (progn
                 ;calculate how much to delay in between frames
                 (sdl2:delay (round (- target_ticks current_ticks)))
                 ;update current_ticks
                 (setf current_ticks (sdl2:get-ticks))))
             
             ;update logic goes here, the code above should delay the appropriate ammount of time
             (sdl2:fill-rect main_surface nil (sdl2:map-rgb (sdl2:surface-format main_surface) #x00 #x00 #x00))
             (sdl2:fill-rect sec_surf nil (sdl2:map-rgb (sdl2:surface-format main_surface) #x00 #x00 #x00))

             ;update and draw the persitent scene
             (update-and-draw-scene sec_surf *persistent-scene* delta)
             ;update and draw the game main scene
             (update-and-draw-scene sec_surf sc delta)
             (when (camera-parent cam)
               (camera-center cam))
             
             (sdl2:blit-surface sec_surf nil
                                main_surface (sdl2:make-rect (camera-x cam) (camera-y cam)
                                                             (camera-w cam) (camera-h cam)))
             (sdl2:update-window win)
             ;update fps counter every second along with last ticks
             (when (>= (- current_ticks last_ticks) 1000)
               (format t "FPS:~S~%" fps)
               (setf fps 0)
               (setf last_ticks (sdl2:get-ticks))
               )))
          (:quit () (progn
                      (sdl2-ttf:close-font *default-font*)
                      (sdl2-ttf:quit)
                      (sdl2-image:quit)
                      t))
          (:keydown (:keysym keysym)
           (let ((scancode (sdl2:scancode-value keysym)))
             (handle-key-down scancode)))
          (:keyup (:keysym keysym)
           (let ((scancode (sdl2:scancode-value keysym)))
             (handle-key-up scancode))))))))
