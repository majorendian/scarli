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
           text
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
           rectangle
           rect-x
           rect-y
           rect-w
           rect-h
           add-2d-vectors
           sub-2d-vectors
           normalize-2d-vector
           add-obj-to-scene
           add-input-handler
           *main-scene*
           main
           ))

(in-package :scarli)


(defparameter *default-font-path* "kongtext.ttf")
(defparameter *default-font* nil)
(defparameter *MAX_FPS* 60)

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
                                                                (declare (ignore self) (ignore parent) (ignore dt))))
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
   (collision-rect :accessor object-collision-rect :initarg :collision-rect :initform (make-instance 'rectangle))
   (collision-enabled :accessor object-collision-enabled :initarg :collision-enabled :initform nil)
   (on-collide :accessor object-on-collide :initarg :on-collide :initform (lambda (self collider)
                                                                            (declare (ignore self) (ignore collider))))
   (is-colliding :accessor object-is-colliding :initform nil)
   (z-index :accessor object-z-index :initarg :z-index :initform 0)
   (attributes :accessor object-attributes :initform (make-hash-table))
   (scripts :accessor object-scripts :initarg :scripts :initform (list))
   (init :accessor object-init :initform (lambda (self)
                                           (declare (ignore self))))
   (ready :accessor object-ready :initarg :ready :initform (lambda (self)
                                                             (declare (ignore self))))
   (update :accessor object-update :initarg :update :initform (lambda (self dt)
                                                                (declare (ignore self) (ignore dt))))
   (draw :accessor object-draw :initarg :draw :initform (lambda (self dst_surf)
                                                          (declare (ignore self) (ignore dst_surf))))
   (input :accessor object-input :initarg :input :initform (lambda (self scancode pressed)
                                                             (declare (ignore self) (ignore scancode) (ignore pressed))))))


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

;=================
; Text class
;=================
(defclass text (object)
  ((text :accessor text-text :initarg :text :initform "PLACEHOLDER_TEXT")
   (draw :accessor object-draw :initform (lambda (self dst_surf)
                                           (let ((text_surf (sdl2-ttf:render-text-solid *default-font*
                                                                                        (text-text self)
                                                                                        255 255 255 0)))
                                             (sdl2:blit-surface text_surf nil
                                                                dst_surf
                                                                (sdl2:make-rect (object-x self) (object-y self)
                                                                                (sdl2:surface-width text_surf) (sdl2:surface-height text_surf)))
                                             (sdl2:free-surface text_surf))))))

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
   (init :accessor object-init :initarg :init :initform 
         (lambda (self)
           (setf (drawable-image self) (sdl2-image:load-image (drawable-image-path self)))))
   (draw :accessor object-draw :initarg :draw :initform
         (lambda (self dst_surf)
           (sdl2:blit-surface (drawable-image self) (rect-to-sdl2-rect (drawable-image-rect self))
                              dst_surf (sdl2:make-rect (object-x self) (object-y self)
                                                       (object-width self) (object-height self)))))))


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
    (funcall (object-init obj) obj)
    (push obj (layer-objects layer))
    ))

(defmethod ready-all-objects ((sc scene))
  (loop for a_layer in (scene-layers sc)
        do (loop for obj in (layer-objects a_layer)
                 do (progn
                      (funcall (object-ready obj) obj)
                      (loop for a_script in (object-scripts obj)
                            do (funcall (script-ready a_script) obj))))))

(defun check-collision (sc obj)
  (loop for a_layer in (scene-layers sc)
        do (loop for obj_b in (layer-objects a_layer)
                 do (progn
                      (if (eq obj obj_b)
                          nil
                          (when (object-collide obj obj_b)
                            (funcall (object-on-collide obj) obj obj_b)
                            (loop for a_script in (object-scripts obj)
                                  do (funcall (script-on-collide a_script) obj obj_b))))))))

(defun update-and-draw-scene (dst_surf sc dt)
  (loop for a_layer in (scene-layers sc)
        do (loop for obj in (sort (layer-objects a_layer) 
                                  (lambda (a b)
                                    (< (object-z-index a) (object-z-index b))))
                 do (progn
                      (funcall (object-update obj) obj dt)
                      (loop for a_script in (object-scripts obj)
                            do (progn
                                 (funcall (script-update a_script) obj dt)
                                 (funcall (script-draw a_script) obj dst_surf)))
                      (check-collision sc obj)
                      (funcall (object-draw obj) obj dst_surf)))))

(defun add-input-handler (obj)
  (push obj *input-handlers*))


(defun handle-key-down (scancode)
  (loop for obj in *input-handlers*
        do (progn
             (funcall (object-input obj) obj scancode t)
             (loop for a_script in (object-scripts obj)
                   do (funcall (script-input a_script) obj scancode t)))))

(defun handle-key-up (scancode)
  (loop for obj in *input-handlers*
        do (progn
             (funcall (object-input obj) obj scancode nil)
             (loop for a_script in (object-scripts obj)
                   do (funcall (script-input a_script) obj scancode nil)))))

(defun main (sc width height)
  (format t "width is ~S~%" width)
  (format t "height is ~S~%" height)
  (sdl2:with-init (:everything)
    (sdl2-image:init (list :png))
    (sdl2-ttf:init)
    (setf *default-font* (sdl2-ttf:open-font (truename *default-font-path*) 16))
    (sdl2:with-window (win :title "Scarli" :flags (list :shown) :w width :h height)
      ;setup main window surface and variables for calculating delta and limmiting fps
      (let ((main_surface (sdl2:get-window-surface win))
            (time_seconds (/ (sdl2:get-ticks) 1000.0))
            (max_frame_ticks (/ 1000.0 *MAX_FPS*))
            (fps 0)
            (last_ticks (sdl2:get-ticks)))
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

             (update-and-draw-scene main_surface sc delta)
             
             (sdl2:update-window win)
             ;update fps counter every second along with last ticks
             (when (>= (- current_ticks last_ticks) 1000)
               ;(format t "FPS:~S~%" fps)
               (setf fps 0)
               (setf last_ticks (sdl2:get-ticks))
               )))
          (:quit () t)
          (:keydown (:keysym keysym)
           (let ((scancode (sdl2:scancode-value keysym)))
             (handle-key-down scancode)))
          (:keyup (:keysym keysym)
           (let ((scancode (sdl2:scancode-value keysym)))
             (handle-key-up scancode))))))))
