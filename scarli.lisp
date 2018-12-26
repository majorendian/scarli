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
           <-
           object-get
           ->
           object-collision-rect
           object-collision-enabled
           object-collide
           object-is-colliding
           object-input
           object-ready
           object-move
           object-remove
           object-mouse-button
           object-scene
           object-layer
           drawable-image-rect
           text
           text-text
           progressive-text
           paged-text
           multiline-text
           script
           script-update
           script-input
           script-draw
           script-name
           script-ready
           drawable
           drawable-advance-frame
           drawable-animate
           drawable-image
           drawable-anim-index
           drawable-set-frame
           drawable-set-anim-index
           drawable-get-frame
           tile
           solid-tile
           make-tiles
           create-tile
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
           get-obj-at-pos
           get-obj-at-pos-in-layer
           clear-input-handlers
           *persistent-scene*
           *mouse_x*
           *mouse_y*
           main
           ))

(in-package :scarli)


(defparameter *default-font-path* "kongtext.ttf")
(defparameter *default-font* nil)
(defparameter *default-font-size* 12)
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
   (layer :accessor object-layer :initarg :layer :initform nil)
   (collision-rect :accessor object-collision-rect :initarg :collision-rect :initform (make-instance 'rectangle))
   (collision-enabled :accessor object-collision-enabled :initarg :collision-enabled :initform nil)
   (on-collide :accessor object-custom-on-collide :initarg :on-collide :initform (lambda (self collider)
                                                                            (declare (ignore self) (ignore collider))))
   (signals :accessor object-signals :initform (make-hash-table))
   (children :accessor object-children :initform (list))
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

;(defgeneric object-input (obj scancode pressed)
;  (:documentation "Fires when added into *input-handlers* this function processes key presses"))

(defgeneric object-on-collide (self obj_b)
  (:documentation "Fires when 2 objects have an intersecting collision rectangle"))

(defgeneric object-add-signal-handler (self symbol function)
  (:documentation "Add a handler to a signal named by SYMBOL"))

(defmethod object-add-signal-handler ((self object) (s symbol) (f function))
  (setf (gethash s (object-signals self)) f))

(defmethod object-remove-signal-handler ((self object) (s symbol))
  (remhash s (object-signals self)))

(defmethod object-fire-signal ((self object) (sig symbol))
  (let ((sighandler (gethash sig (object-signals self))))
    (when sighandler
      (funcall sighandler self))))

(defmethod object-init ((obj object)))

(defmethod object-update ((self object) (dt float)))

(defmethod object-draw ((self object) dst_surf))

(defmethod object-ready ((self object)))

(defmethod object-input ((self object) scancode pressed))

(defmethod object-mouse-button ((self object) btn_index pressed))

(defmethod object-on-collide ((self object) (obj_b object)))

(defmethod object-add-child ((self object) (obj object))
  (push obj (object-children self))
  (setf (object-scene obj) (object-scene self))
  (setf (object-layer obj) (object-layer self))
  (object-ready obj)
  (loop for child in (object-children obj)
        do (progn
             (setf (object-scene child) (object-scene obj))
             (setf (object-layer child) (object-layer obj))
             (object-ready child))))

(defmethod object-remove-child ((self object) (obj object))
  (setf (object-children self) (remove obj (object-children self) :test 'eq)))

(defmethod object-set ((obj object) (sym symbol) (val t))
  (setf (gethash sym (object-attributes obj)) val))

(defmethod object-get ((obj object) (sym symbol))
  (gethash sym (object-attributes obj)))

(defmacro -> (obj sym)
  `(gethash ,sym (object-attributes ,obj)))

(defmacro <- (obj sym val)
  `(setf (-> ,obj ,sym) ,val))

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


(defun get-layer (sc layername)
  (loop for l in (scene-layers sc)
        when (string= (layer-name l) layername)
        do (progn
             (format t "Found layer with name:~S~%" layername)
             (return l))))

(defmethod object-remove ((obj object))
  (setf (layer-objects (get-layer (object-scene obj) (object-layer obj))) 
        (remove obj (layer-objects (get-layer (object-scene obj) (object-layer obj))) :test 'eq)))

;=================
; Text class
;=================
(defclass text (object)
  ((text :accessor text-text :initarg :text :initform " ")))

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





;===========================
; NOTE: need multiline text from sdl2-ttf
;===========================
(defclass progressive-text (text)
  ((text-to-render :accessor text-text-to-render :initarg :text :initform " ")
   (current-text :accessor text-text :initform "")
   ))

(defmethod object-ready ((self progressive-text))
  (format t "initializing progressive text~%")
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
      (object-fire-signal self 'text-finished))
    ))

(defclass multiline-text (object)
  ((lines :accessor text-lines :initarg :lines :initform (list))
   ))

(defmethod multiline-text-new-line ((self multiline-text) y)
  (let ((newline_text (make-instance 'progressive-text
                                     :name "newline"
                                     :x (object-x self)
                                     :y y
                                     :text (nth (object-get self 'line_index) (text-lines self)))))
    (object-add-child self newline_text)
    newline_text))

(defmethod object-ready ((self multiline-text))
  (format t "ready to draw lines~%")
  (object-set self 'line_index 0)
  (object-set self 'line_y (object-y self))
  (object-set self 'last_line (multiline-text-new-line self (object-get self 'line_y)))
  ;the handler attaches itself to subsequent lines being displayed within itself
  (object-set self 'finished-text-handler 
              (lambda (text_obj)
                (declare (ignore text_obj))
                (if (< (object-get self 'line_index) (length (text-lines self)))
                    (progn
                      (object-set self 'line_index (+ 1 (object-get self 'line_index)))
                      (object-set self 'line_y (+ *default-font-size* (object-get self 'line_y)))
                      (object-remove-signal-handler (object-get self 'last_line) 'text-finished)
                      (object-set self 'last_line (multiline-text-new-line self (object-get self 'line_y)))
                      (object-add-signal-handler (object-get self 'last_line) 'text-finished (object-get self 'finished-text-handler)))
                    (progn
                      ;at last remove the last handler so this gets called only once
                      (object-remove-signal-handler (object-get self 'last_line) 'text-finished)
                      (object-fire-signal self 'multiline-text-finished)
                      ))))
  (object-add-signal-handler (object-get self 'last_line) 'text-finished (object-get self 'finished-text-handler)))

(defclass paged-text (object)
  ((pages :accessor paged-text-pages :initarg :pages :initform (list
                                                                 (list "page one line one"
                                                                       "page one line two")
                                                                 (list "page two line one"
                                                                       "page two line two")))
   (has-focus :accessor paged-text-has-focus :initform t)))

(defmethod paged-text-create-multiline ((self paged-text) (mul_text list))
  (let ((multi (make-instance 'multiline-text
                              :x (object-x self)
                              :y (object-y self)
                              :lines mul_text)))
    (object-add-child self multi)
    multi))

(defmethod object-ready ((self paged-text))
  (format t "readying paged text~%")
  (object-set self 'page_index 0)
  (object-set self 'page_finished nil)
  (object-set self 'last_multiline (paged-text-create-multiline self (nth (object-get self 'page_index) (paged-text-pages self))))
  (object-add-signal-handler (object-get self 'last_multiline) 'multiline-text-finished
                             (lambda (multi)
                               (format t "Finished displaying all lines~%")
                               (object-set self 'page_finished t)
                               )))

(defmethod object-input ((self paged-text) scancode pressed)
  (when (and (not pressed) (sdl2:scancode= scancode :scancode-space))
    (when (and (paged-text-has-focus self) (object-get self 'page_finished))
      (object-remove-child self (object-get self 'last_multiline))
      (format t "child removed~%")
      (object-set self 'page_index (+ 1 (object-get self 'page_index)))
      (when (< (object-get self 'page_index) (length (paged-text-pages self)))
        (object-set self 'last_multiline (paged-text-create-multiline self (nth (object-get self 'page_index) (paged-text-pages self)))))
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

(defclass solid-tile (tile)
  ((collision-enabled :accessor object-collision-enabled :initform t)))

(defun create-tile (&key tile-sheet-path tile-size tile-class x y)
  (make-instance tile-class
                 :x x
                 :y y
                 :image-path tile-sheet-path
                 :image-rect (make-instance 
                               'rectangle 
                               :x 0
                               :y 0
                               :w tile-size :h tile-size)
                 :collision-rect (make-instance 'rectangle
                                                :x x
                                                :y y
                                                :w tile-size
                                                :h tile-size)))

(defun make-tiles (sc layer_str tile_size tile_sheet_path tile_map &optional (default_tile_class 'tile))
  (declare (scene sc) (string layer_str) (number tile_size) (string tile_sheet_path) (list tile_map) (number tile_size))
  (loop for row in tile_map
        for ri = 0 then (+ ri 1)
        do (loop for col in row
                 for ci = 0 then (+ ci 1)
                 do (progn
                      (add-obj-to-scene
                        sc
                        layer_str
                        (make-instance (if (> (length col) 2)
                                           (eval (aref col 2))
                                           default_tile_class)
                                       :x (* ci tile_size)
                                       :y (* ri tile_size)
                                       :image-path tile_sheet_path
                                       :image-rect (make-instance 
                                                     'rectangle 
                                                     :x (* (aref col 0) tile_size) 
                                                     :y (* (aref col 1) tile_size) 
                                                     :w tile_size :h tile_size)
                                       :collision-rect (make-instance 'rectangle
                                                                      :x (* ci tile_size)
                                                                      :y (* ri tile_size)
                                                                      :w tile_size
                                                                      :h tile_size)))
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

(defun map-set-tile (amap col row vec)
  (if (and
        (> (length (nth 0 amap)) col)
        (> (length amap) row))
      (progn
        (setf (nth col (nth row amap)) vec))
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

(defmethod camera-center ((c camera) sec_surf)
  (setf (camera-x c) (+ (* -1 (object-x (camera-parent c))) (/ (camera-w c) 2) ) ) 
  (setf (camera-y c) (+ (* -1 (object-y (camera-parent c))) (/ (camera-h c) 2))) 
  (when (and (> (object-x (camera-parent c)) 0)
             (< (object-x (camera-parent c)) (/ (camera-w c) 2)))
    (setf (camera-x c) 0))
  (when (and (> (object-y (camera-parent c)) 0)
             (< (object-y (camera-parent c)) (/ (camera-h c) 2)))
    (setf (camera-y c) 0))
  (when (and (> (object-x (camera-parent c)) (- (sdl2:surface-width sec_surf) (/ (camera-w c) 2)))
             (< (object-x (camera-parent c)) (sdl2:surface-width sec_surf)))
    (format t "surface width:~S~%" (sdl2:surface-width (camera-main-surface c)))
    (setf (camera-x c) (* -1 (- (sdl2:surface-width sec_surf) (camera-w c)))))
  (when (and (> (object-y (camera-parent c)) (- (sdl2:surface-height sec_surf) (/ (camera-h c) 2)))
             (< (object-y (camera-parent c)) (sdl2:surface-height sec_surf)))
    (format t "surface width:~S~%" (sdl2:surface-height (camera-main-surface c)))
    (setf (camera-y c) (* -1 (- (sdl2:surface-height sec_surf) (camera-h c)))))
  )

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
    (setf (object-layer obj) layer_name)
    (loop for child in (object-children obj)
          do (object-init child))
    (object-init obj)
    (push obj (layer-objects layer))
    (when (sdl2:was-init)
      (loop for child in (object-children obj)
            do (object-ready child))
      (object-ready obj))
    ))

(defmethod ready-all-objects ((sc scene))
  (loop for a_layer in (scene-layers sc)
        do (loop for obj in (layer-objects a_layer)
                 do (progn
                      (if (object-custom-ready obj)
                          (funcall (object-custom-ready obj) obj)
                          (progn
                            (loop for child in (object-children obj)
                                  do (object-ready child))
                            (object-ready obj)))
                      (loop for a_script in (object-scripts obj)
                            do (funcall (script-ready a_script) obj))))))

(defun get-obj-at-pos (sc x y)
  (loop for a_layer in (scene-layers sc)
        do (loop for obj in (layer-objects a_layer)
                 do (progn
                      (when (and (= x (object-x obj)) (= y (object-y obj)))
                        (when (eq (find-class 'tile) (class-of obj))
                          (format t "found tile:~S~%" obj)
                          (return-from get-obj-at-pos obj)))))))

(defun get-obj-at-pos-in-layer (sc layer x y)
  (loop for obj in (layer-objects (get-layer sc layer))
        do (progn
             (when (and (= x (object-x obj)) (= y (object-y obj)))
               (when (eq (find-class 'tile) (class-of obj))
                 (format t "found tile:~S~%" obj)
                 (return-from get-obj-at-pos-in-layer obj)))) 
        ))

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

;NOTE: this function still needs more proper testing
(defun rec-update-and-draw-children (l dst_surf dt)
  (let ((child (first l)))
    (if child
        (progn
          (object-update child dt)
          (object-draw child dst_surf)
          (loop for s in (object-scripts child)
                do (progn
                     (funcall (script-update s) child dt)
                     (funcall (script-draw s) child dst_surf)))
          (rec-update-and-draw-children (object-children child) dst_surf dt)
          (rec-update-and-draw-children (rest l) dst_surf dt)))))

(defun update-and-draw-scene (dst_surf sc dt)
  (loop for a_layer in (scene-layers sc)
        do (loop for obj in (sort (layer-objects a_layer) 
                                  (lambda (a b)
                                    (< (object-z-index a) (object-z-index b))))
                 do (progn
                      (if (object-custom-update obj)
                          (funcall (object-custom-update obj) obj dt)
                          ;update and draw children first
                          (progn
                            ;iterate over children
                            (rec-update-and-draw-children (object-children obj) dst_surf dt)
                            ;then update object
                            (object-update obj dt)))
                      ;then update object scripts
                      (loop for a_script in (object-scripts obj)
                            do (progn
                                 (funcall (script-update a_script) obj dt)
                                 (funcall (script-draw a_script) obj dst_surf)))
                      ;finally check parent for collision
                      (when (object-collision-enabled obj)
                        (check-collision sc obj))
                      (if (object-custom-draw obj)
                          (funcall (object-custom-draw obj) obj dst_surf)
                          (object-draw obj dst_surf))
                      ))))

(defun add-input-handler (obj)
  (push obj *input-handlers*))

(defun clear-input-handlers ()
  (setf *input-handlers* (list)))

(defun remove-input-handler (obj)
  (remove obj *input-handlers* :test 'eq))

(defun handle-key-down (scancode)
  (loop for obj in *input-handlers*
        do (progn
             (if (object-custom-input obj)
                 (funcall (object-custom-input obj) obj scancode t)
                 (progn
                   (loop for child in (object-children obj)
                         do (object-input child scancode t))
                   (object-input obj scancode t)))
             (loop for a_script in (object-scripts obj)
                   do (funcall (script-input a_script) obj scancode t)))))

(defun handle-key-up (scancode)
  (loop for obj in *input-handlers*
        do (progn
             (if (object-custom-input obj)
                 (funcall (object-custom-input obj) obj scancode nil)
                 (progn
                   (loop for child in (object-children obj)
                         do (object-input child scancode nil))
                   (format t "calling input function on obj ~S~%" obj)
                   (object-input obj scancode nil)))
             (loop for a_script in (object-scripts obj)
                   do (funcall (script-input a_script) obj scancode nil)))))

(defparameter *mouse_x* 0)
(defparameter *mouse_y* 0)
(defun handle-mouse-down (button_idx)
  (loop for obj in *input-handlers*
        do (progn
             (object-mouse-button obj button_idx t))))
(defun handle-mouse-up (button_idx)
  (loop for obj in *input-handlers*
        do (progn
             (object-mouse-button obj button_idx nil))))

(defun handle-mouse-motion (x y xrel yrel state)
  (setf *mouse_x* x)
  (setf *mouse_y* y))

(defun main (sc cam width height)
  (declare (scene sc) (camera cam) (number width) (number height))
  (format t "width is ~S~%" width)
  (format t "height is ~S~%" height)
  (sdl2:with-init (:everything)
    (sdl2-image:init (list :png))
    (sdl2-ttf:init)
    (setf *default-font* (sdl2-ttf:open-font (truename *default-font-path*) *default-font-size*))
    (sdl2:with-window (win :title "Scarli" :flags (list :shown) :w width :h height)
      ;setup main window surface and variables for calculating delta and limmiting fps
      (let ((main_surface (sdl2:get-window-surface win))
            (time_seconds (/ (sdl2:get-ticks) 1000.0))
            (max_frame_ticks (/ 1000.0 *MAX_FPS*))
            (fps 0)
            (last_ticks (sdl2:get-ticks))
            (sec_surf (sdl2:create-rgb-surface 1024 720 32)))
        (camera-init cam main_surface)
        (ready-all-objects *persistent-scene*)
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

             ;update and draw the game main scene
             (update-and-draw-scene sec_surf sc delta)
             (when (camera-parent cam)
               (camera-center cam sec_surf))

             
             (sdl2:blit-surface sec_surf nil
                                main_surface (sdl2:make-rect (camera-x cam) (camera-y cam)
                                                             (camera-w cam) (camera-h cam)))

             ;update and draw the persitent scene
             (update-and-draw-scene main_surface *persistent-scene* delta)

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
             (handle-key-up scancode)))
          (:mousebuttondown (:button button)
           (handle-mouse-down button))
          (:mousebuttonup (:button button)
           (handle-mouse-up button))
          (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
           (handle-mouse-motion x y xrel yrel state)))))))
