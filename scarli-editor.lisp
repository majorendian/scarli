
(require :scarli)

(defpackage scarli-editor
  (:use cl :scarli)
  (:export highlight-square))

(in-package :scarli-editor)
(defparameter *width* 640)
(defparameter *height* 480)
(defparameter *scene* (make-instance 'scene
                                     :layers (list
                                               (make-instance 'layer :name "bottom")
                                               (make-instance 'layer :name "middle")
                                               (make-instance 'layer :name "top")
                                               )))

(defclass highlight-square (object)
  ())

(defmethod object-ready ((self highlight-square))
  (object-set self 'modifying_tile nil)
  (object-set self 'color_r #x00))

(defmethod object-draw ((self highlight-square) dst_surf)
  (let ((top_line (sdl2:make-rect (object-x self) (object-y self) (object-width self) 2))
        (left_line (sdl2:make-rect (object-x self) (object-y self) 2 (object-height self)))
        (bottom_line (sdl2:make-rect (object-x self) (- (+ (object-height self) (object-y self))2) (object-width self) 2))
        (right_line (sdl2:make-rect (- (+ (object-width self) (object-x self))2) (object-y self) 2 (object-height self))))
    (sdl2:fill-rect dst_surf top_line (sdl2:map-rgb (sdl2:surface-format dst_surf) (object-get self 'color_r) #xff #x00))
    (sdl2:fill-rect dst_surf left_line (sdl2:map-rgb (sdl2:surface-format dst_surf) (object-get self 'color_r) #xff #x00))
    (sdl2:fill-rect dst_surf bottom_line (sdl2:map-rgb (sdl2:surface-format dst_surf) (object-get self 'color_r) #xff #x00))
    (sdl2:fill-rect dst_surf right_line (sdl2:map-rgb (sdl2:surface-format dst_surf) (object-get self 'color_r) #xff #x00))
    ))

(defmethod object-input ((self highlight-square) scancode pressed)
  (if (not (object-get self 'modifying_tile)) 
      (when (not pressed)
        (cond
          ((sdl2:scancode= scancode :scancode-up) (setf (object-y self) (- (object-y self) (object-height self))))
          ((sdl2:scancode= scancode :scancode-down) (setf (object-y self) (+ (object-y self) (object-height self))))
          ((sdl2:scancode= scancode :scancode-left) (setf (object-x self) (- (object-x self) (object-width self))))
          ((sdl2:scancode= scancode :scancode-right) (setf (object-x self) (+ (object-x self) (object-width self))))
          ((sdl2:scancode= scancode :scancode-space)
           (let ((tile (get-obj-at-pos (object-scene self) (object-x self) (object-y self))))
             (if tile
                 (progn
                   (format t "found tile:~S~%" tile)
                   (object-set self 'color_r #xff)
                   (object-set self 'modifying_tile tile))
                 (progn
                   (format t "creating tile~%")
                   (setf tile (create-tile :tile-sheet-path "tile_sheet.png"
                                           :tile-size 32
                                           :tile-class 'tile
                                           :x (object-x self)
                                           :y (object-y self)))
                   (add-obj-to-scene (object-scene self) (object-layer self) tile)))
             ))
          ))
      (progn
        ;modifying tile
        (when (not pressed)
          (cond
            ((sdl2:scancode= scancode :scancode-up) 
             (setf (rect-y (drawable-image-rect (object-get self 'modifying_tile)))
                   (- (rect-y (drawable-image-rect (object-get self 'modifying_tile))) (rect-h (drawable-image-rect (object-get self 'modifying_tile))))))
            ((sdl2:scancode= scancode :scancode-down) 
             (setf (rect-y (drawable-image-rect (object-get self 'modifying_tile)))
                   (+ (rect-y (drawable-image-rect (object-get self 'modifying_tile))) (rect-h (drawable-image-rect (object-get self 'modifying_tile))))))
            ((sdl2:scancode= scancode :scancode-left) 
             (setf (rect-x (drawable-image-rect (object-get self 'modifying_tile)))
                   (- (rect-x (drawable-image-rect (object-get self 'modifying_tile))) (rect-w (drawable-image-rect (object-get self 'modifying_tile))))))
            ((sdl2:scancode= scancode :scancode-right) 
             (setf (rect-x (drawable-image-rect (object-get self 'modifying_tile)))
                   (+ (rect-x (drawable-image-rect (object-get self 'modifying_tile))) (rect-w (drawable-image-rect (object-get self 'modifying_tile))))))
            ((sdl2:scancode= scancode :scancode-space) (progn
                                                         (object-set self 'color_r #x00)
                                                         (object-set self 'modifying_tile nil)))

            ))
        )))

(defparameter *user-cursor* (make-instance 'highlight-square :x 0 :y 0 :w 32 :h 32))
(defparameter *camera* (make-instance 'camera :x 0 :y 0 :w *width* :h *height* :parent *user-cursor*))

(add-obj-to-scene *scene* "middle" *user-cursor*)
(clear-input-handlers)
(add-input-handler *user-cursor*)
(main *scene* *camera* *width* *height*)
