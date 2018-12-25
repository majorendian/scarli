
(require :scarli)
(require :sb-sprof)

(defpackage scarli-test-6
  (:use :cl :scarli :scarli-player))

(in-package :scarli-test-6)

(defparameter *width* 640)
(defparameter *height* 480)

(defparameter *testscene*
  (make-instance 'scene
                 :layers (list
                           (make-instance 'layer :name "bottom")
                           (make-instance 'layer :name "middle")
                           )))

(defparameter *tile-map* (map-from-size 1024 768 32 #(1 0)))

(defparameter *player* (get-default-player 32 32 "test_spritesheet.png"))

(defun make-square (x y)
  (make-instance 'scarli:object
                 :name "square_one"
                 :x x
                 :y y
                 :w 32
                 :h 32
                 :collision-enabled t
                 :collision-rect (make-instance 'scarli:rectangle :x x :y y :w 32 :h 32)
                 :draw (lambda (self dst_surf)
                         (sdl2:fill-rect 
                           dst_surf 
                           (sdl2:make-rect (scarli:object-x self)
                                           (scarli:object-y self)
                                           (scarli:object-width self)
                                           (scarli:object-height self))
                           (sdl2:map-rgb (sdl2:surface-format dst_surf) #xff #x00 #x00)
                           ))))

(defparameter *camera* (make-instance 'camera
                                      :w 640
                                      :h 480
                                      :parent *player*))

(map-set-tile *tile-map* 0 0 #(0 0 'solid-tile))
(map-set-tile *tile-map* 1 0 #(0 0 'solid-tile))
(add-obj-to-scene *testscene* "middle" *player*)
(add-obj-to-scene *testscene* "middle" (make-square (/ *width* 2) 400))
(add-obj-to-scene *testscene* "middle" (make-square (+ 32 (/ *width* 2)) 400))
(add-obj-to-scene *testscene* "middle" (make-square (+ 64 (/ *width* 2)) 400))
(add-input-handler *player*)
(make-tiles *testscene* "bottom" 32 "tile_sheet.png" *tile-map*)
;(sb-sprof:start-profiling :max-samples 10000)
(main *testscene* *camera* *width* *height*)
;(sb-sprof:stop-profiling)
;(sb-sprof:report :type :flat)
