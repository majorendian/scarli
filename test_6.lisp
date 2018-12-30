
(require :scarli)

(defpackage scarli-test-6
  (:use :cl :scarli :scarli-player :scarli-objects))

(in-package :scarli-test-6)

(defparameter *width* 640)
(defparameter *height* 480)


(defun make-default-scene ()
  (make-instance 'scene
                 :layers (list
                           (make-instance 'layer :name "bottom")
                           (make-instance 'layer :name "middle")
                           (make-instance 'layer :name "top")
                           )
                 :width 640
                 :height 480))
(defparameter *testscene* (make-default-scene))

(defparameter *player* (get-default-player 64 64 "test_spritesheet.png"))
(setf (object-z-index *player*) 1)

(defparameter *stairs* (make-tile :tile-sheet-path "tile_sheet.png"
                                  :tile-size 32
                                  :tile-class 'entrance
                                  :ri 0
                                  :ci 2
                                  :x (* 14 32)
                                  :y (* 8 32)))

(setf (object-collision-enabled *stairs*) t)

(defparameter *camera* (make-instance 'camera
                                      :w 640
                                      :h 480
                                      :parent *player*))


(defparameter *newscene* (make-default-scene))

(setf (entrance-next-scene *stairs*) *newscene*)
(setf (entrance-next-player-pos *stairs*) #(64 64))
(setf (entrance-func-load *stairs*) (lambda ()
                                      (display-tiles *newscene* "level_2.map")
                                      (add-obj-to-scene *newscene* "middle" *player*)))

(add-obj-to-scene *testscene* "middle" *player*)
(add-obj-to-scene *testscene* "middle" *stairs*)
;(add-obj-to-scene *testscene* "middle" *sign*)
(clear-input-handlers)
(add-input-handler *player*)
;(sb-sprof:start-profiling :max-samples 10000)
(display-tiles *testscene* "level_1.map")
(main *testscene* *camera* *width* *height*)
;(sb-sprof:stop-profiling)
;(sb-sprof:report :type :flat)
