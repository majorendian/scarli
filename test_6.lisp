
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


(setf (object-name *stairs*) "floor_1")
(setf (entrance-connected-door-id *stairs*) "floor_2_1")
(setf (entrance-next-player-pos *stairs*) #(0 32))


(defparameter *newscene* (make-default-scene))
(add-obj-to-scene *newscene* "middle" *player*)

(setf (entrance-next-level *stairs*) "level_2.map")

(defparameter *camera* (make-instance 'camera
                                      :w 640
                                      :h 480
                                      :parent *player*))

(register-scene *newscene* "level_2.map")
(register-scene *testscene* "level_1.map")
(add-obj-to-scene *testscene* "middle" *player*)
(add-obj-to-scene *testscene* "middle" *stairs*)
;;(add-obj-to-scene *testscene* "middle" *sign*)
(clear-input-handlers)
(add-input-handler *player*)
;(sb-sprof:start-profiling :max-samples 10000)
(display-tiles *testscene* "level_1.map")
(main "Prime Garden" *testscene* *camera* *width* *height*)
;(sb-sprof:stop-profiling)
;(sb-sprof:report :type :flat)
