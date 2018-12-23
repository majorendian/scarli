
(require :scarli)

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

(defparameter *tile-map* (map-from-size *width* *height* 32 #(1 0)))

(defparameter *player* (get-default-player 0 0 "test_spritesheet.png"))
(add-obj-to-scene *testscene* "middle" *player*)
(add-input-handler *player*)
(make-tiles *testscene* "bottom" 32 "tile_sheet.png" *tile-map*)
(main *testscene* *width* *height*)
