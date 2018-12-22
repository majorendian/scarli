(require :scarli)

(defpackage scarli-test-5
  (:use :cl :scarli))

(in-package scarli-test-5)

(defparameter *width* 640)
(defparameter *height* 480)

(defparameter *tile-map* (map-from-size *width* *height* 32 #(1 1)))

(defparameter *testscene* (make-instance 'scene
                                         :layers (list
                                                   (make-instance 'layer :name "bottom"))))
(map-set-tile *tile-map* 19 14 #(0 0))
(map-set-tile *tile-map* 10 5 #(1 0))
(map-set-tile *tile-map* 5 5 #(1 0))
(map-set-tile *tile-map* 8 12 #(1 0))
(map-set-tile *tile-map* 4 9 #(1 0))
(map-set-tile *tile-map* 9 3 #(1 0))
(make-tiles *testscene* "bottom" 32 "tile_sheet.png" *tile-map*)
(main *testscene* *width* *height*)
