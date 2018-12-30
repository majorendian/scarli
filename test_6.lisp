
(require :scarli)

(defpackage scarli-test-6
  (:use :cl :scarli :scarli-player :scarli-objects))

(in-package :scarli-test-6)

(defparameter *width* 640)
(defparameter *height* 480)

(defparameter *testscene*
  (make-instance 'scene
                 :layers (list
                           (make-instance 'layer :name "bottom")
                           (make-instance 'layer :name "middle")
                           )
                 :width 640
                 :height 480))

(defparameter *player* (get-default-player 64 64 "test_spritesheet.png"))



;(defparameter *pushable-block* (make-tile :tile-sheet-path "tile_sheet.png"
                                          ;:tile-size 32
                                          ;:tile-class 'pushable-block
                                          ;:x (* 10 32)
                                          ;:y (* 10 32)
                                          ;:ci 0
                                          ;:ri 2))

;(defparameter *pushable-block-2* (make-tile :tile-sheet-path "tile_sheet.png"
                                          ;:tile-size 32
                                          ;:tile-class 'pushable-block
                                          ;:x (* 12 32)
                                          ;:y (* 10 32)
                                          ;:ci 0
                                          ;:ri 2))

(defparameter *sign* (make-tile :tile-sheet-path "tile_sheet.png"
                                :tile-size 32
                                :tile-class 'interactible
                                :x (* 14 32)
                                :y (* 11 32)
                                :ci 1
                                :ri 2))
(setf (interactible-pages *sign*) (list 
                                    (list "This is a sign. It is old and wooden.")
                                    (list "There seems to be something written on it,"
                                          "but it is impossible to make out.")))

(defparameter *camera* (make-instance 'camera
                                      :w 640
                                      :h 480
                                      :parent *player*))



(add-obj-to-scene *testscene* "middle" *player*)
(add-obj-to-scene *testscene* "middle" *pushable-block*)
(add-obj-to-scene *testscene* "middle" *pushable-block-2*)
(add-obj-to-scene *testscene* "middle" *sign*)
(clear-input-handlers)
(add-input-handler *player*)
;(sb-sprof:start-profiling :max-samples 10000)
(display-tiles *testscene* "editor-output.map")
(main *testscene* *camera* *width* *height*)
;(sb-sprof:stop-profiling)
;(sb-sprof:report :type :flat)
