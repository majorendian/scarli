
(require :scarli)

(defpackage :scarli-test-8
  (:use :cl :scarli))

(in-package :scarli-test-8)

(defparameter *width* 640)
(defparameter *height* 480)

(defparameter *camera* (make-instance 'camera
                                      :x 0
                                      :y 0
                                      :w *width*
                                      :h *height*))

(defparameter *testscene* (make-instance 'scene
                                         :layers (list
                                                   (make-instance 'layer :name "bottom"))))

(main *testscene* *camera* *width* *height*)
