
(require :scarli)

(defpackage :scarli-test-7
  (:use :cl :scarli))

(in-package :scarli-test-7)

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

(defparameter *paged-text* (make-instance 'paged-text
                                          :pages (list
                                                   (list "line one"
                                                         "line two")
                                                   (list "page two l1"
                                                         "page two l2"))))

(defparameter *multi* (make-instance 'multiline-text
                                     :x 0
                                     :y 100
                                     :lines (list
                                              "This is on line one"
                                              "This is on line two"
                                              "This is line three"
                                              "This is on line four"
                                              "This is on line five"
                                              "This is on line six")))

(add-input-handler *paged-text*)
(add-obj-to-scene *testscene* "bottom" *paged-text*)
(add-obj-to-scene *testscene* "bottom" *multi*)
(main *testscene* *camera* *width* *height*)
