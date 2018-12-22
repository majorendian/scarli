(require :scarli)

(defpackage scarli-test-4
  (:use :cl :scarli))

(in-package scarli-test-4)

(defparameter *width* 640)
(defparameter *height* 480)

(defparameter *prog-text*
  (make-instance 'progressive-text
                 :x 0
                 :y 0
                 :text "This text appears gradually."))

(defparameter *dummy-scene* (make-instance 'scene :layers (list (make-instance 'layer :name "one"))))
(add-input-handler *prog-text*)
(add-obj-to-scene *persistent-scene* "top" *prog-text*)
(main *dummy-scene* *width* *height*)


