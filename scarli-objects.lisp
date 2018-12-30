;(require :scarli)

(defpackage scarli-objects
  (:use :cl :scarli)
  (:export interactible
           interactible-pages))

(in-package :scarli-objects)

(defclass interactible (solid-tile)
  ((pages :accessor interactible-pages :initarg :pages :initform (list (list "Placeholder")))))
