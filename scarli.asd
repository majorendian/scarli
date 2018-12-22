(defpackage scarli-system
  (:use :cl :asdf))

(in-package :scarli-system)

(defsystem "scarli"
  :description "Simple SDL2 2D Game engine"
  :depends-on (:sdl2 :sdl2-ttf :sdl2-mixer :sdl2-image)
  :version "0.1"
  :author "Ernest Deák"
  :license "MIT License"
  :components ((:file "scarli")))
