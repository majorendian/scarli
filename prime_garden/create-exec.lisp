(defun main-function ()
  (load "main.lisp"))
#+:linux (sb-ext:save-lisp-and-die "prime_garden" :toplevel 'main-function :executable t)
#-:linux (sb-ext:save-lisp-and-die "prime_garden.exe" :toplevel 'main-function :executable t :application-type :gui)
