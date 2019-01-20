#!/usr/bin/env bash

sbcl --eval "(progn (require :scarli) (sb-ext:save-lisp-and-die \"scarli-lisp.core\"))"
sbcl --core scarli-lisp.core --load create-exec.lisp
rm scarli-lisp.core
