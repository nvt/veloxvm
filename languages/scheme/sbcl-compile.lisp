(load "compiler-cli.lisp")

(let ((executable-file (make-pathname :name "compiler")))
  (when (probe-file executable-file)
    (delete-file executable-file))
  (sb-ext:save-lisp-and-die executable-file :executable t :toplevel 'main))
