;;;; srfi-2.asd

(cl:in-package :asdf)

(defsystem :srfi-2
  :version "1"
  :description "SRFI 2: AND-LET*: an AND with local bindings"
  :long-description "SRFI 2: AND-LET*: an AND with local bindings
https://srfi.schemers.org/srfi-2"
  :author "Oleg Kiselyov"
  :maintainer "CHIBA Masaomi"
  :license "Unlicense"
  :serial t
  :depends-on ()
  :components ((:file "package")
               (:file "srfi-2")))

(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-2))))
  (let ((name "https://github.com/g000001/srfi-2")
        (nickname :srfi-2))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))

(defsystem :srfi-2.test
  :version "1"
  :description "SRFI 2: AND-LET*: an AND with local bindings"
  :long-description "SRFI 2: AND-LET*: an AND with local bindings
https://srfi.schemers.org/srfi-2"
  :author "Oleg Kiselyov"
  :maintainer "CHIBA Masaomi"
  :license "Unlicense"
  :serial t
  :depends-on (:srfi-23 :fiveam)
  :components ((:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-2.test))))
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
        (let ((result (funcall (_ :fiveam :run)
                               (_ "https://github.com/g000001/srfi-2#internals" :srfi-2))))
          (funcall (_ :fiveam :explain!) result)
          (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
