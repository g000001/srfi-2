;;;; srfi-2.asd

(cl:in-package :asdf)

(defsystem :srfi-2
  :serial t
  :depends-on (:srfi-23)
  :components ((:file "package")
               (:file "srfi-2")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-2))))
  (load-system :srfi-2)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-2-internal :srfi-2))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

