(require 'json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Protocol commands.

;; TODO localise
(setq psc-ide-command-cwd (json-encode (list :command "cwd")))

(defun psc-ide-command-load (modules deps)
  (json-encode
   (list :command "load"
         :params (list
                  :modules modules
                  :dependencies deps ))))

(defun psc-ide-command-show-type (filters search)
  (json-encode
   (list :command "type"
         :params (list
                  :filters filters
                  :search search ))))

(defun psc-ide-command-complete (filters matcher)
  (json-encode
   (list :command "complete"
         :params (list
                  :filters filters
                  :matcher matcher))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Protocol utilities.


(defun psc-ide-generic-filter (name params)
   (list :filter name
         :params params))

(defun psc-ide-filter-exact (filter-str)
  (psc-ide-generic-filter "exact" (list :search filter-str)))

(defun psc-ide-filter-prefix (prefix-str)
  (psc-ide-generic-filter "prefix" (list :search prefix-str)))

(defun psc-ide-filter-modules (modules-list)      ;; modules without dependencies
  (psc-ide-generic-filter "modules" (list :modules modules-list)))

(defun psc-ide-filter-dependencies (modules-list) ;; modules with dependencies
  (psc-ide-generic-filter "dependencies" (list :modules modules-list)))


(defun psc-ide-generic-matcher (name params)
   (list :matcher name
         :params params))

(defun psc-ide-matcher-flex (match-str)
  (psc-ide-generic-matcher "flex" (list :search match-str)))

(defun psc-ide-matcher-distance (match-str max-dist)
  (psc-ide-generic-matcher "distance" (list :search match-str
                                            :maxDist max-dist)))

(defun psc-ide-unwrap-result (res)
  "Unwraps the result from psc-ide and in case of an error throws it"
  (let ((result-type (cdr (assoc 'resultType res)))
        (result (cdr (assoc 'result res))))
    (if (string= result-type "error") (error "%s" result) result)))

(provide 'psc-ide-protocol)
