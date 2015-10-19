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


(defun generic-filter (name params)
   (list :filter name
         :params params))

(defun filter-exact (filter-str)
  (generic-filter "exact" (list :search filter-str)))

(defun filter-prefix (prefix-str)
  (generic-filter "prefix" (list :search prefix-str)))

(defun filter-modules (modules-list)      ;; modules without dependencies
  (generic-filter "modules" (list :modules modules-list)))

(defun filter-dependencies (modules-list) ;; modules with dependencies
  (generic-filter "dependencies" (list :modules modules-list)))


(defun generic-matcher (name params)
   (list :matcher name
         :params params))

(defun matcher-flex (match-str)
  (generic-matcher "flex" (list :search match-str)))

(defun matcher-distance (match-str max-dist)
  (generic-matcher "distance" (list :search match-str
                                    :maxDist max-dist)))

(defun unwrap-result (res)
  "Unwraps the result from psc-ide and in case of an error throws it"
  (let ((result-type (cdr (assoc 'resultType res)))
        (result (cdr (assoc 'result res))))
    (if (string= result-type "error") (error "%s" result) result)))

(provide 'psc-ide-protocol)
