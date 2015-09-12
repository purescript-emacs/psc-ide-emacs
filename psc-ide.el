;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Imports

(require 'company)
(require 'psc-ide-backported)
(require 'json)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; psc-ide-mode definition

(provide 'psc-ide)

;;;###autoload
(define-minor-mode psc-ide-mode
  "psc-ide-mode definition"
  :lighter " psc-ide"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-s") 'psc-ide-server)
            (define-key map (kbd "C-c C-l") 'psc-ide-load-module)
            (define-key map (kbd "C-<SPC>") 'company-complete)
            (define-key map (kbd "C-c C-t") 'psc-ide-show-type)
            map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Settings, override as needed.

(defgroup psc-ide nil
  "Settings for psc-ide."
  :prefix "psc-ide-"
  :group 'psc-ide)

(defcustom psc-ide-executable "psc-ide"
  "Path to the 'psc-ide' executable."
  :group 'psc-ide
  :type  'string)

(defcustom psc-ide-server-executable "psc-ide-server"
  "Path to the 'psc-ide-server' executable."
  :group 'psc-ide
  :type  'string)

(defcustom psc-ide-completion-matcher "flex"
  "The method used for completions."
  :options '("flex" "prefix")
  :group 'psc-ide
  :type  'string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactive.

(defun company-psc-ide-backend (command &optional arg &rest ignored)
  "The psc-ide backend for 'company-mode'."
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-psc-ide-backend))

    (prefix (and (eq major-mode 'purescript-mode)
                 (company-grab-symbol)
                 ;; (psc-ide-ident-at-point)
                 ))

    (candidates (psc-ide-complete-impl arg))

    (sorted t)

    (annotation (psc-ide-annotation arg))
    (meta (psc-ide-meta arg))
))

(defun psc-ide-server (dir-name)
  "Starts psc-ide-server"
  (interactive "DProject directory: ")
  (start-process "*psc-ide-server*" "*psc-ide-server*" psc-ide-server-executable "-d" dir-name)
)

(defun psc-ide-load-module (module-name)
  "Provide module to load"
  (interactive (list (read-string "Module: " (car (split-string (buffer-name) "\\.")))) )
  (psc-ide-load-module-impl module-name)
)

(defun psc-ide-complete ()
  "Complete prefix string using psc-ide."
  (interactive)
  (psc-ide-complete-impl (psc-ide-ident-at-point))
)

(defun psc-ide-show-type ()
  "Show type of the symbol under cursor"
  (interactive)
  (psc-ide-show-type-impl (psc-ide-ident-at-point))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commands.

;; TODO localise
(setq psc-ide-command-cwd (json-encode (list :command "cwd")))

(defun psc-ide-command-load (modules deps)
  (json-encode
   (list :command "load"
         :params (list
                  :modules modules
                  :dependencies deps )))
)

(defun psc-ide-command-show-type (filters search)
  (json-encode
   (list :command "type"
         :params (list
                  :filters filters
                  :search search )))
)

(defun psc-ide-command-complete (filters search)
  (json-encode
   (list :command "complete"
         :params (list
                  :filters filters
                  :matcher (list
                            :matcher psc-ide-completion-matcher
                            :params (list
                                     :search search
                                     )))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Non-interactive.

(defun psc-ide-send (cmd)
  "Send a command to psc-ide."
  (shell-command-to-string (concat "echo '" cmd "' | " psc-ide-executable))
)

(defun psc-ide-ask-project-dir ()
  "Ask psc-ide-server for the project dir."
  (psc-ide-send psc-ide-command-cwd)
)

(defun psc-ide-load-module-impl (module-name)
  "Load PureScript module and its dependencies."
  (psc-ide-send (psc-ide-command-load [] (list module-name)))
  )

(defun psc-ide-complete-impl (prefix)
  "Complete."
  (mapcar
   (lambda (x)
     (let ((completion (cdr (assoc 'identifier x)))
           (type (cdr (assoc 'type x)))
           (module (cdr (assoc 'module x))))
       (add-text-properties 0 1 (list :type type :module module) completion)
       completion
       ))
   
   (json-read-from-string
    (psc-ide-send (psc-ide-command-complete [] prefix))))
)

(defun psc-ide-show-type-impl (ident)
  "Show type."
  (let* ((resp (psc-ide-send (psc-ide-command-show-type [] ident)))
         (first-result (aref
                       (json-read-from-string
                        resp) 0)))

    (message (cdr (assoc 'type first-result))))
)

(defun psc-ide-annotation (s)
  (format " (%s)" (get-text-property 0 :module s))
)

(defun psc-ide-meta (s)
  (format "(%s)" (get-text-property 0 :type s))
)

(setq company-tooltip-align-annotations t)

(defun company-psc-ide-frontend (command)
  (case command
    (post-command (and (eq major-mode 'purescript-mode)
                       (message
                        (get-text-property 0 :type
                                           (nth company-selection company-candidates))))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utilities

(add-to-list 'company-backends 'company-psc-ide-backend)
(add-to-list 'company-frontends 'company-psc-ide-frontend)
