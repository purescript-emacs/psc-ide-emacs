;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Imports

(require 'company)
(require 'psc-ide-backported)

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

;; TODO localise
(setq psc-ide-command-load     "load")
(setq psc-ide-command-cwd      "cwd")
(setq psc-ide-command-complete "complete")
(setq psc-ide-command-show-type "typeLookup")

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
;; Non-interactive.

(defun psc-ide-send (cmd)
  "Send a command to psc-ide."
  (shell-command-to-string (concat "echo '" cmd "' | " psc-ide-executable))
)

(defun psc-ide-ask-project-dir ()
  "Ask psc-ide-server for the project dir."
  (psc-ide-cleanup-shell-output (psc-ide-send psc-ide-command-cwd))
)

(defun psc-ide-load-module-impl (module-name)
  "Load PureScript module."
  (psc-ide-send (concat psc-ide-command-load " " module-name))
)

(defun psc-ide-complete-impl (prefix)
  "Complete."
  (let ((default-directory (psc-ide-ask-project-dir))
        )
    (split-string
     (psc-ide-cleanup-shell-output
       (psc-ide-send (concat psc-ide-command-complete " " prefix " Project"))
     )
    ", "))
  )

(defun psc-ide-show-type-impl (ident)
  "Show type."
  (message (psc-ide-cleanup-shell-output (psc-ide-send (concat psc-ide-command-show-type " " ident))))
)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utilities


(defun psc-ide-cleanup-shell-output (str)
  (replace-regexp-in-string "[\"\n]" "" str)
)


;;(add-to-list 'company-backends 'company-psc-ide-backend)

