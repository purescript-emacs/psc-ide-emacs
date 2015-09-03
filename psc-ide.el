(require 'company)
(require 'dash) ;; standard in emacs 25.x?
(require 'purescript-mode) ;; purescript-ident-at-point

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

;; TODO localise
(setq psc-ide-command-load     "load")
(setq psc-ide-command-cwd      "cwd")
(setq psc-ide-command-complete "completion")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactive.

(defun company-psc-ide-backend (command &optional arg &rest ignored)
  "The psc-ide backend for 'company-mode'."
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-sample-backend))

    (prefix (and (eq major-mode 'purescript-mode)
                 (company-grab-symbol)
                 ;; (purescript-ident-at-point)
                 ))

    (candidates (psc-ide-complete-impl arg))
))

(defun psc-ide-complete ()
  "Complete prefix string using psc-ide."
  (interactive)
   (psc-ide-complete-impl (purescript-ident-at-point))
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
  ;; (replace-regexp-in-string "[\"\n]" "" (psc-ide-send psc-ide-command-cwd))
  (psc-ide-cleanup-shell-output (psc-ide-send psc-ide-command-cwd))
)

(defun psc-ide-load-module (module-name)
  "Load PureScript module."
  (psc-ide-send (concat psc-ide-command-load " " module-name))
)

(defun psc-ide-complete-impl (prefix)
  "Complete."
  (let ((default-directory (psc-ide-ask-project-dir))
        )
    (split-string
     (psc-ide-cleanup-shell-output
       (psc-ide-send (concat psc-ide-command-complete " " prefix))
     )
    ", "))
  )

(add-to-list 'company-backends 'company-psc-ide-backend)

(defun psc-ide-cleanup-shell-output (str)
  (replace-regexp-in-string "[\"\n]" "" str)
)
