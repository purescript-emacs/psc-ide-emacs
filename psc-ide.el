;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Imports

(require 'company)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Imported from purescript-mode

(defun psc-ide-ident-at-point ()
  "Return the identifier under point, or nil if none found.
May return a qualified name."
  (let ((reg (psc-ide-ident-pos-at-point)))
    (when reg
      (buffer-substring-no-properties (car reg) (cdr reg)))))

(defun psc-ide-ident-pos-at-point ()
  "Return the span of the identifier under point, or nil if none found.
May return a qualified name."
  (save-excursion
    ;; Skip whitespace if we're on it.  That way, if we're at "map ", we'll
    ;; see the word "map".
    (if (and (not (eobp))
             (eq ?  (char-syntax (char-after))))
        (skip-chars-backward " \t"))

    (let ((case-fold-search nil))
      (multiple-value-bind (start end)
          (if (looking-at "\\s_")
              (list (progn (skip-syntax-backward "_") (point))
                    (progn (skip-syntax-forward "_") (point)))
            (list
             (progn (skip-syntax-backward "w'")
                    (skip-syntax-forward "'") (point))
             (progn (skip-syntax-forward "w'") (point))))
        ;; If we're looking at a module ID that qualifies further IDs, add
        ;; those IDs.
        (goto-char start)
        (while (and (looking-at "[[:upper:]]") (eq (char-after end) ?.)
                    ;; It's a module ID that qualifies further IDs.
                    (goto-char (1+ end))
                    (save-excursion
                      (when (not (zerop (skip-syntax-forward
                                         (if (looking-at "\\s_") "_" "w'"))))
                        (setq end (point))))))
        ;; If we're looking at an ID that's itself qualified by previous
        ;; module IDs, add those too.
        (goto-char start)
        (if (eq (char-after) ?.) (forward-char 1)) ;Special case for "."
        (while (and (eq (char-before) ?.)
                    (progn (forward-char -1)
                           (not (zerop (skip-syntax-backward "w'"))))
                    (skip-syntax-forward "'")
                    (looking-at "[[:upper:]]"))
          (setq start (point)))
        ;; This is it.
        (cons start end)))))
