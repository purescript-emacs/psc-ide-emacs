;;; psc-ide.el --- Minor mode for PureScript's psc-ide tool.

;; Copyright (C) 2015 The psc-ide-emacs authors

;; Author   : Erik Post <erik@shinsetsu.nl>
;;            Dmitry Bushenko <d.bushenko@gmail.com>
;;            Christoph Hegemann
;; Homepage : https://github.com/epost/psc-ide-emacs
;; Version  : 0.1.0
;; Package-Requires: ((dash "2.11.0") (company "0.8.7") (cl-lib "0.5") (s "1.10.0"))
;; Keywords : languages

;;; Commentary:

;; Emacs integration for PureScript's psc-ide tool

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Imports

(require 'company)
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'psc-ide-backported)
(require 'psc-ide-protocol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; psc-ide-mode definition

;;;###autoload
(define-minor-mode psc-ide-mode
  "psc-ide-mode definition"
  :lighter " psc-ide"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-s") 'psc-ide-server-start)
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

(defconst psc-ide-import-regex
  (rx (and line-start "import" (1+ space) (opt (and "qualified" (1+ space)))
        (group (and (1+ (any word "."))))
        (opt (1+ space) "as" (1+ space) (group (and (1+ word))))
        (opt (1+ space) "(" (group (0+ not-newline)) ")"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactive.

(defun psc-ide-init ()
  (interactive)

  (set (make-local-variable 'psc-ide-buffer-import-list)
       (psc-ide-parse-imports-in-buffer)))


(defun company-psc-ide-backend (command &optional arg &rest ignored)
  "The psc-ide backend for 'company-mode'."
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-psc-ide-backend))

    (init (psc-ide-init))

    (prefix (and (eq major-mode 'purescript-mode)
                 (company-grab-symbol)
                 ;; (psc-ide-ident-at-point)
                 ))

    (candidates (psc-ide-complete-impl arg))

    (sorted t)

    (annotation (psc-ide-annotation arg))))

(defun psc-ide-server-start (dir-name)
  "Start 'psc-ide-server'."
  (interactive (list (read-directory-name "Project root? "
                                          (psc-ide-suggest-project-dir))))
  (psc-ide-server-start-impl dir-name))

(defun psc-ide-load-module (module-name)
  "Provide module to load"
  (interactive (list (read-string "Module: " (car (split-string (buffer-name) "\\.")))) )
  (psc-ide-load-module-impl module-name))

(defun psc-ide-complete ()
  "Complete prefix string using psc-ide."
  (interactive)
  (psc-ide-complete-impl (psc-ide-ident-at-point)))

(defun psc-ide-show-type ()
  "Show type of the symbol under cursor."
  (interactive)
  (let ((ident (psc-ide-ident-at-point)))
    (-if-let (type-description (psc-ide-show-type-impl ident))
        (message type-description)
      (message (concat "Know nothing about type of `%s'. "
                       "Have you loaded the corresponding module?")
               ident))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Non-interactive.

(defun psc-ide-parse-exposed (exposed)
  "Parsed the EXPOSED names from a qualified import."
  (if exposed
      (mapcar (lambda (item)
                (s-trim item))
              (s-split "," exposed))
    nil))

(defun psc-ide-extract-import-from-match-data (&optional string)

  "Helper function for use when using the psc-ide-import-regex to match imports to extract
the relevant info from the groups. STRING is for use when the search used was with `string-match'."

  (let* ((data (match-data))
         (len (length data))
         (idx 3)
         result)
    (push `(module . ,(match-string-no-properties 1 string)) result)
    (push `(alias . ,(match-string-no-properties 2 string)) result)
    (push `(exposing . ,(psc-ide-parse-exposed (match-string-no-properties 3 string))) result)
    result))

(defun psc-ide-parse-imports-in-buffer (&optional buffer)

  "Parse the list of imports for the current elm BUFFER."

  (let (matches)
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char 1)
            (while (search-forward-regexp psc-ide-import-regex nil t 1)
              (push (psc-ide-extract-import-from-match-data) matches))))))
    matches))

;; TODO - figure out how to make this cross platform
;; On windows, the shell quote characters are sent to the psc-ide stdin and the json is not valid
(defconst debug-psc-ide-send-command t)

(defun psc-ide-send (cmd)
  "Send a command to psc-ide."
  (let* ((cmd2 (format "echo %s | %s"
                       cmd
                       psc-ide-executable))
         (resp (shell-command-to-string cmd2)))
    (when debug-psc-ide-send-command
        (message (format "Sent cmd: %s" cmd2 cmd))
        ;;(message (format "Got response: %s" resp))
        )
    resp))

(defun psc-ide-ask-project-dir ()
  "Ask psc-ide-server for the project dir."
  (psc-ide-send psc-ide-command-cwd))

(defun psc-ide-server-start-impl (dir-name)
  "Start psc-ide-server."
  (apply #'start-process `("*psc-ide-server*" "*psc-ide-server*"
                           ,@(split-string psc-ide-server-executable)
                           "-d" ,dir-name)))

(defun psc-ide-load-module-impl (module-name)
  "Load PureScript module and its dependencies."
  (psc-ide-unwrap-result (json-read-from-string
                          (psc-ide-send (psc-ide-command-load
                                         [] (list module-name))))))

(defun psc-ide-filter-bare-imports (imports)
  "Filter out all alias or explicit imports."
  (->> imports
       (-filter (lambda (import)
                  (and
                   (not (cdr (assoc 'alias import)))
                   (not (cdr (assoc 'exposing import))))))
       (-map (lambda (import)
               (cdr (assoc 'module import))))))


(defun psc-ide-filter-imports-by-alias (imports alias)
  "Filters the imports by alias."
  (let ((result (->> imports
                     (-filter (lambda (import)
                                (equal (cdr (assoc 'alias import)) alias)))
                     (-map (lambda (import)
                             (cdr (assoc 'module import)))))))
    (if result
        result
      (list alias))))


(defun psc-ide-get-completion-settings (prefix imports)
  "Split the prefix into the alias and search term from PREFIX.
Returns a cons cell with the search term minus any suffix and a list of modules to search."
  (let* ((components (s-split "\\." prefix))
         (search (car (last components)))
         (qualifier (s-join "." (butlast components))))
    (if (equal "" qualifier)
        (cons (cons search qualifier) (psc-ide-filter-bare-imports imports))
      (cons (cons search qualifier) (psc-ide-filter-imports-by-alias imports qualifier)))))


(defun psc-ide-make-module-filter (type modules)
  (list :filter type
        :params (list :modules modules)))


(defun psc-ide-complete-impl (prefix)
  "Complete."
  (let* ((pprefix (psc-ide-get-completion-settings prefix psc-ide-buffer-import-list))
         (search (caar pprefix))
         (qualifier (cdar pprefix))
         (filters (cdr pprefix)))

    (mapcar
     (lambda (x)
       (let ((completion (s-join "." (list qualifier (cdr (assoc 'identifier x)))))
             (type (cdr (assoc 'type x)))
             (module (cdr (assoc 'module x))))
         (add-text-properties 0 1 (list :type type :module module) completion)
         completion))

     (psc-ide-unwrap-result
      (json-read-from-string
       (psc-ide-send (psc-ide-command-complete
                      (vector (psc-ide-make-module-filter "modules" filters))
                      (psc-ide-matcher-flex search))))))))

(defun psc-ide-show-type-impl (ident)
  "Returns a string that describes the type of IDENT.
Returns NIL if the type of IDENT is not found."
  (let* ((resp (psc-ide-send (psc-ide-command-show-type [] ident)))
         (result (psc-ide-unwrap-result (json-read-from-string
                                         resp))))
    (when (not (zerop (length result)))
      (cdr (assoc 'type (aref result 0))))))

(defun psc-ide-annotation (s)
  (format " (%s)" (get-text-property 0 :module s)))

(defun psc-ide-suggest-project-dir ()
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
      (file-name-directory (buffer-file-name))))

(setq company-tooltip-align-annotations t)

(defun company-psc-ide-frontend (command)
  (cl-case command
    (post-command (and (eq major-mode 'purescript-mode)
                       (message
                        (get-text-property 0 :type
                                           (nth company-selection company-candidates)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utilities

(add-to-list 'company-backends 'company-psc-ide-backend)
(add-to-list 'company-frontends 'company-psc-ide-frontend)

(provide 'psc-ide)

;;; psc-ide.el ends here
