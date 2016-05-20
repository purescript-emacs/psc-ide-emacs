;;; psc-ide.el --- Minor mode for PureScript's psc-ide tool. -*- lexical-binding: t -*-

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
(require 'dash-functional)
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
            (define-key map (kbd "C-c C-q") 'psc-ide-server-quit)
            (define-key map (kbd "C-c C-l") 'psc-ide-load-all)
            (define-key map (kbd "C-c C-S-l") 'psc-ide-load-module)
            (define-key map (kbd "C-c C-a") 'psc-ide-add-clause)
            (define-key map (kbd "C-c C-c") 'psc-ide-case-split)
            (define-key map (kbd "C-c C-i") 'psc-ide-add-import)
            (define-key map (kbd "C-c C-t") 'psc-ide-show-type)
            (define-key map (kbd "C-c C-b") 'psc-ide-rebuild)
            map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Settings, override as needed.

(defgroup psc-ide nil
  "Settings for psc-ide."
  :prefix "psc-ide-"
  :group 'psc-ide)

(defcustom psc-ide-client-executable "psc-ide-client"
  "Path to the 'psc-ide' executable."
  :group 'psc-ide
  :type  'string)

(defcustom psc-ide-server-executable "psc-ide-server"
  "Path to the 'psc-ide-server' executable."
  :group 'psc-ide
  :type  'string)

(defcustom psc-ide-port 4242
  "The port that psc-ide-server and the client use."
  :group 'psc-ide
  :type  'integer)

(defcustom psc-ide-debug nil
  "Whether psc-ide-server should be started with the debug flag"
  :group 'psc-ide
  :type  'boolean)

(defcustom psc-ide-completion-matcher "flex"
  "The method used for completions."
  :options '("flex" "prefix")
  :group 'psc-ide
  :type  'string)

(defcustom psc-ide-add-import-on-completion "t"
  "Whether to add imports on completion"
  :group 'psc-ide
  :type 'boolean)

(defcustom psc-ide-rebuild-on-save nil
  "Whether to rebuild files on save and display errors/warnings
in a buffer"
  :group 'psc-ide
  :type 'boolean)

(defconst psc-ide-import-regex
  (rx (and line-start "import" (1+ space) (opt (and "qualified" (1+ space)))
           (group (and (1+ (any word "."))))
           (opt (1+ space) "as" (1+ space) (group (and (1+ word))))
           (opt (1+ space) "(" (group (0+ not-newline)) ")"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactive.

(add-hook 'after-save-hook
          (lambda ()
            (set 'psc-ide-buffer-import-list
                 (psc-ide-parse-imports-in-buffer)))
          nil t)

(defun psc-ide-rebuild-on-save-hook()
  "Rebuilds the current module on safe"
  (when (eq major-mode 'purescript-mode)
    (psc-ide-rebuild)))

(when psc-ide-rebuild-on-save
  (add-hook 'after-save-hook 'psc-ide-rebuild-on-save-hook))

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

    (prefix (when (and (eq major-mode 'purescript-mode)
                       (not (company-in-string-or-comment)))
              ;; (psc-ide-ident-at-point)
              (let ((symbol (company-grab-symbol)))
                (if symbol
                    (if (psc-ide-qualified-p symbol)
                        (progn
                          (cons (car (last (s-split "\\." symbol))) t))
                      symbol)
                  'stop))))

    ;; (candidates (psc-ide-complete-impl arg company--manual-action))
    (candidates (psc-ide-company-fetcher arg))

    (sorted t)

    (annotation (psc-ide-annotation arg))

    (meta (get-text-property 0 :type arg))

    (post-completion
     (unless (or
              ;; Don't add an import when the option to do so is disabled
              (not psc-ide-add-import-on-completion)
              ;; or when a qualified identifier was completed
              (get-text-property 0 :qualifier arg))
       (psc-ide-add-import-impl arg (vector
                                     (psc-ide-filter-modules
                                      (list (get-text-property 0 :module arg)))))))))

(defun psc-ide-server-start (dir-name)
  "Start 'psc-ide-server'."
  (interactive (list (read-directory-name "Project root? "
                                          (psc-ide-suggest-project-dir))))
  (psc-ide-server-start-impl dir-name))

(defun psc-ide-server-quit ()
  "Quit 'psc-ide-server'."
  (interactive)
  (psc-ide-send-sync psc-ide-command-quit))

(defun psc-ide-load-module (module-name)
  "Provide module to load"
  (interactive (list (read-string "Module: " (psc-ide-get-module-name))))
  (psc-ide-load-module-impl module-name))

(defun psc-ide-load-all ()
  "Loads all the modules in the current project"
  (interactive)
  (psc-ide-send psc-ide-command-load-all (-const "yay")))

(defun psc-ide-show-type ()
  "Show type of the symbol under cursor."
  (interactive)
  (let ((ident (psc-ide-ident-at-point)))
    (-if-let (type-description (psc-ide-show-type-impl ident))
        (message type-description)
      (message (concat "Know nothing about type of `%s'. "
                       "Have you loaded the corresponding module?")
               ident))))

(defun psc-ide-case-split (type)
  "Case Split on identifier under cursor."
  (interactive "sType: ")
  (let ((new-lines (psc-ide-case-split-impl type)))
    (beginning-of-line) (kill-line) ;; clears the current line
    (insert (mapconcat 'identity new-lines "\n"))))

(defun psc-ide-add-clause ()
  "Add clause on identifier under cursor."
  (interactive)
  (let ((new-lines (psc-ide-add-clause-impl)))
    (beginning-of-line) (kill-line) ;; clears the current line
    (insert (mapconcat 'identity new-lines "\n"))))

(defun psc-ide-add-import ()
  "Add an import for the symbol under the cursor."
  (interactive)
  (psc-ide-add-import-impl (psc-ide-ident-at-point)))

(defun psc-ide-rebuild ()
  "Rebuild the current module"
  (interactive)
  (let* ((res (psc-ide-send-sync (psc-ide-command-rebuild)))
         (is-success (string= "success" (cdr (assoc 'resultType res))))
         (result (cdr (assoc 'result res))))

    (if (not is-success)
        (let* ((first-error (aref result 0)))
          (psc-ide-display-rebuild-error (psc-ide-pretty-json-error first-error))))
    (if (<= (length result) 0)
        (progn
          (delete-windows-on (get-buffer-create "*psc-ide-rebuild*"))
          (message "OK"))
      (let* ((first-warning (aref result 0)))
        (psc-ide-display-rebuild-error (psc-ide-pretty-json-error first-warning))))))

(defun psc-ide-display-rebuild-error (err)
  (with-current-buffer (get-buffer-create "*psc-ide-rebuild*")
    (compilation-mode)
    (read-only-mode -1)
    (erase-buffer)
    (insert err)
    (read-only-mode 1))
  (display-buffer "*psc-ide-rebuild*")
  (set-window-point (get-buffer-window "*psc-ide-rebuild*") (point-min)))

(defun psc-ide-pretty-json-error (first-error)
  (let ((err-message (cdr (assoc 'message first-error)))
        (err-filename (cdr (assoc 'filename first-error)))
        (err-position (cdr (assoc 'position first-error))))
    (if (not err-position)
        err-message
      (let ((err-column (cdr (assoc 'startColumn err-position)))
            (err-line (cdr (assoc 'startLine err-position))))
        (concat err-filename ":" (number-to-string err-line) ":" (number-to-string err-column) ":" "\n" err-message)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Non-interactive.

(defun psc-ide-case-split-impl (type)
  "Case Split on identifier under cursor"
  (let ((reg (psc-ide-ident-pos-at-point)))
    (psc-ide-unwrap-result (psc-ide-send-sync
                            (psc-ide-command-case-split
                             (substring (thing-at-point 'line t) 0 -1)
                             (save-excursion (goto-char (car reg)) (current-column))
                             (save-excursion (goto-char (cdr reg)) (current-column))
                             type)))))

(defun psc-ide-add-clause-impl ()
  "Add clause on identifier under cursor"
  (let ((reg (psc-ide-ident-pos-at-point)))
    (psc-ide-unwrap-result (psc-ide-send-sync (psc-ide-command-add-clause
                                               (substring (thing-at-point 'line t) 0 -1) nil)))))

(defun psc-ide-get-module-name ()
  "Return the qualified name of the module in the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "module +\\([A-Z][A-Za-z0-9.]*\\)" nil t)
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

(defun psc-ide-parse-exposed (exposed)
  "Parsed the EXPOSED names from a qualified import."
  (if exposed
      (mapcar (lambda (item)
                (s-trim item))
              (s-split "," exposed))
    nil))

(defun psc-ide-extract-import-from-match-data (&optional string)

  "Helper function for use when using the `psc-ide-import-regex' to match
imports to extract the relevant info from the groups.  STRING is for
use when the search used was with `string-match'."

  (let* ((data (match-data))
         (len (length data))
         (idx 3)
         result)
    (push `(module . ,(match-string-no-properties 1 string)) result)
    (push `(alias . ,(match-string-no-properties 2 string)) result)
    (push `(exposing . ,(psc-ide-parse-exposed (match-string-no-properties 3 string))) result)
    result))

(defun psc-ide-parse-imports-in-buffer (&optional buffer)

  "Parse the list of imports for the current purescript BUFFER."

  (let ((module-name (psc-ide-get-module-name))
        (matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char (point-min))
            (when module-name
              (push `((module . ,module-name)) matches))
            (while (search-forward-regexp psc-ide-import-regex nil t 1)
              (push (psc-ide-extract-import-from-match-data) matches))))))
    matches))

(defun psc-ide-send-sync (cmd)
  (with-temp-buffer
    (condition-case nil
        (let ((proc (make-network-process
                     :name "psc-ide-server"
                     :buffer (buffer-name (current-buffer))
                     :family 'ipv4
                     :host "localhost"
                     :service psc-ide-port)))
          (process-send-string proc (s-prepend cmd "\n"))
          ;; Wait for the process in a blocking manner for a maximum of 2
          ;; seconds
          (accept-process-output proc 2)
          (json-read-from-string (-first-item (s-lines (buffer-string)))))
      (error
       (error
        (s-join " "
                '("It seems like the server is not running. You can"
                  "start it using psc-ide-server-start.")))))))

(defun psc-ide-send (cmd callback)
  (condition-case err
      (let* ((buffer (generate-new-buffer "*psc-ide-client*"))
             (proc (make-network-process
                    :name "psc-ide-server"
                    :buffer buffer
                    :family 'ipv4
                    :host "localhost"
                    :service psc-ide-port
                    :sentinel (-partial 'wrap-psc-ide-callback callback buffer))))
        (process-send-string proc (s-prepend cmd "\n")))
    ;; Catch all the errors that happen when trying to connect
    (error
     (error
      (s-join " "
              '("It seems like the server is not running. You can"
                "start it using psc-ide-server-start."))))))

(defun wrap-psc-ide-callback (callback buffer proc status)
  "Wraps a function that expects a parsed psc-ide response"
  (when (string= "closed" (process-status proc))
    (let ((parsed
           (with-current-buffer buffer
             (json-read-from-string
              (buffer-substring (point-min) (point-max))))))
        (kill-buffer buffer)
        (funcall callback parsed))))

(defun psc-ide-ask-project-dir ()
  "Ask psc-ide-server for the project dir."
  (interactive)
  (psc-ide-send psc-ide-command-cwd
                (-compose 'message 'psc-ide-unwrap-result)))

(defun psc-ide-server-start-impl (dir-name)
  "Start psc-ide-server."
  (apply 'start-process `("*psc-ide-server*" "*psc-ide-server*"
                          ,@(psc-ide-server-command dir-name))))

(defun psc-ide-server-command (dir-name)
  "Tries to find the psc-ide-server-executable and builds up the
  command by appending eventual options. Returns a list that can
  be expanded and passed to start-process"
  (let ((path (executable-find psc-ide-server-executable))
        (port (number-to-string psc-ide-port))
        (directory (expand-file-name dir-name))
        (debug-flag (when psc-ide-debug "--debug")))
    (if path
        (remove nil `(,path "-p" ,port "-d" ,directory ,debug-flag))
      (error (s-join " " '("Couldn't locate the psc-ide-server executable. You"
                           "could either customize the psc-ide-server-executable"
                           "setting, or put the executable on your path."))))))

(defun psc-ide-load-module-impl (module-name)
  "Load PureScript module and its dependencies."
  (psc-ide-unwrap-result
   (psc-ide-send-sync (psc-ide-command-load
                       [] (list module-name)))))

(defun psc-ide-add-import-impl (identifier &optional filters)
  "Invoke the addImport command"
  (let* ((tmp-file (make-temp-file "psc-ide-add-import"))
         (filename (buffer-file-name (current-buffer)))
         (result (progn
                   (write-region (point-min) (point-max) tmp-file)
                   (psc-ide-unwrap-result
                    (psc-ide-send-sync (psc-ide-command-add-import identifier filters tmp-file tmp-file))))))
    (if (not (stringp result))
        (let ((selection
               (completing-read "Which Module to import from: "
                                (-map (lambda (x)
                                        (cdr (assoc 'module x))) result))))
          (psc-ide-add-import-impl identifier (vector (psc-ide-filter-modules (vector selection)))))
      (progn (message (concat "Added import for " identifier))
             (save-restriction
               (widen)
               ;; command successful, insert file with replacement to preserve
               ;; markers.
               (insert-file-contents tmp-file nil nil nil t))))
    (delete-file tmp-file)))

(defun psc-ide-filter-bare-imports (imports)
  "Filter out all alias imports."
  (->> imports
       (-filter (lambda (import)
                  (and
                   ;;(not (cdr (assoc 'exposing import)))
                   (not (cdr (assoc 'alias import))))))
       (-map (lambda (import)
               (cdr (assoc 'module import))))))


(defun psc-ide-filter-imports-by-alias (imports alias)
  "Filters the IMPORTS by ALIAS.  If nothing is found then just return ALIAS
unchanged."
  (let ((result (->> imports
                     (-filter (lambda (import)
                                (equal (cdr (assoc 'alias import))
                                       alias)))
                     (-map (lambda (import)
                             (cdr (assoc 'module import)))))))
    (if result
        result
      (list alias))))

(defun psc-ide-find-import (imports name)
  (-find (lambda (import)
           (equal (assoc 'module import) name))
         imports))

(defun psc-ide-qualified-p (name)
  (s-contains-p "." name))

(defun psc-ide-get-ident-context (prefix imports)
  "Split the prefix into the qualifier and search term from PREFIX.
Returns an plist with the search, qualifier, and relevant modules."
  (let* ((components (s-split "\\." prefix))
         (search (car (last components)))
         (qualifier (s-join "." (butlast components))))
    (if (equal "" qualifier)
        (list 'search search 'qualifier nil 'modules (psc-ide-filter-bare-imports imports))
      (list 'search search 'qualifier qualifier 'modules (psc-ide-filter-imports-by-alias imports qualifier)))))


(defun psc-ide-make-module-filter (type modules)
  (list :filter type
        :params (list :modules modules)))

(defun psc-ide-filter-results-p (imports search qualifier result)
  (let ((completion (cdr (assoc 'identifier result)))
        (type (cdr (assoc 'type result)))
        (module (cdr (assoc 'module result))))
    (if qualifier
        t ;; return all results from qualified imports
      (-find (lambda (import)
               ;; return t for explicit imported names and open imports
               (if (and
                    (equal module (cdr (assoc 'module import)))
                    (not (cdr (assoc 'alias import)))
                    (or (not (cdr (assoc 'exposing import)))
                        (-contains? (cdr (assoc 'exposing import)) completion)))
                   t
                 nil))
             imports))))

(defun psc-ide-complete-impl (prefix &optional nofilter)
  "Complete."
  (when psc-ide-buffer-import-list
    (let* ((pprefix (psc-ide-get-ident-context
                     (company-grab-symbol)
                     psc-ide-buffer-import-list))
           (search (plist-get pprefix 'search))
           (qualifier (plist-get pprefix 'qualifier))
           (moduleFilters (plist-get pprefix 'modules))
           (annotate (lambda (type module qualifier str)
                       (add-text-properties 0 1 (list :type type
                                                      :module module
                                                      :qualifier qualifier) str)
                       str))
           (prefilter (psc-ide-filter-prefix prefix))
           (filters (-non-nil (list (psc-ide-make-module-filter "modules" moduleFilters) prefilter)))
           (result (psc-ide-unwrap-result
                    (psc-ide-send-sync
                     (psc-ide-command-complete
                      (if nofilter
                          (vector prefilter) ;; (vconcat nil) = []
                        (vconcat filters))
                      nil (psc-ide-get-module-name))))))
      (->> result
           (remove-if-not
            (lambda (x)
              (or nofilter (psc-ide-filter-results-p psc-ide-buffer-import-list search qualifier x))))

           (mapcar
            (lambda (x)
              (let ((completion (cdr (assoc 'identifier x)))
                    (type (cdr (assoc 'type x)))
                    (module (cdr (assoc 'module x))))
                (funcall annotate type module qualifier completion))))))))

(defun psc-ide-company-fetcher (prefix)
  `(:async . ,(-partial 'psc-ide-complete-async prefix)))

(defun psc-ide-complete-async (prefix callback)
  (let ((command (psc-ide-command-complete
                  (vector (psc-ide-filter-prefix prefix))))
        (handler (-partial 'psc-ide-handle-completionresponse callback)))
    (psc-ide-send command handler)))

(defun psc-ide-handle-completionresponse (callback response)
  "Accepts a callback and a completion response from psc-ide,
processes the response into a format suitable for company and
passes it into the callback"
  (let* ((result (psc-ide-unwrap-result response))
         (completions (-map 'psc-ide-annotate-completion result)))
    (funcall callback completions)))

(defun psc-ide-annotate-completion (completion)
  "Turns a completion from psc-ide into a string with
  text-properties, which carry additional information"
  (let ((identifier (cdr (assoc 'identifier completion)))
        (type (cdr (assoc 'type completion)))
        (module (cdr (assoc 'module completion))))
    ;; :qualifier qualifier <- TODO: Add this back in
    (add-text-properties 0 1 (list :type type
                                   :module module) identifier)
    ;; add-text-properties is sideeffecting and doesn't return the modified
    ;; string, so we need to explicitly return the identifier from here
    identifier))

(defun psc-ide-show-type-impl (ident)
  "Returns a string that describes the type of IDENT.
Returns NIL if the type of IDENT is not found."

  (let* ((pprefix (psc-ide-get-ident-context
                   ident
                   psc-ide-buffer-import-list))
         (search (plist-get pprefix 'search))
         (qualifier (plist-get pprefix 'qualifier))
         (moduleFilters (plist-get pprefix 'modules))
         (resp (psc-ide-send-sync
                (psc-ide-command-show-type
                 (vector (psc-ide-make-module-filter "modules" moduleFilters))
                 search)))
         (result (psc-ide-unwrap-result resp)))
    (when (not (zerop (length result)))
      (cdr (assoc 'type (aref result 0))))))

(defun psc-ide-annotation (s)
  (format " (%s)" (get-text-property 0 :module s)))

(defun psc-ide-suggest-project-dir ()
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
    (file-name-directory (buffer-file-name))))

(setq company-tooltip-align-annotations t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utilities

(add-to-list 'company-backends 'company-psc-ide-backend)

(provide 'psc-ide)

;;; psc-ide.el ends here
