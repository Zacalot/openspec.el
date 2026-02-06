;;; openspec-core.el --- Core utilities for OpenSpec -*- lexical-binding: t; -*-

;;; Commentary:

;; CLI integration, project detection, path helpers, data fetching,
;; and shared utilities for the OpenSpec Emacs package.

;;; Code:

(require 'cl-lib)
(require 'json)

;; Variables defined in openspec.el — declare to silence byte-compiler
(defvar openspec-executable)
(defvar openspec-minimum-version)

;;; Internal Variables

(defvar-local openspec--project-root nil
  "Root directory of the current OpenSpec project.")

(defvar-local openspec--active-changes nil
  "Cached list of active (non-complete) changes for the current buffer.")

(defvar-local openspec--completed-changes nil
  "Cached list of completed changes for the current buffer.")

(defvar-local openspec--specs nil
  "Cached list of specs for the current buffer.")

(defvar-local openspec--validation-cache nil
  "Hash table caching validation results per change name.
Values are symbols: `passed', `failed', or nil for not validated.")

;;; Hooks

(defvar openspec-refresh-hook nil
  "Hook run after refreshing the status buffer.
Functions in this hook are called with no arguments, in the context
of the openspec-mode buffer.")

(defvar openspec-archive-hook nil
  "Hook run after successfully archiving a change.
Functions in this hook are called with the change name as the single argument.")

;;; CLI Integration

(defun openspec--cli-available-p ()
  "Return non-nil if the openspec CLI is available."
  (executable-find openspec-executable))

(defun openspec--version ()
  "Return the version string of the openspec CLI, or nil if unavailable."
  (when (openspec--cli-available-p)
    (let ((output (shell-command-to-string
                   (format "%s --version" openspec-executable))))
      (when (string-match "\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" output)
        (match-string 1 output)))))

(defun openspec--version-ok-p ()
  "Return non-nil if the CLI version meets minimum requirements."
  (let ((version (openspec--version)))
    (and version
         (version<= openspec-minimum-version version))))

(defun openspec--run-command-sync (args)
  "Run openspec with ARGS synchronously.
Returns a cons of (exit-code . output)."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process openspec-executable nil t nil args)))
      (cons exit-code (buffer-string)))))

(defun openspec--run-json-command-sync (args)
  "Run openspec with ARGS synchronously, parsing JSON output.
Returns the parsed JSON or nil on error."
  (let ((result (openspec--run-command-sync args)))
    (when (= (car result) 0)
      (condition-case nil
          (json-parse-string (cdr result)
                             :object-type 'alist
                             :array-type 'list)
        (error nil)))))

(defun openspec--strip-ansi-codes (string)
  "Remove ANSI escape codes from STRING."
  (replace-regexp-in-string "\e\\[[0-9;?]*[a-zA-Z]" "" string))

;;; Project Detection

(defun openspec--find-root (&optional dir)
  "Find the OpenSpec project root starting from DIR.
Searches upward for an `openspec/' directory."
  (let ((start (or dir default-directory)))
    (locate-dominating-file start "openspec")))

(declare-function project-roots "project" (project))
(declare-function projectile-project-root "projectile" ())

(defun openspec--project-root ()
  "Return the OpenSpec project root for the current context.
Uses cached value if in an openspec-mode buffer."
  (or openspec--project-root
      (openspec--find-root)
      (when (fboundp 'project-current)
        (when-let* ((proj (project-current)))
          (if (fboundp 'project-root)
              (project-root proj)
            (car (project-roots proj)))))
      (when (fboundp 'projectile-project-root)
        (projectile-project-root))
      default-directory))

(defun openspec--initialized-p (&optional root)
  "Return non-nil if OpenSpec is initialized at ROOT."
  (let ((root (or root (openspec--project-root))))
    (and root
         (file-directory-p (expand-file-name "openspec" root)))))

;;; Path Helpers

(defun openspec--openspec-dir (&optional root)
  "Return the openspec directory path for ROOT."
  (expand-file-name "openspec" (or root (openspec--project-root))))

(defun openspec--change-dir (name &optional root)
  "Return the directory path for change NAME under ROOT.
If ROOT is nil, uses the current project root."
  (expand-file-name (format "openspec/changes/%s" name)
                    (or root (openspec--project-root))))

(defun openspec--change-file (name file &optional root)
  "Return path to FILE in change NAME's directory.
If ROOT is nil, uses the current project root.
FILE should be a relative path like \"proposal.md\" or \"specs/foo/spec.md\"."
  (expand-file-name file (openspec--change-dir name root)))

(defun openspec--spec-file (name &optional root)
  "Return path to spec.md for spec NAME.
If ROOT is nil, uses the current project root."
  (expand-file-name (format "openspec/specs/%s/spec.md" name)
                    (or root (openspec--project-root))))

;;; Data Fetching

(defun openspec--fetch-changes ()
  "Fetch the list of all changes from CLI."
  (let ((default-directory (openspec--project-root)))
    (let ((result (openspec--run-json-command-sync '("list" "--json"))))
      (alist-get 'changes result))))

(defun openspec--split-changes-by-status (changes)
  "Split CHANGES into active and completed lists.
Returns a cons cell (ACTIVE . COMPLETED)."
  (let ((active nil)
        (completed nil))
    (dolist (change changes)
      (if (equal (alist-get 'status change) "complete")
          (push change completed)
        (push change active)))
    (cons (nreverse active) (nreverse completed))))

(defun openspec--fetch-specs ()
  "Fetch the list of specs with requirement counts via CLI JSON API."
  (let* ((root (openspec--project-root))
         (specs-dir (expand-file-name "openspec/specs" root))
         (default-directory root)
         (specs nil))
    (when (file-directory-p specs-dir)
      (dolist (entry (directory-files specs-dir nil "^[^.]"))
        (when (file-directory-p (expand-file-name entry specs-dir))
          (let ((json (openspec--run-json-command-sync
                       (list "show" entry "--type" "spec" "--json" "--no-interactive"))))
            (if json
                (push (list (cons 'name (or (alist-get 'id json) entry))
                            (cons 'requirementCount (or (alist-get 'requirementCount json) 0))
                            (cons 'title (alist-get 'title json)))
                      specs)
              (push (list (cons 'name entry)
                          (cons 'requirementCount 0))
                    specs))))))
    (nreverse specs)))

;;; Shared Utilities

(defun openspec--format-relative-time (iso-time)
  "Format ISO-TIME as a relative time string."
  (condition-case nil
      (let* ((time (date-to-time iso-time))
             (diff (float-time (time-subtract (current-time) time)))
             (minutes (/ diff 60))
             (hours (/ diff 3600))
             (days (/ diff 86400)))
        (cond
         ((< minutes 1) "just now")
         ((< minutes 60) (format "%dm ago" (floor minutes)))
         ((< hours 24) (format "%dh ago" (floor hours)))
         ((< days 7) (format "%dd ago" (floor days)))
         (t (format-time-string "%Y-%m-%d" time))))
    (error "unknown")))

(defun openspec--show-output-buffer (buffer-name title content-fn)
  "Create and display a result buffer.
BUFFER-NAME is the buffer name (e.g., \"*openspec-validation*\").
TITLE is inserted as the first line.
CONTENT-FN is called with no arguments in the buffer context to insert content."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert title)
        (funcall content-fn)
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)
    buf))

(defun openspec--item-at-point ()
  "Return the item at point, or nil."
  (get-text-property (point) 'openspec-item))

(defun openspec--change-at-point-or-error ()
  "Return the change name at point, or signal an error."
  (if-let* ((item (openspec--item-at-point)))
      (if (eq (alist-get 'type item) 'change)
          (alist-get 'name item)
        (user-error "Not on a change"))
    (user-error "No item at point")))

(defun openspec--item-at-point-or-error ()
  "Return the item at point, or signal an error."
  (or (openspec--item-at-point)
      (user-error "No item at point")))

;;; Validation Cache

(defun openspec--get-validation-status (name)
  "Get cached validation status for change NAME.
Returns `passed', `failed', or nil if not validated."
  (when openspec--validation-cache
    (gethash name openspec--validation-cache)))

(defun openspec--set-validation-status (name status)
  "Set cached validation STATUS for change NAME.
STATUS should be `passed', `failed', or nil."
  (unless openspec--validation-cache
    (setq openspec--validation-cache (make-hash-table :test 'equal)))
  (puthash name status openspec--validation-cache))

(defun openspec--clear-validation-cache ()
  "Clear the validation cache."
  (when openspec--validation-cache
    (clrhash openspec--validation-cache)))

;;; Status Buffer Refresh Helper

(declare-function openspec-refresh "openspec-status")

(defun openspec--refresh-status-buffers (project-root)
  "Refresh all openspec-mode buffers for PROJECT-ROOT."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (eq major-mode 'openspec-mode)
                 (equal openspec--project-root project-root))
        (openspec-refresh)))))

;;; Conflict Buffer Cleanup Helper

;; Defined in openspec-conflict.el
(defvar openspec--conflict-context)

(defun openspec--cleanup-conflict-buffer ()
  "Clean up the conflict buffer, clearing context and killing it."
  (when-let* ((buf (get-buffer "*openspec-conflict*")))
    (with-current-buffer buf
      (setq openspec--conflict-context nil))
    (kill-buffer buf)))

(provide 'openspec-core)

;;; openspec-core.el ends here
