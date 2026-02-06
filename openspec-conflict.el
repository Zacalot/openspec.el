;;; openspec-conflict.el --- Archive conflict resolution for OpenSpec -*- lexical-binding: t; -*-

;;; Commentary:

;; Archive conflict resolution subsystem: conflict mode, conflict buffer,
;; output parsing, resolution commands, warning display, and conflict transient.

;;; Code:

(require 'openspec-core)
(require 'openspec-faces)
(require 'transient)

;;; Internal Variables

(defvar-local openspec--conflict-context nil
  "Buffer-local context for the current archive conflict.
An alist with keys: change-name, conflicts, project-root.
Used in the *openspec-conflict* buffer for non-transient access.")

;;; Conflict Mode

(defvar openspec-conflict-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'openspec--conflict-buffer-open-delta)
    (define-key map (kbd "s") #'openspec-conflict-skip-specs)
    (define-key map (kbd "e") #'openspec-conflict-edit-delta)
    (define-key map (kbd "q") #'openspec-conflict-abort)
    map)
  "Keymap for `openspec-conflict-mode'.")

(define-derived-mode openspec-conflict-mode special-mode "OpenSpec-Conflict"
  "Major mode for OpenSpec archive conflict resolution.

\\{openspec-conflict-mode-map}"
  :group 'openspec)

;;; Output Parsing

(defun openspec--parse-archive-blocker (output)
  "Parse archive OUTPUT for spec conflicts and warnings.
Returns an alist with keys:
  `conflicts' - list of blocking spec conflicts
  `warnings' - list of non-blocking warning messages
Each conflict alist has keys: spec, operation, header, reason.
Returns nil values for both keys if none are detected.

Handles multiple conflict formats:
  - `<spec> ADDED failed for header \"<header>\" - already exists'
  - `<spec> MODIFIED failed for header \"<header>\" - not found'
  - `<spec> REMOVED failed for header \"<header>\" - <reason>'
  - `<spec>: target spec does not exist; ...'

Handles warnings:
  - Lines with `⚠' symbol (proposal warnings)
  - `X incomplete task(s) found'"
  (let ((output (openspec--strip-ansi-codes output))
        (conflicts nil)
        (warnings nil)
        (pos 0))
    ;; Pattern 1: "<spec> ADDED/MODIFIED/REMOVED failed for header \"<header>\" - <reason>"
    (while (string-match
            "\\([a-zA-Z0-9_-]+\\) \\(ADDED\\|MODIFIED\\|REMOVED\\|RENAMED\\) failed for header \"\\([^\"]+\\)\" - \\([^\n]+\\)"
            output pos)
      (push (list (cons 'spec (match-string 1 output))
                  (cons 'operation (match-string 2 output))
                  (cons 'header (match-string 3 output))
                  (cons 'reason (string-trim (match-string 4 output))))
            conflicts)
      (setq pos (match-end 0)))
    ;; Pattern 2: "<spec>: target spec does not exist; ..."
    (setq pos 0)
    (while (string-match
            "\\([a-zA-Z0-9_-]+\\): target spec does not exist[;,] \\([^\n]+\\)"
            output pos)
      (push (list (cons 'spec (match-string 1 output))
                  (cons 'operation "MODIFIED")
                  (cons 'header nil)
                  (cons 'reason (concat "target spec does not exist; " (string-trim (match-string 2 output)))))
            conflicts)
      (setq pos (match-end 0)))
    ;; Pattern 3: Generic "<spec>: <error>" patterns (catch-all for other errors)
    (setq pos 0)
    (while (string-match
            "\\([a-zA-Z0-9_-]+\\): \\(only ADDED requirements are allowed\\|cannot \\|invalid \\)\\([^\n]+\\)"
            output pos)
      (let ((spec (match-string 1 output))
            (reason (concat (match-string 2 output) (match-string 3 output))))
        ;; Avoid duplicates from pattern 2
        (unless (cl-find spec conflicts :key (lambda (c) (alist-get 'spec c)) :test #'equal)
          (push (list (cons 'spec spec)
                      (cons 'operation "UNKNOWN")
                      (cons 'header nil)
                      (cons 'reason (string-trim reason)))
                conflicts)))
      (setq pos (match-end 0)))
    ;; Parse warnings: lines with "⚠" symbol
    (setq pos 0)
    (while (string-match "\\(?:\n\\|\\`\\)[ \t]*⚠[ \t]*\\([^\n]+\\)" output pos)
      (push (string-trim (match-string 1 output)) warnings)
      (setq pos (match-end 0)))
    ;; Parse incomplete tasks warning
    (when (string-match "\\([0-9]+\\) incomplete task(s) found" output)
      (push (format "%s incomplete task(s) found" (match-string 1 output)) warnings))
    (list (cons 'conflicts (nreverse conflicts))
          (cons 'warnings (nreverse warnings)))))

;;; Pre-Archive Warning Collection

(defun openspec--check-proposal-warnings (change-name project-root)
  "Check proposal.md for CHANGE-NAME under PROJECT-ROOT for missing sections.
Returns a list of warning strings for missing required headers."
  (let ((proposal-file (openspec--change-file change-name "proposal.md" project-root))
        (warnings nil))
    (when (file-exists-p proposal-file)
      (with-temp-buffer
        (insert-file-contents proposal-file)
        (goto-char (point-min))
        (unless (re-search-forward "^## Why\\b" nil t)
          (push "Missing required section: ## Why" warnings))
        (goto-char (point-min))
        (unless (re-search-forward "^## What Changes\\b" nil t)
          (push "Missing required section: ## What Changes" warnings))))
    (nreverse warnings)))

(defun openspec--check-task-warnings (change-name project-root)
  "Check tasks.md for CHANGE-NAME under PROJECT-ROOT for incomplete tasks.
Returns a list containing a warning string if incomplete tasks found, or nil."
  (let ((tasks-file (openspec--change-file change-name "tasks.md" project-root)))
    (when (file-exists-p tasks-file)
      (with-temp-buffer
        (insert-file-contents tasks-file)
        (let ((total 0)
              (incomplete 0))
          (goto-char (point-min))
          (while (re-search-forward "^- \\[.\\]" nil t)
            (setq total (1+ total))
            (unless (looking-back "^- \\[x\\]" (line-beginning-position))
              (setq incomplete (1+ incomplete))))
          (when (> incomplete 0)
            (list (format "%d incomplete task(s) found" incomplete))))))))

(defun openspec--collect-archive-warnings (change-name project-root)
  "Collect all pre-archive warnings for CHANGE-NAME under PROJECT-ROOT.
Returns a combined list of warning strings from proposal and task checks."
  (append (openspec--check-proposal-warnings change-name project-root)
          (openspec--check-task-warnings change-name project-root)))

(defun openspec--confirm-warnings (change-name warnings)
  "Display WARNINGS for CHANGE-NAME and prompt for confirmation.
If WARNINGS is nil, return t immediately.
Otherwise, show warnings in *openspec-warning* buffer and prompt with `y-or-n-p'.
Returns t if user confirms, nil if declined."
  (if (null warnings)
      t
    (openspec--show-warning-buffer change-name warnings)
    (y-or-n-p (format "%d warning(s) found. Proceed with archive? " (length warnings)))))

;;; Archive Execution

(defun openspec--archive-change (name &optional skip-specs no-validate)
  "Archive change NAME via CLI.
If SKIP-SPECS is non-nil, pass the --skip-specs flag.
If NO-VALIDATE is non-nil, pass the --no-validate flag.
Always uses --yes to auto-confirm all prompts.
Returns an alist with keys:
  `success' - t if archive succeeded
  `output' - raw CLI output (ANSI codes stripped)
  `conflicts' - list of parsed conflicts (if any)
  `warnings' - list of parsed warnings (if any)"
  (let* ((args (append (list "archive" name "--yes")
                       (when skip-specs '("--skip-specs"))
                       (when no-validate '("--no-validate"))))
         (result-pair (openspec--run-command-sync args))
         (exit-code (car result-pair))
         (output (openspec--strip-ansi-codes (cdr result-pair)))
         (parsed (openspec--parse-archive-blocker output))
         (conflicts (alist-get 'conflicts parsed))
         (warnings (alist-get 'warnings parsed))
         (aborted (string-match-p "Aborted" output))
         (success (and (= exit-code 0) (not conflicts) (not aborted))))
    (list (cons 'success success)
          (cons 'output output)
          (cons 'conflicts conflicts)
          (cons 'warnings warnings))))

;;; Archive Result Processing (unified)

(defun openspec--process-archive-result (name result project-root &optional cleanup-conflict-p)
  "Process the RESULT of archiving change NAME.
PROJECT-ROOT is the project directory.
When CLEANUP-CONFLICT-P is non-nil, also clean up the conflict
buffer on success.
Returns t if the archive succeeded, nil otherwise.

Dispatch:
  - Conflicts: show conflict buffer for resolution
  - Success with warnings: show warnings (informational), archive completed
  - Success: refresh, done
  - Failure: show error buffer"
  (let ((success (alist-get 'success result))
        (output (alist-get 'output result))
        (conflicts (alist-get 'conflicts result))
        (warnings (alist-get 'warnings result)))
    (cond
     ;; Conflicts detected - show conflict UI for resolution
     (conflicts
      (message "Archive failed with %d conflict(s)" (length conflicts))
      (openspec--show-conflict-buffer name conflicts project-root)
      nil)
     ;; Success with warnings
     ((and success warnings)
      (message "Archived with %d warning(s): %s" (length warnings) name)
      (openspec--show-warning-buffer name warnings)
      (run-hook-with-args 'openspec-archive-hook name)
      (when cleanup-conflict-p
        (openspec--cleanup-conflict-buffer))
      (openspec--refresh-status-buffers project-root)
      t)
     ;; Success - no issues
     (success
      (message "Archived: %s" name)
      (run-hook-with-args 'openspec-archive-hook name)
      (when cleanup-conflict-p
        (openspec--cleanup-conflict-buffer))
      (openspec--refresh-status-buffers project-root)
      t)
     ;; Failure - show error buffer
     (t
      (message "Archive failed: %s" name)
      (openspec--show-archive-error name output)
      nil))))

;;; Display Buffers

(defun openspec--show-archive-error (name output)
  "Show archive error OUTPUT for NAME in a buffer."
  (openspec--show-output-buffer
   "*openspec-archive*"
   (format "Archive failed for: %s\n\n" name)
   (lambda ()
     (insert (if (stringp output) output (format "%S" output))))))

(defun openspec--show-warning-buffer (name warnings)
  "Display WARNINGS for change NAME in a dedicated buffer.
Returns the buffer."
  (openspec--show-output-buffer
   "*openspec-warning*"
   ""
   (lambda ()
     (insert (propertize "Archive Warnings\n" 'face 'openspec-section-header))
     (insert (format "Change: %s\n\n" name))
     (insert (propertize (format "%d warning(s) detected:\n\n"
                                 (length warnings))
                         'face 'openspec-warning))
     (dolist (warning warnings)
       (insert "  ")
       (insert (propertize "⚠" 'face 'openspec-warning))
       (insert " ")
       (insert warning)
       (insert "\n"))
     (insert "\n"))))

(defun openspec--show-conflict-buffer (change-name conflicts project-root)
  "Display CONFLICTS for CHANGE-NAME in a dedicated buffer.
PROJECT-ROOT is used for navigating to delta files."
  (let ((buf (get-buffer-create "*openspec-conflict*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Archive Conflict\n" 'face 'openspec-section-header))
        (insert (format "Change: %s\n\n" change-name))
        (insert (propertize (format "%d conflict(s) detected:\n\n"
                                    (length conflicts))
                            'face 'openspec-warning))
        (dolist (conflict conflicts)
          (let* ((spec (alist-get 'spec conflict))
                 (operation (alist-get 'operation conflict))
                 (header (alist-get 'header conflict))
                 (reason (alist-get 'reason conflict))
                 (start (point)))
            (insert "  ")
            (insert (propertize spec 'face 'openspec-spec-name))
            (insert " ")
            (insert (propertize (or operation "ERROR") 'face 'openspec-error))
            (if header
                (insert (format " failed for \"%s\"\n" header))
              (insert " failed\n"))
            (insert (format "    Reason: %s\n" (or reason "unknown")))
            (put-text-property start (point)
                               'openspec-conflict-item
                               (list (cons 'spec spec)
                                     (cons 'header header)
                                     (cons 'change-name change-name)
                                     (cons 'project-root project-root)))
            (insert "\n")))
        (insert (propertize "Resolution options:\n" 'face 'font-lock-keyword-face))
        (insert "  s - Skip specs (archive without updating specs)\n")
        (insert "  e - Edit delta file\n")
        (insert "  q - Abort archive\n")
        (insert "\nPress RET on a conflict to open the delta file.\n")
        (goto-char (point-min)))
      (openspec-conflict-mode)
      ;; Set buffer-local context for non-transient access
      (setq openspec--conflict-context
            (list (cons 'change-name change-name)
                  (cons 'conflicts conflicts)
                  (cons 'project-root project-root))))
    (display-buffer buf)
    ;; Call transient with scope for transient commands
    (transient-setup 'openspec-archive-conflict-transient nil nil
                     :scope (list (cons 'change-name change-name)
                                  (cons 'conflicts conflicts)
                                  (cons 'project-root project-root)))))

;;; Delta File Navigation

(defun openspec--open-delta-at-header (delta-file header)
  "Open DELTA-FILE and search for HEADER if non-nil.
Returns non-nil if the file was found, nil otherwise."
  (if (file-exists-p delta-file)
      (progn
        (find-file delta-file)
        (goto-char (point-min))
        (when header
          (re-search-forward (regexp-quote header) nil t))
        t)
    (message "Delta file not found: %s" delta-file)
    nil))

;;; Conflict Resolution Commands

(defun openspec--conflict-buffer-open-delta ()
  "Open the delta file for the conflict at point."
  (interactive)
  (if-let* ((item (get-text-property (point) 'openspec-conflict-item)))
      (let* ((spec (alist-get 'spec item))
             (change-name (alist-get 'change-name item))
             (project-root (alist-get 'project-root item))
             (delta-file (openspec--change-file
                          change-name (format "specs/%s/spec.md" spec) project-root)))
        (openspec--open-delta-at-header delta-file (alist-get 'header item)))
    (message "No conflict at point")))

(defun openspec--get-conflict-context ()
  "Get the current conflict context from transient scope or buffer-local.
Checks transient scope first (for transient commands), then falls back
to buffer-local variable (for direct keybindings in conflict buffer)."
  (or (transient-scope)
      (buffer-local-value 'openspec--conflict-context
                          (get-buffer "*openspec-conflict*"))
      (user-error "No conflict context available")))

(defun openspec-conflict-skip-specs ()
  "Re-run archive with --skip-specs flag.
Pre-checks for warnings and requires confirmation if any are found."
  (interactive)
  (let* ((ctx (openspec--get-conflict-context))
         (change-name (alist-get 'change-name ctx))
         (project-root (alist-get 'project-root ctx))
         (default-directory project-root)
         (warnings (openspec--collect-archive-warnings change-name project-root)))
    (if (openspec--confirm-warnings change-name warnings)
        (progn
          (message "Archiving %s (skipping specs)..." change-name)
          (let* ((result (openspec--archive-change change-name t))
                 (success (alist-get 'success result)))
            (if success
                (progn
                  (message "Archived (specs skipped): %s" change-name)
                  (run-hook-with-args 'openspec-archive-hook change-name)
                  (openspec--cleanup-conflict-buffer)
                  (openspec--refresh-status-buffers project-root))
              (message "Archive failed: %s" (alist-get 'output result)))))
      (message "Archive cancelled"))))

(defun openspec-conflict-edit-delta ()
  "Open the first conflicting delta spec file for editing."
  (interactive)
  (let* ((ctx (openspec--get-conflict-context))
         (change-name (alist-get 'change-name ctx))
         (conflicts (alist-get 'conflicts ctx))
         (project-root (alist-get 'project-root ctx))
         (first-conflict (car conflicts)))
    (if first-conflict
        (let* ((spec (alist-get 'spec first-conflict))
               (delta-file (openspec--change-file
                            change-name (format "specs/%s/spec.md" spec) project-root)))
          (when (openspec--open-delta-at-header delta-file (alist-get 'header first-conflict))
            (message "Edit the delta file and retry archive when ready")))
      (message "No conflicts to edit"))))

(defun openspec-conflict-force-archive ()
  "Force archive, skipping validation.
Prompts for confirmation before proceeding."
  (interactive)
  (let* ((ctx (openspec--get-conflict-context))
         (change-name (alist-get 'change-name ctx))
         (project-root (alist-get 'project-root ctx))
         (default-directory project-root))
    (when (y-or-n-p (format "Force archive '%s' (skip validation)? " change-name))
      (message "Archiving %s (skipping validation)..." change-name)
      (let ((result (openspec--archive-change change-name nil t)))
        (openspec--process-archive-result change-name result project-root t)))))

(defun openspec-conflict-abort ()
  "Abort the archive operation."
  (interactive)
  (when-let* ((buf (get-buffer "*openspec-conflict*")))
    (with-current-buffer buf
      (setq openspec--conflict-context nil))
    (quit-window nil (get-buffer-window buf)))
  (message "Archive cancelled"))

(defun openspec-conflict-view-details ()
  "Display the conflict details buffer."
  (interactive)
  (if-let* ((buf (get-buffer "*openspec-conflict*")))
      (display-buffer buf)
    (message "No conflict details available")))

;;; Conflict Transient

(transient-define-prefix openspec-archive-conflict-transient ()
  "Transient menu for resolving archive conflicts."
  ["Archive Conflict Resolution"
   ("s" "Skip specs (archive without spec updates)" openspec-conflict-skip-specs)
   ("f" "Force archive (skip validation)" openspec-conflict-force-archive)
   ("e" "Edit delta file" openspec-conflict-edit-delta)
   ("v" "View conflict details" openspec-conflict-view-details)
   ("q" "Abort" openspec-conflict-abort)])

(provide 'openspec-conflict)

;;; openspec-conflict.el ends here
