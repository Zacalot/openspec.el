;;; openspec-commands.el --- Interactive commands for OpenSpec -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive commands: show, validate, archive, delete, init,
;; file navigation, kill-ring commands, and transient menus.

;;; Code:

(require 'openspec-core)
(require 'openspec-status)
(require 'openspec-conflict)
(require 'openspec-faces)
(require 'transient)

;;; Show Commands

(defun openspec-show-at-point ()
  "Show the item at point."
  (interactive)
  (let* ((item (openspec--item-at-point-or-error))
         (type (alist-get 'type item))
         (name (alist-get 'name item))
         (root openspec--project-root))
    (pcase type
      ('change
       (let ((proposal (openspec--change-file name "proposal.md" root)))
         (if (file-exists-p proposal)
             (find-file proposal)
           (message "proposal.md not found for %s" name))))
      ('spec
       (let ((spec-file (openspec--spec-file name root)))
         (if (file-exists-p spec-file)
             (find-file spec-file)
           (message "spec.md not found for %s" name)))))))

(defun openspec--show-change-file (filename)
  "Open FILENAME for the change at point."
  (let* ((name (openspec--change-at-point-or-error))
         (file (openspec--change-file name filename openspec--project-root)))
    (if (file-exists-p file)
        (find-file file)
      (message "%s not found" filename))))

(defun openspec-show-proposal ()
  "Open the proposal.md for the change at point."
  (interactive)
  (openspec--show-change-file "proposal.md"))

(defun openspec-show-tasks ()
  "Open the tasks.md for the change at point."
  (interactive)
  (openspec--show-change-file "tasks.md"))

(defun openspec-show-design ()
  "Open the design.md for the change at point."
  (interactive)
  (openspec--show-change-file "design.md"))

(defun openspec-show-specs-dir ()
  "Open the specs directory for the change at point."
  (interactive)
  (let* ((name (openspec--change-at-point-or-error))
         (dir (openspec--change-file name "specs" openspec--project-root)))
    (if (file-directory-p dir)
        (dired dir)
      (message "specs/ directory not found"))))

;;; Validate

(defun openspec-validate-at-point ()
  "Validate the item at point and cache the result."
  (interactive)
  (let* ((item (openspec--item-at-point-or-error))
         (type (alist-get 'type item))
         (name (alist-get 'name item))
         (default-directory openspec--project-root))
    (message "Validating %s..." name)
    (let* ((result-pair (openspec--run-command-sync
                         (list "validate" name "--strict" "--no-interactive" "--json")))
           (exit-code (car result-pair))
           (output (cdr result-pair))
           (result (condition-case nil
                       (json-parse-string output
                                          :object-type 'alist
                                          :array-type 'list)
                     (error nil))))
      (if (= exit-code 0)
          (progn
            (when (eq type 'change)
              (openspec--set-validation-status name 'passed)
              (openspec--render-buffer))
            (message "Validation passed: %s" name))
        (let ((items (and result (alist-get 'items result))))
          (when (eq type 'change)
            (openspec--set-validation-status name 'failed)
            (openspec--render-buffer))
          (if items
              (let ((issues (cl-loop for item in items
                                     append (alist-get 'issues item))))
                (message "Validation failed: %s (%d issues)" name (length issues))
                (openspec--show-validation-issues name issues))
            (message "Validation failed: %s" name)))))))

(defun openspec--show-validation-issues (name issues)
  "Show validation ISSUES for NAME in a buffer."
  (openspec--show-output-buffer
   "*openspec-validation*"
   (format "Validation results for: %s\n\n" name)
   (lambda ()
     (if (null issues)
         (insert "No issues found.\n")
       (dolist (issue issues)
         (let ((level (alist-get 'level issue))
               (path (alist-get 'path issue))
               (msg (alist-get 'message issue)))
           (insert (propertize (or level "ERROR")
                               'face (pcase level
                                       ("WARNING" 'openspec-warning)
                                       (_ 'openspec-error))))
           (insert ": ")
           (when path
             (insert (format "[%s] " path)))
           (insert (or msg "Unknown error"))
           (insert "\n")))))))

;;; Archive

(defun openspec-archive-at-point ()
  "Archive the change at point after confirmation.
Pre-checks for warnings (missing proposal sections, incomplete tasks)
and requires explicit confirmation if any are found."
  (interactive)
  (let* ((name (openspec--change-at-point-or-error))
         (project-root openspec--project-root)
         (default-directory project-root)
         (warnings (openspec--collect-archive-warnings name project-root)))
    (if (y-or-n-p (format "Archive change '%s'? " name))
        (if (openspec--confirm-warnings name warnings)
            (progn
              (message "Archiving %s..." name)
              (let ((result (openspec--archive-change name)))
                (openspec--process-archive-result name result project-root)))
          (message "Archive cancelled"))
      (message "Archive cancelled"))))

;;; Delete

(defun openspec--count-completed-tasks-in-file (tasks-file)
  "Count completed tasks (lines matching `- [x]') in TASKS-FILE.
Returns the count, or 0 if file does not exist."
  (if (file-exists-p tasks-file)
      (with-temp-buffer
        (insert-file-contents tasks-file)
        (let ((count 0))
          (goto-char (point-min))
          (while (re-search-forward "^- \\[x\\]" nil t)
            (setq count (1+ count)))
          count))
    0))

(defun openspec-delete-at-point ()
  "Delete the change at point after confirmation.
Prompts for confirmation, with an additional warning if the change
has completed tasks."
  (interactive)
  (let* ((name (openspec--change-at-point-or-error))
         (project-root openspec--project-root)
         (change-dir (openspec--change-dir name project-root))
         (tasks-file (openspec--change-file name "tasks.md" project-root))
         (completed-count (openspec--count-completed-tasks-in-file tasks-file))
         (saved-line (line-number-at-pos)))
    (if (y-or-n-p (format "Delete change '%s'? " name))
        (if (and (> completed-count 0)
                 (not (y-or-n-p
                       (format "Warning: This change has %d completed task%s. Continue? "
                               completed-count
                               (if (= completed-count 1) "" "s")))))
            (message "Delete cancelled")
          (condition-case err
              (progn
                (delete-directory change-dir t)
                (message "Deleted: %s" name)
                (openspec-refresh)
                (goto-char (point-min))
                (forward-line (1- (min saved-line
                                       (count-lines (point-min) (point-max))))))
            (error
             (message "Delete failed: %s" name)
             (openspec--show-output-buffer
              "*openspec-error*"
              (format "Delete failed for: %s\n\n" name)
              (lambda ()
                (insert (format "Error: %s" (error-message-string err))))))))
      (message "Delete cancelled"))))

;;; Initialization

(defvar openspec--available-agents-cache nil
  "Cached alist of available agents fetched from CLI.
Each element is (DISPLAY-NAME . TOOL-ID).")

(defconst openspec--fallback-agents
  '(("Amazon Q Developer" . "amazon-q")
    ("Antigravity" . "antigravity")
    ("Auggie" . "auggie")
    ("Claude Code" . "claude")
    ("Cline" . "cline")
    ("Codex" . "codex")
    ("CodeBuddy" . "codebuddy")
    ("Continue" . "continue")
    ("CoStrict" . "costrict")
    ("Crush" . "crush")
    ("Cursor" . "cursor")
    ("Factory Droid" . "factory")
    ("Gemini CLI" . "gemini")
    ("GitHub Copilot" . "github-copilot")
    ("iFlow" . "iflow")
    ("Kilo Code" . "kilocode")
    ("OpenCode" . "opencode")
    ("Qoder" . "qoder")
    ("Qwen Code" . "qwen")
    ("RooCode" . "roocode")
    ("Windsurf" . "windsurf")
    ("Other / None" . "none"))
  "Fallback agent list used when CLI is unavailable.
Maps display names to tool IDs.")

(defun openspec--tool-id-to-display-name (tool-id)
  "Convert TOOL-ID to a human-readable display name.
Looks up the ID in `openspec--fallback-agents' (reversed), falling
back to capitalizing the ID with hyphens replaced by spaces."
  (or (car (cl-rassoc tool-id openspec--fallback-agents :test #'equal))
      (capitalize (replace-regexp-in-string "-" " " tool-id))))

(defun openspec--parse-tools-from-help (help-output)
  "Parse tool IDs from HELP-OUTPUT of `openspec init -h'.
Returns a list of tool ID strings, excluding meta-values like \"all\" and \"none\"."
  (when (string-match
         "comma-separated list of: \\(\\(?:[a-z0-9-]+,? *\n? *\\)+\\)"
         help-output)
    (let* ((tools-str (match-string 1 help-output))
           (tools (split-string tools-str "[, \n\t]+" t)))
      (cl-remove-if (lambda (tool)
                      (member tool '("all" "none")))
                    tools))))

(defun openspec--fetch-available-agents ()
  "Fetch available agents from the CLI and return an alist.
Returns alist of (DISPLAY-NAME . TOOL-ID) pairs.
Falls back to `openspec--fallback-agents' if CLI is unavailable."
  (if (not (openspec--cli-available-p))
      (progn
        (message "Warning: OpenSpec CLI not found, using fallback agent list")
        openspec--fallback-agents)
    (let ((result (openspec--run-command-sync '("init" "-h"))))
      (if (and (= (car result) 0)
               (stringp (cdr result)))
          (let ((tool-ids (openspec--parse-tools-from-help (cdr result))))
            (if tool-ids
                (append
                 (mapcar (lambda (id)
                           (cons (openspec--tool-id-to-display-name id) id))
                         tool-ids)
                 '(("Other / None" . "none")))
              (message "Warning: Could not parse CLI help output, using fallback agent list")
              openspec--fallback-agents))
        (message "Warning: CLI help command failed, using fallback agent list")
        openspec--fallback-agents))))

(defun openspec--get-available-agents ()
  "Return alist of available agents, using cache if available.
Each element is (DISPLAY-NAME . TOOL-ID)."
  (or openspec--available-agents-cache
      (setq openspec--available-agents-cache
            (openspec--fetch-available-agents))))

(defun openspec-refresh-agents-cache ()
  "Clear the cached agent list, forcing a refresh on next use."
  (interactive)
  (setq openspec--available-agents-cache nil)
  (message "Agent cache cleared"))

(defun openspec-init ()
  "Initialize OpenSpec in the current project."
  (interactive)
  (when (openspec--initialized-p openspec--project-root)
    (user-error "OpenSpec is already initialized in this project"))
  (let* ((agents (openspec--get-available-agents))
         (choices (mapcar #'car agents))
         (choice (completing-read "Select AI agent: " choices nil t))
         (agent (alist-get choice agents nil nil #'equal))
         (default-directory (or openspec--project-root (openspec--project-root))))
    (message "Initializing OpenSpec with %s..." choice)
    (let ((result (openspec--run-command-sync
                   (list "init" "--tools" agent))))
      (if (= (car result) 0)
          (progn
            (message "OpenSpec initialized successfully")
            (when (eq major-mode 'openspec-mode)
              (openspec-refresh)))
        (message "Initialization failed: %s" (cdr result))
        (openspec--show-output-buffer
         "*openspec-error*"
         ""
         (lambda ()
           (insert (cdr result))))))))

;;; Bulk Validation

(declare-function openspec--section-at-point "openspec-status")

(defun openspec-validate-section ()
  "Validate items based on the section at point.
On Active/Completed Changes: validates changes.
On Specs: validates specs.
Elsewhere: validates all."
  (interactive)
  (let* ((section (openspec--section-at-point))
         (scope (cond
                 ((member section '("changes" "completed-changes")) "--changes")
                 ((equal section "specs") "--specs")
                 (t "--all")))
         (label (cond
                 ((string= scope "--changes") "changes")
                 ((string= scope "--specs") "specs")
                 (t "all")))
         (default-directory openspec--project-root))
    (message "Validating %s..." label)
    (let* ((result-pair (openspec--run-command-sync
                         (list "validate" scope "--strict" "--json")))
           (exit-code (car result-pair))
           (output (cdr result-pair))
           (result (condition-case nil
                       (json-parse-string output
                                          :object-type 'alist
                                          :array-type 'list)
                     (error nil)))
           (items (and result (alist-get 'items result)))
           (total (length items))
           (passed 0)
           (failed-items nil))
      (dolist (item items)
        (let ((id (alist-get 'id item))
              (item-type (alist-get 'type item))
              (valid (alist-get 'valid item)))
          (if (eq valid t)
              (progn
                (setq passed (1+ passed))
                (when (equal item-type "change")
                  (openspec--set-validation-status id 'passed)))
            (push item failed-items)
            (when (equal item-type "change")
              (openspec--set-validation-status id 'failed)))))
      (let ((failed (length failed-items)))
        (if (= failed 0)
            (message "Validation: all %d %s passed" total label)
          (message "Validation: %d/%d %s passed (%d failed)" passed total label failed)
          (openspec--show-bulk-validation-issues failed-items)))
      (openspec--render-buffer))))

(defun openspec--show-bulk-validation-issues (failed-items)
  "Show FAILED-ITEMS in *openspec-validation* buffer."
  (openspec--show-output-buffer
   "*openspec-validation*"
   "Bulk Validation Results\n\n"
   (lambda ()
     (dolist (item failed-items)
       (let ((id (alist-get 'id item))
             (item-type (alist-get 'type item))
             (issues (alist-get 'issues item)))
         (insert (propertize (format "%s (%s)" id (or item-type "unknown"))
                             'face 'openspec-error))
         (insert "\n")
         (dolist (issue issues)
           (let ((level (alist-get 'level issue))
                 (path (alist-get 'path issue))
                 (msg (alist-get 'message issue)))
             (insert "  ")
             (insert (propertize (or level "ERROR")
                                 'face (pcase level
                                         ("WARNING" 'openspec-warning)
                                         (_ 'openspec-error))))
             (insert ": ")
             (when path
               (insert (format "[%s] " path)))
             (insert (or msg "Unknown error"))
             (insert "\n")))
         (insert "\n"))))))

;;; Change Creation

(defun openspec-new-change ()
  "Create a new OpenSpec change.
Prompts for a name and validates it as kebab-case
\(lowercase letters, numbers, and hyphens only)."
  (interactive)
  (let ((name (read-string "Change name: "))
        (default-directory openspec--project-root))
    (when (string-empty-p name)
      (user-error "Change name cannot be empty"))
    (unless (string-match-p "\\`[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\'" name)
      (user-error "Invalid change name: must contain only lowercase letters, numbers, and hyphens"))
    (message "Creating change: %s..." name)
    (let ((result (openspec--run-command-sync (list "new" "change" name))))
      (if (= (car result) 0)
          (progn
            (message "Created change: %s" name)
            (when (eq major-mode 'openspec-mode)
              (openspec-refresh)))
        (message "Failed to create change: %s"
                 (string-trim (openspec--strip-ansi-codes (cdr result))))))))

;;; Update

(defun openspec-update ()
  "Update OpenSpec instruction files."
  (interactive)
  (let ((default-directory (or openspec--project-root (openspec--project-root))))
    (message "Updating OpenSpec instruction files...")
    (let ((result (openspec--run-command-sync '("update"))))
      (if (= (car result) 0)
          (message "OpenSpec instruction files updated")
        (message "Update failed: %s"
                 (string-trim (openspec--strip-ansi-codes (cdr result))))))))

;;; Transient Menus

(transient-define-prefix openspec-transient ()
  "OpenSpec commands."
  ["Actions"
   ("g" "Refresh" openspec-refresh)
   ("U" "Update instruction files" openspec-update)
   ("i" "Initialize" openspec-init)
   ("q" "Quit" quit-window)]
  ["Changes"
   ("c" "Create change" openspec-new-change)
   ("v" "Validate at point" openspec-validate-at-point)
   ("V" "Validate section" openspec-validate-section)
   ("a" "Archive at point (confirm)" openspec-archive-at-point)
   ("k" "Delete at point (confirm)" openspec-delete-at-point)
   ("d" "Design" openspec-show-design)
   ("t" "Tasks" openspec-show-tasks)
   ("s" "Specs" openspec-show-specs-dir)]
  ["Kill-Ring"
   ("w" "Kill-Ring menu..." openspec-kill-ring-transient)]
  ["Navigation"
   ("n" "Next" openspec-next)
   ("p" "Previous" openspec-prev)
   ("TAB" "Toggle section" openspec-toggle-section)])

;;; Kill-Ring Commands

(defun openspec-kill-ring-apply-command ()
  "Kill-Ring the apply command for the change at point."
  (interactive)
  (let* ((name (openspec--change-at-point-or-error))
         (str (format "/opsx:apply %s" name)))
    (kill-new str)
    (message "Copied: %s" str)))

(defun openspec-kill-ring-verify-command ()
  "Kill-Ring the verify command for the change at point."
  (interactive)
  (let* ((name (openspec--change-at-point-or-error))
         (str (format "/opsx:verify %s" name)))
    (kill-new str)
    (message "Copied: %s" str)))

(defun openspec-kill-ring-archive-command ()
  "Kill-Ring the archive command for the change at point."
  (interactive)
  (let* ((name (openspec--change-at-point-or-error))
         (str (format "/opsx:archive %s" name)))
    (kill-new str)
    (message "Copied: %s" str)))

(defun openspec-kill-ring-item-name ()
  "Kill-Ring the name of the item at point."
  (interactive)
  (let* ((item (openspec--item-at-point-or-error))
         (name (alist-get 'name item)))
    (kill-new name)
    (message "Copied: %s" name)))

(defun openspec-kill-ring-item-path ()
  "Kill-Ring the path to the primary file of the item at point."
  (interactive)
  (let* ((item (openspec--item-at-point-or-error))
         (type (alist-get 'type item))
         (name (alist-get 'name item))
         (root openspec--project-root)
         (path (pcase type
                 ('change (openspec--change-file name "proposal.md" root))
                 ('spec (openspec--spec-file name root)))))
    (kill-new path)
    (message "Copied: %s" path)))

(transient-define-prefix openspec-kill-ring-transient ()
  "Kill-Ring commands for OpenSpec items."
  ["Kill-Ring"
   ("a" "Apply command" openspec-kill-ring-apply-command)
   ("v" "Verify command" openspec-kill-ring-verify-command)
   ("r" "Archive command" openspec-kill-ring-archive-command)
   ("i" "Item name" openspec-kill-ring-item-name)
   ("p" "Path to file" openspec-kill-ring-item-path)])

(provide 'openspec-commands)

;;; openspec-commands.el ends here
