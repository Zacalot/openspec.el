;;; openspec.el --- Interface for OpenSpec -*- lexical-binding: t; -*-

;; Copyright (C)

;; Author: Zacalot
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))
;; Keywords: ai, llm, openspec, agent
;; URL: https://github.com/Zacalot/openspec.el

;;; Commentary:

;; openspec.el provides a interface for managing OpenSpec
;; specifications and change proposals.  It wraps the `openspec' CLI tool
;; and presents its output in an interactive Emacs buffer.

;;; Usage:

;; Main entry point:
;;   M-x `openspec-status' to open the OpenSpec status buffer
;;
;; Or use the default keybinding:
;;   C-c o                  Open the OpenSpec status buffer
;;
;; In the status buffer:
;;   g     Refresh
;;   RET   Open item at point
;;   TAB   Expand/collapse section
;;   v     Validate active change at point
;;   a     Archive change at point (with confirmation)
;;   k     Delete change at point (with confirmation)
;;   w     Kill-Ring menu (apply command, name, etc.)
;;   i     Initialize project (if not initialized)
;;   n/p   Navigate between items and sections
;;   ?     Show help (transient menu)
;;   q     Quit
;;
;; Hooks for extensibility:
;;   `openspec-refresh-hook' - Run after refreshing the status buffer
;;   `openspec-archive-hook' - Run after successfully archiving a change

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'transient)

;;; Customization

(defgroup openspec nil
  "Magit-style interface for OpenSpec."
  :group 'tools
  :prefix "openspec-")

(defcustom openspec-executable "openspec"
  "Path to the openspec executable."
  :type 'string
  :group 'openspec)

(defcustom openspec-status-key "C-c o"
  "Keybinding for `openspec-status'.
Set to nil to disable the global keybinding."
  :type '(choice (string :tag "Key sequence")
                 (const :tag "No keybinding" nil))
  :group 'openspec
  :set (lambda (sym val)
         (when (boundp 'openspec-status-key)
           (let ((old-key (symbol-value sym)))
             (when old-key
               (global-unset-key (kbd old-key)))))
         (set-default sym val)
         (when val
           (global-set-key (kbd val) #'openspec-status))))

(defcustom openspec-minimum-version "0.20.0"
  "Minimum required version of the openspec CLI."
  :type 'string
  :group 'openspec)

;;; Faces

(defgroup openspec-faces nil
  "Faces used by OpenSpec."
  :group 'openspec
  :group 'faces)

(defface openspec-section-header
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section headers."
  :group 'openspec-faces)

(defface openspec-section-header-count
  '((t :inherit shadow))
  "Face for section header counts."
  :group 'openspec-faces)

(defface openspec-change-name
  '((t :inherit font-lock-function-name-face))
  "Face for change names."
  :group 'openspec-faces)

(defface openspec-spec-name
  '((t :inherit font-lock-type-face))
  "Face for spec names."
  :group 'openspec-faces)

(defface openspec-task-progress
  '((t :inherit shadow))
  "Face for task progress indicators."
  :group 'openspec-faces)

(defface openspec-timestamp
  '((t :inherit shadow))
  "Face for timestamps."
  :group 'openspec-faces)

(defface openspec-success
  '((t :inherit success))
  "Face for success indicators."
  :group 'openspec-faces)

(defface openspec-error
  '((t :inherit error))
  "Face for error indicators."
  :group 'openspec-faces)

(defface openspec-warning
  '((t :inherit warning))
  "Face for warning indicators."
  :group 'openspec-faces)

(defface openspec-uninitialized
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for uninitialized project message."
  :group 'openspec-faces)

;;; Internal variables

(defvar-local openspec--project-root nil
  "Root directory of the current OpenSpec project.")

(defvar-local openspec--changes nil
  "Cached list of all changes for the current buffer.")

(defvar-local openspec--active-changes nil
  "Cached list of active (non-complete) changes for the current buffer.")

(defvar-local openspec--completed-changes nil
  "Cached list of completed changes for the current buffer.")

(defvar-local openspec--specs nil
  "Cached list of specs for the current buffer.")

(defvar-local openspec--validation-cache nil
  "Hash table caching validation results per change name.
Values are symbols: `passed', `failed', or nil for not validated.")

(defvar-local openspec--section-visibility (make-hash-table :test 'equal)
  "Hash table tracking section visibility (expanded/collapsed).")

(defvar-local openspec--point-item nil
  "Item at point before refresh, for restoration.")

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

;;; Project Detection

(defun openspec--find-root (&optional dir)
  "Find the OpenSpec project root starting from DIR.
Searches upward for an `openspec/' directory."
  (let ((start (or dir default-directory)))
    (locate-dominating-file start "openspec")))

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

(defun openspec--count-requirements (spec-file)
  "Count the number of requirements in SPEC-FILE.
Requirements are identified by '### Requirement:' headers."
  (if (file-exists-p spec-file)
      (with-temp-buffer
        (insert-file-contents spec-file)
        (let ((count 0))
          (goto-char (point-min))
          (while (re-search-forward "^### Requirement:" nil t)
            (setq count (1+ count)))
          count))
    0))

(defun openspec--fetch-specs ()
  "Fetch the list of specs with requirement counts."
  (let* ((root (openspec--project-root))
         (specs-dir (expand-file-name "openspec/specs" root))
         (specs nil))
    (when (file-directory-p specs-dir)
      (dolist (entry (directory-files specs-dir t "^[^.]"))
        (when (and (file-directory-p entry)
                   (file-exists-p (expand-file-name "spec.md" entry)))
          (let* ((spec-file (expand-file-name "spec.md" entry))
                 (req-count (openspec--count-requirements spec-file)))
            (push (list (cons 'name (file-name-nondirectory entry))
                        (cons 'path entry)
                        (cons 'requirementCount req-count))
                  specs)))))
    (nreverse specs)))

;;; Buffer Rendering

(defun openspec--buffer-name (root)
  "Return the buffer name for project at ROOT."
  (format "*openspec: %s*" (file-name-nondirectory (directory-file-name root))))

(defun openspec--section-visible-p (section)
  "Return non-nil if SECTION is visible (expanded)."
  (not (gethash section openspec--section-visibility)))

(defun openspec--toggle-section-visibility (section)
  "Toggle visibility of SECTION."
  (puthash section
           (not (gethash section openspec--section-visibility))
           openspec--section-visibility))

(defun openspec--insert-header ()
  "Insert the buffer header."
  (insert (propertize "OpenSpec" 'face 'openspec-section-header))
  (insert ": ")
  (insert (abbreviate-file-name openspec--project-root))
  (insert "\n\n"))

(defun openspec--insert-uninitialized ()
  "Insert the uninitialized project section."
  (insert (propertize "Project not initialized\n" 'face 'openspec-uninitialized))
  (insert "\n")
  (insert (propertize "  Press " 'face 'openspec-uninitialized))
  (insert (propertize "i" 'face 'font-lock-keyword-face))
  (insert (propertize " to initialize OpenSpec in this project\n" 'face 'openspec-uninitialized)))

(defun openspec--insert-section-header (title count &optional section-id)
  "Insert a section header with TITLE and COUNT.
SECTION-ID is used for collapse tracking."
  (let ((section-id (or section-id title))
        (start (point)))
    (insert (propertize title 'face 'openspec-section-header))
    (when count
      (insert " ")
      (insert (propertize (format "(%d)" count) 'face 'openspec-section-header-count)))
    (insert "\n")
    (put-text-property start (point) 'openspec-section section-id)))

(defun openspec--insert-summary-section ()
  "Insert the summary section at the top of the buffer."
  (let* ((specs openspec--specs)
         (active openspec--active-changes)
         (completed openspec--completed-changes)
         (spec-count (length specs))
         (total-requirements (cl-reduce #'+ specs
                                        :key (lambda (s) (or (alist-get 'requirementCount s) 0))
                                        :initial-value 0))
         (active-count (length active))
         (completed-count (length completed))
         (total-tasks 0)
         (completed-tasks 0))
    ;; Calculate aggregate task progress from active changes
    (dolist (change active)
      (setq total-tasks (+ total-tasks (or (alist-get 'totalTasks change) 0)))
      (setq completed-tasks (+ completed-tasks (or (alist-get 'completedTasks change) 0))))
    (let ((percentage (if (> total-tasks 0)
                          (/ (* 100 completed-tasks) total-tasks)
                        0))
          (start (point)))
      (insert (propertize "Summary" 'face 'openspec-section-header))
      (insert "\n")
      (put-text-property start (point) 'openspec-section "summary")
      (when (openspec--section-visible-p "summary")
        ;; Specifications line
        (insert "  Specifications: ")
        (insert (format "%d %s" spec-count (if (= spec-count 1) "spec" "specs")))
        (when (> total-requirements 0)
          (insert (format ", %d %s"
                          total-requirements
                          (if (= total-requirements 1) "requirement" "requirements"))))
        (insert "\n")
        ;; Active changes line
        (insert (format "  Active Changes: %d in progress\n" active-count))
        ;; Completed changes line
        (insert (format "  Completed Changes: %d\n" completed-count))
        ;; Task progress line (only if there are tasks)
        (when (> total-tasks 0)
          (insert (format "  Task Progress: %d/%d (%d%%)\n"
                          completed-tasks total-tasks percentage))))
      (insert "\n"))))

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

(defun openspec--insert-change (change)
  "Insert a single CHANGE entry with validation indicator and percentage."
  (let* ((name (alist-get 'name change))
         (completed (alist-get 'completedTasks change 0))
         (total (alist-get 'totalTasks change 0))
         (last-modified (alist-get 'lastModified change))
         (validation-status (openspec--get-validation-status name))
         (percentage (if (> total 0) (/ (* 100 completed) total) 0))
         (start (point)))
    (insert "  ")
    ;; Validation indicator: [V] passed, [F] failed, spaces for not validated
    (pcase validation-status
      ('passed (insert (propertize "[V]" 'face 'openspec-success) " "))
      ('failed (insert (propertize "[F]" 'face 'openspec-error) " "))
      (_ (insert "    ")))
    (insert (propertize name 'face 'openspec-change-name))
    (when (> total 0)
      (insert "  ")
      (insert (propertize (format "[%d/%d tasks (%d%%)]" completed total percentage)
                          'face 'openspec-task-progress)))
    (when last-modified
      (insert "  ")
      (insert (propertize (openspec--format-relative-time last-modified)
                          'face 'openspec-timestamp)))
    (insert "\n")
    (put-text-property start (point) 'openspec-item `((type . change) (name . ,name)))))

(defun openspec--insert-completed-change (change)
  "Insert a single completed CHANGE entry with minimal info."
  (let* ((name (alist-get 'name change))
         (start (point)))
    (insert "  ")
    (insert (propertize name 'face 'openspec-change-name))
    (insert "\n")
    (put-text-property start (point) 'openspec-item `((type . change) (name . ,name)))))

(defun openspec--insert-spec (spec)
  "Insert a single SPEC entry with requirement count."
  (let* ((name (alist-get 'name spec))
         (req-count (or (alist-get 'requirementCount spec) 0))
         (start (point)))
    (insert "  ")
    (insert (propertize name 'face 'openspec-spec-name))
    (when (> req-count 0)
      (insert "  ")
      (insert (propertize (format "%d %s" req-count
                                  (if (= req-count 1) "requirement" "requirements"))
                          'face 'openspec-task-progress)))
    (insert "\n")
    (put-text-property start (point) 'openspec-item `((type . spec) (name . ,name)))))

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

(defun openspec--render-buffer ()
  "Render the OpenSpec status buffer.
Section order: Header, Summary, Active Changes, Completed Changes, Specs."
  (let ((inhibit-read-only t)
        (item-at-point (get-text-property (point) 'openspec-item)))
    (erase-buffer)
    (openspec--insert-header)
    (if (not (openspec--initialized-p openspec--project-root))
        (openspec--insert-uninitialized)
      ;; Summary section
      (openspec--insert-summary-section)
      ;; Active Changes section
      (let ((changes openspec--active-changes))
        (openspec--insert-section-header "Active Changes" (length changes) "changes")
        (if (openspec--section-visible-p "changes")
            (if changes
                (dolist (change changes)
                  (openspec--insert-change change))
              (insert (propertize "  (none)\n" 'face 'shadow)))
          (insert (propertize "  ...\n" 'face 'shadow))))
      (insert "\n")
      ;; Completed Changes section (collapsed by default)
      (let ((completed openspec--completed-changes))
        (when completed
          (openspec--insert-section-header "Completed Changes" (length completed) "completed-changes")
          (if (openspec--section-visible-p "completed-changes")
              (dolist (change completed)
                (openspec--insert-completed-change change))
            (insert (propertize "  ...\n" 'face 'shadow)))
          (insert "\n")))
      ;; Specs section
      (let ((specs openspec--specs))
        (openspec--insert-section-header "Specs" (length specs) "specs")
        (if (openspec--section-visible-p "specs")
            (if specs
                (dolist (spec specs)
                  (openspec--insert-spec spec))
              (insert (propertize "  (none)\n" 'face 'shadow)))
          (insert (propertize "  ...\n" 'face 'shadow)))))
    (goto-char (point-min))
    ;; Try to restore point to same item
    (when item-at-point
      (openspec--goto-item (alist-get 'type item-at-point)
                           (alist-get 'name item-at-point)))))

(defun openspec--goto-item (type name)
  "Move point to item of TYPE with NAME."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (let ((item (get-text-property (point) 'openspec-item)))
        (when (and item
                   (eq (alist-get 'type item) type)
                   (equal (alist-get 'name item) name))
          (setq found t)))
      (unless found
        (forward-line 1))))
  (beginning-of-line))

(defun openspec--goto-section (section-id)
  "Move point to section with SECTION-ID.
Returns non-nil if found."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (when (equal (get-text-property (point) 'openspec-section) section-id)
        (setq found t))
      (unless found
        (forward-line 1)))
    (when found
      (beginning-of-line))
    found))

;;; Major Mode

(defvar openspec-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'openspec-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'openspec-show-at-point)
    (define-key map (kbd "TAB") #'openspec-toggle-section)
    (define-key map (kbd "v") #'openspec-validate-at-point)
    (define-key map (kbd "a") #'openspec-archive-at-point)
    (define-key map (kbd "k") #'openspec-delete-at-point)
    (define-key map (kbd "w") #'openspec-kill-ring-transient)
    (define-key map (kbd "i") #'openspec-init)
    (define-key map (kbd "n") #'openspec-next)
    (define-key map (kbd "p") #'openspec-prev)
    (define-key map (kbd "?") #'openspec-transient)
    (define-key map (kbd "d") #'openspec-show-design)
    (define-key map (kbd "t") #'openspec-show-tasks)
    (define-key map (kbd "s") #'openspec-show-specs-dir)
    map)
  "Keymap for `openspec-mode'.")

(define-derived-mode openspec-mode special-mode "OpenSpec"
  "Major mode for OpenSpec status buffer.

\\{openspec-mode-map}"
  :group 'openspec
  (setq-local revert-buffer-function #'openspec--revert-buffer)
  (setq-local openspec--section-visibility (make-hash-table :test 'equal))
  ;; Default completed-changes section to collapsed
  (puthash "completed-changes" t openspec--section-visibility)
  (setq-local openspec--validation-cache (make-hash-table :test 'equal)))

(defun openspec--revert-buffer (_ignore-auto _noconfirm)
  "Revert the OpenSpec buffer by refreshing data."
  (openspec-refresh))

;;; Interactive Commands

;;;###autoload
(defun openspec-status ()
  "Open the OpenSpec status buffer for the current project."
  (interactive)
  (unless (openspec--cli-available-p)
    (user-error "OpenSpec CLI not found. Please install it from https://openspec.dev"))
  (let ((root (openspec--project-root)))
    (unless root
      (user-error "Could not determine project root"))
    (let ((buf (get-buffer-create (openspec--buffer-name root))))
      (with-current-buffer buf
        (unless (eq major-mode 'openspec-mode)
          (openspec-mode))
        (setq openspec--project-root root)
        (setq default-directory root)
        (openspec-refresh))
      (pop-to-buffer buf))))

(defun openspec-refresh ()
  "Refresh the OpenSpec status buffer."
  (interactive)
  (when (eq major-mode 'openspec-mode)
    (message "Openspec...")
    (let ((default-directory openspec--project-root))
      ;; Fetch and split changes
      (setq openspec--changes (openspec--fetch-changes))
      (let ((split (openspec--split-changes-by-status openspec--changes)))
        (setq openspec--active-changes (car split))
        (setq openspec--completed-changes (cdr split)))
      (setq openspec--specs (openspec--fetch-specs))
      ;; Clear validation cache on refresh
      (openspec--clear-validation-cache)
      (openspec--render-buffer)
      (run-hooks 'openspec-refresh-hook)
      (message ""))))

(defun openspec-toggle-section ()
  "Toggle the section at point.
Preserves point at the section header after re-rendering."
  (interactive)
  (let ((section (get-text-property (point) 'openspec-section)))
    (if section
        (progn
          (openspec--toggle-section-visibility section)
          (openspec--render-buffer)
          (openspec--goto-section section))
      ;; If on an item, try to find the section header above
      (let ((found-section nil))
        (save-excursion
          (beginning-of-line)
          (while (and (not (bobp))
                      (not (get-text-property (point) 'openspec-section)))
            (forward-line -1))
          (setq found-section (get-text-property (point) 'openspec-section)))
        (when found-section
          (openspec--toggle-section-visibility found-section)
          (openspec--render-buffer)
          (openspec--goto-section found-section))))))

(defun openspec-next ()
  "Move to the next item or section header."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (get-text-property (point) 'openspec-item))
                (not (get-text-property (point) 'openspec-section)))
      (forward-line 1))
    (when (eobp)
      (goto-char start)
      (message "No more items"))))

(defun openspec-prev ()
  "Move to the previous item or section header."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (get-text-property (point) 'openspec-item))
                (not (get-text-property (point) 'openspec-section)))
      (forward-line -1))
    (when (and (bobp)
               (not (get-text-property (point) 'openspec-item))
               (not (get-text-property (point) 'openspec-section)))
      (goto-char start)
      (message "No more items"))))

(defun openspec--item-at-point ()
  "Return the item at point, or nil."
  (get-text-property (point) 'openspec-item))

(defun openspec-show-at-point ()
  "Show the item at point."
  (interactive)
  (if-let* ((item (openspec--item-at-point)))
      (let* ((type (alist-get 'type item))
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
               (message "spec.md not found for %s" name))))))
    (message "No item at point")))

(defun openspec-validate-at-point ()
  "Validate the item at point and cache the result."
  (interactive)
  (if-let* ((item (openspec--item-at-point)))
      (let* ((type (alist-get 'type item))
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
                ;; Cache validation passed for changes
                (when (eq type 'change)
                  (openspec--set-validation-status name 'passed)
                  (openspec--render-buffer))
                (message "Validation passed: %s" name))
            (let ((items (and result (alist-get 'items result))))
              ;; Cache validation failed for changes
              (when (eq type 'change)
                (openspec--set-validation-status name 'failed)
                (openspec--render-buffer))
              (if items
                  (let ((issues (cl-loop for item in items
                                         append (alist-get 'issues item))))
                    (message "Validation failed: %s (%d issues)" name (length issues))
                    (openspec--show-validation-issues name issues))
                (message "Validation failed: %s" name))))))
    (message "No item at point")))

(defun openspec--archive-change (name &optional skip-specs)
  "Archive change NAME via CLI.
If SKIP-SPECS is non-nil, pass the --skip-specs flag.
Returns an alist with keys:
  `success' - t if archive succeeded
  `output' - raw CLI output
  `conflicts' - list of parsed conflicts (if any)"
  (let* ((args (if skip-specs
                   (list "archive" name "--yes" "--skip-specs")
                 (list "archive" name "--yes")))
         (result-pair (openspec--run-command-sync args))
         (exit-code (car result-pair))
         (output (cdr result-pair))
         (conflicts (openspec--parse-archive-error output)))
    (list (cons 'success (and (= exit-code 0) (not conflicts)))
          (cons 'output output)
          (cons 'conflicts conflicts))))

(defun openspec--handle-archive-result (name result project-root saved-line)
  "Handle the RESULT of archiving change NAME.
PROJECT-ROOT is the project directory.
SAVED-LINE is the line number to restore point to on success.
Returns t if the archive succeeded, nil otherwise."
  (let ((success (alist-get 'success result))
        (output (alist-get 'output result))
        (conflicts (alist-get 'conflicts result)))
    (cond
     ;; Conflicts detected - show conflict UI
     (conflicts
      (message "Archive failed with %d conflict(s)" (length conflicts))
      (openspec--show-conflict-buffer name conflicts project-root)
      nil)
     ;; Success - no conflicts
     (success
      (message "Archived: %s" name)
      (run-hook-with-args 'openspec-archive-hook name)
      (openspec-refresh)
      (goto-char (point-min))
      (forward-line (1- (min saved-line (count-lines (point-min) (point-max)))))
      t)
     ;; Other failure - show generic error
     (t
      (message "Archive failed: %s" name)
      (openspec--show-archive-error name output)
      nil))))

(defun openspec-archive-at-point ()
  "Archive the change at point after confirmation."
  (interactive)
  (if-let* ((item (openspec--item-at-point)))
      (if (eq (alist-get 'type item) 'change)
          (let* ((name (alist-get 'name item))
                 (project-root openspec--project-root)
                 (default-directory project-root)
                 (saved-line (line-number-at-pos)))
            (if (y-or-n-p (format "Archive change '%s'? " name))
                (progn
                  (message "Archiving %s..." name)
                  (let ((result (openspec--archive-change name)))
                    (openspec--handle-archive-result name result project-root saved-line)))
              (message "Archive cancelled")))
        (message "Not on a change"))
    (message "No item at point")))

(defun openspec--show-archive-error (name output)
  "Show archive error OUTPUT for NAME in a buffer."
  (let ((buf (get-buffer-create "*openspec-archive*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Archive failed for: %s\n\n" name))
        (insert (if (stringp output)
                    output
                  (format "%S" output)))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun openspec--show-validation-issues (name issues)
  "Show validation ISSUES for NAME in a buffer."
  (let ((buf (get-buffer-create "*openspec-validation*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Validation results for: %s\n\n" name))
        (if (null issues)
            (insert "No issues found.\n")
          (dolist (issue issues)
            (let ((level (alist-get 'level issue))
                  (path (alist-get 'path issue))
                  (message (alist-get 'message issue)))
              (insert (propertize (or level "ERROR")
                                  'face (pcase level
                                          ("WARNING" 'openspec-warning)
                                          (_ 'openspec-error))))
              (insert ": ")
              (when path
                (insert (format "[%s] " path)))
              (insert (or message "Unknown error"))
              (insert "\n"))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

;;; Archive Conflict Handling

(defvar-local openspec--conflict-context nil
  "Buffer-local context for the current archive conflict.
An alist with keys: change-name, conflicts, project-root.
Used in the *openspec-conflict* buffer for non-transient access.")

(defun openspec--parse-archive-error (output)
  "Parse archive error OUTPUT for spec conflicts.
Returns a list of conflict alists, each with keys:
spec, operation, header, reason.
Returns nil if no conflicts are detected."
  (let ((conflicts nil)
        (pos 0))
    ;; Match patterns like: "<spec> ADDED failed for header \"<header>\" - already exists"
    ;; or: "<spec> MODIFIED failed for header \"<header>\" - not found"
    (while (string-match
            "\\([a-zA-Z0-9_-]+\\) \\(ADDED\\|MODIFIED\\|REMOVED\\) failed for header \"\\([^\"]+\\)\" - \\(.+\\)"
            output pos)
      (push (list (cons 'spec (match-string 1 output))
                  (cons 'operation (match-string 2 output))
                  (cons 'header (match-string 3 output))
                  (cons 'reason (match-string 4 output)))
            conflicts)
      (setq pos (match-end 0)))
    (nreverse conflicts)))

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
            (insert (propertize operation 'face 'openspec-error))
            (insert (format " failed for \"%s\"\n" header))
            (insert (format "    Reason: %s\n" reason))
            ;; Add navigation property
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
        (insert "  c - Convert ADDED to MODIFIED\n")
        (insert "  q - Abort archive\n")
        (insert "\nPress RET on a conflict to open the delta file.\n")
        (goto-char (point-min))
        ;; Set up local keymap
        (use-local-map (let ((map (make-sparse-keymap)))
                         (set-keymap-parent map special-mode-map)
                         (define-key map (kbd "RET") #'openspec--conflict-buffer-open-delta)
                         (define-key map (kbd "s") #'openspec-conflict-skip-specs)
                         (define-key map (kbd "e") #'openspec-conflict-edit-delta)
                         (define-key map (kbd "c") #'openspec-conflict-convert-to-modified)
                         (define-key map (kbd "q") #'openspec-conflict-abort)
                         map))
        (special-mode)
        ;; Set buffer-local context for non-transient access
        (setq openspec--conflict-context
              (list (cons 'change-name change-name)
                    (cons 'conflicts conflicts)
                    (cons 'project-root project-root)))))
    (display-buffer buf)
    ;; Call transient with scope for transient commands
    (transient-setup 'openspec-archive-conflict-transient nil nil
                     :scope (list (cons 'change-name change-name)
                                  (cons 'conflicts conflicts)
                                  (cons 'project-root project-root)))))

(defun openspec--conflict-buffer-open-delta ()
  "Open the delta file for the conflict at point."
  (interactive)
  (if-let* ((item (get-text-property (point) 'openspec-conflict-item)))
      (let* ((spec (alist-get 'spec item))
             (header (alist-get 'header item))
             (change-name (alist-get 'change-name item))
             (project-root (alist-get 'project-root item))
             (delta-file (openspec--change-file
                          change-name (format "specs/%s/spec.md" spec) project-root)))
        (if (file-exists-p delta-file)
            (progn
              (find-file delta-file)
              ;; Try to position at the header
              (goto-char (point-min))
              (when header
                (re-search-forward (regexp-quote header) nil t)))
          (message "Delta file not found: %s" delta-file)))
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
  "Re-run archive with --skip-specs flag."
  (interactive)
  (let* ((ctx (openspec--get-conflict-context))
         (change-name (alist-get 'change-name ctx))
         (project-root (alist-get 'project-root ctx))
         (default-directory project-root))
    (message "Archiving %s (skipping specs)..." change-name)
    (let* ((result (openspec--archive-change change-name t))
           (success (alist-get 'success result)))
      (if success
          (progn
            (message "Archived (specs skipped): %s" change-name)
            (run-hook-with-args 'openspec-archive-hook change-name)
            (when-let* ((buf (get-buffer "*openspec-conflict*")))
              (with-current-buffer buf
                (setq openspec--conflict-context nil))
              (kill-buffer buf))
            ;; Refresh the status buffer if it exists
            (dolist (buf (buffer-list))
              (with-current-buffer buf
                (when (and (eq major-mode 'openspec-mode)
                           (equal openspec--project-root project-root))
                  (openspec-refresh)))))
        (message "Archive failed: %s" (alist-get 'output result))))))

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
               (header (alist-get 'header first-conflict))
               (delta-file (openspec--change-file
                            change-name (format "specs/%s/spec.md" spec) project-root)))
          (if (file-exists-p delta-file)
              (progn
                (find-file delta-file)
                ;; Try to position at the header
                (goto-char (point-min))
                (when header
                  (re-search-forward (regexp-quote header) nil t))
                (message "Edit the delta file and retry archive when ready"))
            (message "Delta file not found: %s" delta-file)))
      (message "No conflicts to edit"))))

(defun openspec-conflict-convert-to-modified ()
  "Convert ADDED to MODIFIED in the conflicting delta file."
  (interactive)
  (let* ((ctx (openspec--get-conflict-context))
         (change-name (alist-get 'change-name ctx))
         (conflicts (alist-get 'conflicts ctx))
         (project-root (alist-get 'project-root ctx))
         (first-conflict (car conflicts))
         (operation (and first-conflict (alist-get 'operation first-conflict))))
    (cond
     ((not first-conflict)
      (message "No conflicts to convert"))
     ((not (equal operation "ADDED"))
      (message "Conversion not applicable for %s conflicts" operation))
     (t
      (let* ((spec (alist-get 'spec first-conflict))
             (delta-file (openspec--change-file
                          change-name (format "specs/%s/spec.md" spec) project-root)))
        (if (not (file-exists-p delta-file))
            (message "Delta file not found: %s" delta-file)
          (when (yes-or-no-p
                 (format "Convert '## ADDED Requirements' to '## MODIFIED Requirements' in %s? "
                         spec))
            (openspec--convert-added-to-modified delta-file)
            (message "Converted ADDED to MODIFIED in %s" spec)
            (when (yes-or-no-p "Retry archive? ")
              (openspec--retry-archive change-name project-root)))))))))

(defun openspec--convert-added-to-modified (file)
  "Replace '## ADDED Requirements' with '## MODIFIED Requirements' in FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^## ADDED Requirements" nil t)
        (replace-match "## MODIFIED Requirements")))
    (save-buffer)))

(defun openspec--retry-archive (change-name project-root)
  "Retry archiving CHANGE-NAME in PROJECT-ROOT."
  (let ((default-directory project-root))
    (message "Retrying archive %s..." change-name)
    (let* ((result (openspec--archive-change change-name))
           (success (alist-get 'success result))
           (conflicts (alist-get 'conflicts result)))
      (cond
       ;; Conflicts detected
       (conflicts
        (message "Archive failed with %d conflict(s)" (length conflicts))
        (openspec--show-conflict-buffer change-name conflicts project-root))
       ;; Success
       (success
        (message "Archived: %s" change-name)
        (run-hook-with-args 'openspec-archive-hook change-name)
        (when-let* ((buf (get-buffer "*openspec-conflict*")))
          (with-current-buffer buf
            (setq openspec--conflict-context nil))
          (kill-buffer buf))
        ;; Refresh status buffer
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (and (eq major-mode 'openspec-mode)
                       (equal openspec--project-root project-root))
              (openspec-refresh)))))
       ;; Other failure
       (t
        (message "Archive failed: %s" (alist-get 'output result)))))))

(defun openspec-conflict-abort ()
  "Abort the archive operation."
  (interactive)
  (when-let* ((buf (get-buffer "*openspec-conflict*")))
    (with-current-buffer buf
      (setq openspec--conflict-context nil))
    (quit-window nil (get-buffer-window buf)))
  (message "Archive cancelled"))

(transient-define-prefix openspec-archive-conflict-transient ()
  "Transient menu for resolving archive conflicts."
  ["Archive Conflict Resolution"
   ("s" "Skip specs (archive without spec updates)" openspec-conflict-skip-specs)
   ("e" "Edit delta file" openspec-conflict-edit-delta)
   ("c" "Convert ADDED to MODIFIED" openspec-conflict-convert-to-modified)
   ("v" "View conflict details" openspec-conflict-view-details)
   ("q" "Abort" openspec-conflict-abort)])

(defun openspec-conflict-view-details ()
  "Display the conflict details buffer."
  (interactive)
  (if-let* ((buf (get-buffer "*openspec-conflict*")))
      (display-buffer buf)
    (message "No conflict details available")))

;;; Delete Change

(defun openspec--count-completed-tasks-in-file (tasks-file)
  "Count completed tasks (lines matching '- [x]') in TASKS-FILE.
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
  (if-let* ((item (openspec--item-at-point)))
      (if (eq (alist-get 'type item) 'change)
          (let* ((name (alist-get 'name item))
                 (project-root openspec--project-root)
                 (change-dir (openspec--change-dir name project-root))
                 (tasks-file (openspec--change-file name "tasks.md" project-root))
                 (completed-count (openspec--count-completed-tasks-in-file tasks-file))
                 (saved-line (line-number-at-pos)))
            ;; First confirmation
            (if (y-or-n-p (format "Delete change '%s'? " name))
                ;; Second confirmation if there are completed tasks
                (if (and (> completed-count 0)
                         (not (y-or-n-p
                               (format "Warning: This change has %d completed task%s. Continue? "
                                       completed-count
                                       (if (= completed-count 1) "" "s")))))
                    (message "Delete cancelled")
                  ;; Proceed with deletion
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
                     (let ((buf (get-buffer-create "*openspec-error*")))
                       (with-current-buffer buf
                         (let ((inhibit-read-only t))
                           (erase-buffer)
                           (insert (format "Delete failed for: %s\n\n" name))
                           (insert (format "Error: %s" (error-message-string err)))
                           (goto-char (point-min))
                           (special-mode)))
                       (display-buffer buf)))))
              (message "Delete cancelled")))
        (message "No item at point"))
    (message "No item at point")))

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
  "Fallback agent list used when CLI is unavailable.")

(defconst openspec--tool-id-display-names
  '(("amazon-q" . "Amazon Q Developer")
    ("antigravity" . "Antigravity")
    ("auggie" . "Auggie")
    ("claude" . "Claude Code")
    ("cline" . "Cline")
    ("codex" . "Codex")
    ("codebuddy" . "CodeBuddy")
    ("continue" . "Continue")
    ("costrict" . "CoStrict")
    ("crush" . "Crush")
    ("cursor" . "Cursor")
    ("factory" . "Factory Droid")
    ("gemini" . "Gemini CLI")
    ("github-copilot" . "GitHub Copilot")
    ("iflow" . "iFlow")
    ("kilocode" . "Kilo Code")
    ("opencode" . "OpenCode")
    ("qoder" . "Qoder")
    ("qwen" . "Qwen Code")
    ("roocode" . "RooCode")
    ("windsurf" . "Windsurf"))
  "Alist mapping tool IDs to human-readable display names.")

(defun openspec--parse-tools-from-help (help-output)
  "Parse tool IDs from HELP-OUTPUT of `openspec init -h`.
Returns a list of tool ID strings, excluding meta-values like \"all\" and \"none\"."
  (when (string-match
         "comma-separated list of: \\(\\(?:[a-z0-9-]+,? *\n? *\\)+\\)"
         help-output)
    (let* ((tools-str (match-string 1 help-output))
           (tools (split-string tools-str "[, \n\t]+" t)))
      ;; Filter out meta-values
      (cl-remove-if (lambda (tool)
                      (member tool '("all" "none")))
                    tools))))

(defun openspec--tool-id-to-display-name (tool-id)
  "Convert TOOL-ID to a human-readable display name."
  (or (alist-get tool-id openspec--tool-id-display-names nil nil #'equal)
      ;; Fallback: capitalize and replace hyphens with spaces
      (capitalize (replace-regexp-in-string "-" " " tool-id))))

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
                ;; Build alist with display names, add "Other / None" at end
                (append
                 (mapcar (lambda (id)
                           (cons (openspec--tool-id-to-display-name id) id))
                         tool-ids)
                 '(("Other / None" . "none")))
              ;; Parsing failed, use fallback
              (message "Warning: Could not parse CLI help output, using fallback agent list")
              openspec--fallback-agents))
        ;; CLI call failed, use fallback
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
        (let ((buf (get-buffer-create "*openspec-error*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (cdr result))
              (special-mode)))
          (display-buffer buf))))))

;;; Transient Menus

(transient-define-prefix openspec-transient ()
  "OpenSpec commands."
  ["Actions"
   ("g" "Refresh" openspec-refresh)
   ("i" "Initialize" openspec-init)
   ("q" "Quit" quit-window)]
  ["Changes"
   ("v" "Validate at point" openspec-validate-at-point)
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

(defun openspec--change-at-point-or-error ()
  "Return the change name at point, or signal an error."
  (if-let* ((item (openspec--item-at-point)))
      (if (eq (alist-get 'type item) 'change)
          (alist-get 'name item)
        (user-error "Not on a change"))
    (user-error "No item at point")))

(defun openspec-show-proposal ()
  "Open the proposal.md for the change at point."
  (interactive)
  (let* ((name (openspec--change-at-point-or-error))
         (file (openspec--change-file name "proposal.md" openspec--project-root)))
    (if (file-exists-p file)
        (find-file file)
      (message "proposal.md not found"))))

(defun openspec-show-tasks ()
  "Open the tasks.md for the change at point."
  (interactive)
  (let* ((name (openspec--change-at-point-or-error))
         (file (openspec--change-file name "tasks.md" openspec--project-root)))
    (if (file-exists-p file)
        (find-file file)
      (message "tasks.md not found"))))

(defun openspec-show-design ()
  "Open the design.md for the change at point."
  (interactive)
  (let* ((name (openspec--change-at-point-or-error))
         (file (openspec--change-file name "design.md" openspec--project-root)))
    (if (file-exists-p file)
        (find-file file)
      (message "design.md not found"))))

(defun openspec-show-specs-dir ()
  "Open the specs directory for the change at point."
  (interactive)
  (let* ((name (openspec--change-at-point-or-error))
         (dir (openspec--change-file name "specs" openspec--project-root)))
    (if (file-directory-p dir)
        (dired dir)
      (message "specs/ directory not found"))))

;;; Kill-Ring Menu

(defun openspec-kill-ring-apply-command ()
  "Kill-Ring the /openspec:apply command for the change at point."
  (interactive)
  (if-let* ((item (openspec--item-at-point)))
      (if (eq (alist-get 'type item) 'change)
          (let* ((name (alist-get 'name item))
                 (str (format "/openspec:apply %s" name)))
            (kill-new str)
            (message "Copied: %s" str))
        (message "Not on a change"))
    (message "No item at point")))

(defun openspec-kill-ring-item-name ()
  "Kill-Ring the name of the item at point."
  (interactive)
  (if-let* ((item (openspec--item-at-point)))
      (let ((name (alist-get 'name item)))
        (kill-new name)
        (message "Copied: %s" name))
    (message "No item at point")))

(defun openspec-kill-ring-item-path ()
  "Kill-Ring the path to the primary file of the item at point."
  (interactive)
  (if-let* ((item (openspec--item-at-point)))
      (let* ((type (alist-get 'type item))
             (name (alist-get 'name item))
             (root openspec--project-root)
             (path (pcase type
                     ('change (openspec--change-file name "proposal.md" root))
                     ('spec (openspec--spec-file name root)))))
        (kill-new path)
        (message "Copied: %s" path))
    (message "No item at point")))

(transient-define-prefix openspec-kill-ring-transient ()
  "Kill-Ring commands for OpenSpec items."
  ["Kill-Ring"
   ("a" "Apply command (/openspec:apply ...)" openspec-kill-ring-apply-command)
   ("i" "Item name" openspec-kill-ring-item-name)
   ("p" "Path to file" openspec-kill-ring-item-path)])

;;; Global keybinding setup

(when openspec-status-key
  (global-set-key (kbd openspec-status-key) #'openspec-status))

(provide 'openspec)

;;; openspec.el ends here
