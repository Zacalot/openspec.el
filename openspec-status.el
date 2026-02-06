;;; openspec-status.el --- Status buffer for OpenSpec -*- lexical-binding: t; -*-

;;; Commentary:

;; Major mode definition, buffer rendering, section visibility,
;; navigation, and refresh logic for the OpenSpec status buffer.

;;; Code:

(require 'openspec-core)
(require 'openspec-faces)

;; Commands defined in openspec-commands.el — declare to avoid
;; byte-compiler warnings from keymap references.
(declare-function openspec-show-at-point "openspec-commands")
(declare-function openspec-validate-at-point "openspec-commands")
(declare-function openspec-archive-at-point "openspec-commands")
(declare-function openspec-delete-at-point "openspec-commands")
(declare-function openspec-kill-ring-transient "openspec-commands")
(declare-function openspec-init "openspec-commands")
(declare-function openspec-transient "openspec-commands")
(declare-function openspec-show-design "openspec-commands")
(declare-function openspec-show-tasks "openspec-commands")
(declare-function openspec-show-specs-dir "openspec-commands")
(declare-function openspec-new-change "openspec-commands")
(declare-function openspec-validate-section "openspec-commands")

;;; Internal Variables

(defvar-local openspec--section-visibility (make-hash-table :test 'equal)
  "Hash table tracking section visibility (expanded/collapsed).")

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
        (insert "  Specifications: ")
        (insert (format "%d %s" spec-count (if (= spec-count 1) "spec" "specs")))
        (when (> total-requirements 0)
          (insert (format ", %d %s"
                          total-requirements
                          (if (= total-requirements 1) "requirement" "requirements"))))
        (insert "\n")
        (insert (format "  Active Changes: %d in progress\n" active-count))
        (insert (format "  Completed Changes: %d\n" completed-count))
        (when (> total-tasks 0)
          (insert (format "  Task Progress: %d/%d (%d%%)\n"
                          completed-tasks total-tasks percentage))))
      (insert "\n"))))

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

(defun openspec--render-buffer ()
  "Render the OpenSpec status buffer.
Section order: Header, Summary, Active Changes, Completed Changes, Specs."
  (let ((inhibit-read-only t)
        (item-at-point (get-text-property (point) 'openspec-item))
        (saved-line (line-number-at-pos)))
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
    ;; Try to restore point to same item, fall back to same line number
    (unless (and item-at-point
                 (openspec--goto-item (alist-get 'type item-at-point)
                                      (alist-get 'name item-at-point)))
      (goto-char (point-min))
      (forward-line (1- (min saved-line
                              (count-lines (point-min) (point-max))))))))

;;; Navigation

(defun openspec--goto-item (type name)
  "Move point to item of TYPE with NAME.
Returns non-nil if the item was found."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (let ((item (get-text-property (point) 'openspec-item)))
        (when (and item
                   (eq (alist-get 'type item) type)
                   (equal (alist-get 'name item) name))
          (setq found t)))
      (unless found
        (forward-line 1)))
    (when found
      (beginning-of-line))
    found))

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

;;; Section Detection

(defun openspec--section-at-point ()
  "Return the `openspec-section' text property at or above point.
Checks the current line first, then walks upward to find the
nearest section header."
  (or (get-text-property (point) 'openspec-section)
      (save-excursion
        (beginning-of-line)
        (let ((found nil))
          (while (and (not found) (not (bobp)))
            (forward-line -1)
            (setq found (get-text-property (point) 'openspec-section)))
          found))))

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
    (define-key map (kbd "c") #'openspec-new-change)
    (define-key map (kbd "V") #'openspec-validate-section)
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

;;; Refresh

(defun openspec-refresh ()
  "Refresh the OpenSpec status buffer."
  (interactive)
  (when (eq major-mode 'openspec-mode)
    (message "Openspec...")
    (let ((default-directory openspec--project-root))
      (let ((split (openspec--split-changes-by-status (openspec--fetch-changes))))
        (setq openspec--active-changes (car split))
        (setq openspec--completed-changes (cdr split)))
      (setq openspec--specs (openspec--fetch-specs))
      (openspec--clear-validation-cache)
      (openspec--render-buffer)
      (run-hooks 'openspec-refresh-hook)
      (message ""))))

(provide 'openspec-status)

;;; openspec-status.el ends here
