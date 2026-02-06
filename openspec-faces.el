;;; openspec-faces.el --- Faces for OpenSpec -*- lexical-binding: t; -*-

;;; Commentary:

;; Face definitions used by the OpenSpec Emacs package.

;;; Code:

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

(provide 'openspec-faces)

;;; openspec-faces.el ends here
