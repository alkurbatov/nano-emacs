;;; nano-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2023-2024 - N Λ N O developers

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Configures Org mode

;;; Code:
(require 'bind-key)
(require 'nano-settings)
(require 'org-agenda)

(defun nano-org-time-stamp-inactive ()
  "Insert inactive timestamp with current date."
  (interactive)
  (org-insert-time-stamp (current-time) nil t))

(with-eval-after-load 'org
  (require 'nano-modeline)

  ;; Load org files with tasks
  (if (file-directory-p nano-org-directory)
      (setq org-agenda-files (directory-files-recursively nano-org-directory "\\.org$")))

  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE" "CANCELLED")))

  ;; Add current time when marking item as 'DONE'
  (setq org-log-done 'time)

  ;; Use RET to open org-mode links, including those in quick-help.org
  (setq org-return-follows-link t)

  ;; Hide org markup for README
  (setq org-hide-emphasis-markers t)

  ;; Allow manual change of inline images size
  (setq org-image-actual-width nil)

  ;; Customize some links, see:
  ;; https://kitchingroup.cheme.cmu.edu/blog/2016/11/04/New-link-features-in-org-9/

  ;; "Green" projects aka moving me to personal targets
  (org-link-set-parameters
    "green-project"
    :face 'nano-face-org-green-project
    :follow (lambda (path) (org-open-file path)))

  ;; "Red" projects aka must-do-or-be-fired
  (org-link-set-parameters
    "red-project"
    :face 'nano-face-org-red-project
    :follow (lambda (path) (org-open-file path)))

  (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
  (add-hook 'org-mode-hook             #'display-fill-column-indicator-mode)
  (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)

  (bind-keys :map org-mode-map
             ("C-c z" . org-toggle-link-display)
             ("C-c i" . nano-org-time-stamp-inactive)
             ("<s-return>" . toggle-frame-maximized)))

;; Small speedup, we don't need the agenda immediately after every start
(setq org-agenda-inhibit-startup t)

;; Let the agenda buffer consume whole window
(setq org-agenda-window-setup 'current-window)

(bind-key* "C-c a" 'org-agenda)

(provide 'nano-org)
;;; nano-org.el ends here
