;;; nano-git.el --- Git workflow -*- lexical-binding: t; -*-

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

;; Configures git behavior.

;;; Code:
(require 'diff-hl)
(require 'log-edit)
(require 's)
(require 'vc-git)

;; Show changes in unsaved buffers
(diff-hl-flydiff-mode)

;; Show the diff indicators in the margin when in TUI mode
(add-hook 'prog-mode-hook #'diff-hl-margin-mode)
(add-hook 'text-mode-hook #'diff-hl-margin-mode)

;; As usually only git is used.
;; This may have an effect on performance, as Emacs will not try to
;; check for a bunch of backends.
(setq vc-handled-backends '(Git))

(setq vc-git-log-edit-summary-target-len 50
      vc-git-log-edit-summary-max-len 70)

(defun nano-setup-log-edit ()
  "Setup vc-git-log-edit-mode."

  ;; Show fill column when composing commit message
  (setq-local fill-column 64)

  (display-fill-column-indicator-mode))

(add-hook 'vc-git-log-edit-mode-hook #'nano-setup-log-edit)

;; Enable syntax highlighting when composing commit message
(add-to-list 'auto-mode-alist '("\\.git/COMMIT_EDITMSG\\'" . vc-git-log-edit-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig" . gitconfig-mode))

(global-set-key (kbd "C-c g l") 'git-link)

(provide 'nano-git)
;;; nano-git.el ends here
