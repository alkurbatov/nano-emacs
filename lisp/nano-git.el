;;; nano-git.el --- Git workflow

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2023 - N Λ N O developers

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

;; Show changes in unsaved buffers
(diff-hl-flydiff-mode)

;; Show the diff indicators in the margin when in TUI mode
(unless (display-graphic-p)
  (add-hook 'prog-mode-hook 'diff-hl-margin-mode)
  (add-hook 'text-mode-hook 'diff-hl-margin-mode))

;; Integrate diff-hl with magit
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Check spelling when writing commit message
(add-hook 'git-commit-setup 'git-commit-turn-on-flyspell)

;; Enable syntax highlighting when composing commit message
(add-to-list 'auto-mode-alist '("\\.git/COMMIT_EDITMSG\\'" . markdown-mode))

(provide 'nano-git)
;;; nano-git.el ends here
