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
(require 'nano-modeline)

;; Show changes in unsaved buffers
(diff-hl-flydiff-mode)

;; Save modified files in a repository without additional questions
(setq magit-save-repository-buffers 'dontask)

;; Show the diff indicators in the margin when in TUI mode
(unless (display-graphic-p)
  (add-hook 'prog-mode-hook #'diff-hl-margin-mode)
  (add-hook 'text-mode-hook #'diff-hl-margin-mode))

;; Integrate diff-hl with magit
(add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

;; Check spelling when writing commit message
(add-hook 'git-commit-setup #'git-commit-turn-on-flyspell)

;; Improve display of nano-modeline in magit buffers, see:
;; See: https://github.com/rougier/nano-modeline/issues/8
(defun nano-modeline-magit-mode (orig-fun input)
  "Nano modeline for magit mode."
  (funcall nano-modeline-position
           `((nano-modeline-buffer-status) " "
             (nano-modeline-buffer-name) " "
             ,(concat "(" input ")"))
           '((nano-modeline-cursor-position)
             (nano-modeline-window-dedicated))))

(advice-add 'magit-set-header-line-format :around #'nano-modeline-magit-mode)

(with-eval-after-load 'magit-blame
  (setq magit-blame-styles '((headings
                              (heading-format . "%.8H - %a, %C • %s\12"))
                             (highlight
                              (highlight-face . magit-blame-highlight))
                             (lines
                              (show-lines . t)
                              (show-message . t)))))

;; Enable syntax highlighting when composing commit message
(add-to-list 'auto-mode-alist '("\\.git/COMMIT_EDITMSG\\'" . markdown-mode))

;; Enforce bindings recommended by Magit.
(setq magit-define-global-key-bindings 'recommended)

(provide 'nano-git)
;;; nano-git.el ends here
