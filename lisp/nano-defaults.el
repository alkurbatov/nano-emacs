;;; nano-defaults.el --- Global default settings

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers

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

;;; Code:
(require 'bind-key)
(require 'exec-path-from-shell)

;; Move customization variables to a separate file, otherwise init.el will be used
(setq custom-file "~/.emacs.d/nano-custom.el")
(load custom-file 'noerror 'nomessage)

;; Use a single space after dots
(setq sentence-end-double-space nil)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; Mouse active in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (bind-keys*
   ("<mouse-4>" . 'scroll-down-line)
   ("<mouse-5>" . 'scroll-up-line)))

;; No scroll bars
(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

;; No menu bar
(if (display-graphic-p)
    (menu-bar-mode t) ;; When nil, focus problem on OSX
  (menu-bar-mode -1))

;; Enable autocompletion
(global-company-mode)
(define-key company-mode-map [remap indent-for-tab-command]
            #'company-indent-or-complete-common)

;; Completion style, see
;; gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
(require 'orderless)
(setq completion-styles '(substring orderless basic)
      orderless-component-separator 'orderless-escapable-split-on-space
      completion-category-overrides '((file (styles basic partial-completion)))
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; Enable indentation+completion using the TAB key.
;; completion-at-point is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Pixel scroll (as opposed to char scrool)
;; (pixel-scroll-mode t)

;; Mac specific
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t
        mac-use-title-bar nil))

;; Make sure clipboard works properly in tty mode on OSX
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(when (and (not (display-graphic-p))
           (eq system-type 'darwin))
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx))

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Tab.space equivalence
(setq-default tab-width 4)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; Forward some shell variables in OS X GUI
(when (memq window-system '(mac ns x))
  (dolist (var '("LANG" "LC_ALL" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize)

  ;; Reinit Emacs locale as now we have properly set environment
  (set-locale-environment (getenv "LANG")))

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Default shell in term
(unless
    (or (eq system-type 'windows-nt)
        (not (file-exists-p "/bin/zsh")))
  (setq-default shell-file-name "/bin/zsh")
  (setq explicit-shell-file-name "/bin/zsh"))

;; Kill term buffer when exiting
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Delete selected text when starting to type over it
(setq delete-selection-mode t)

;; Remember the last place visited in a file
(save-place-mode 1)

;; Automatically revert buffers for changed files if they don't have unsaved changes
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t
      dired-auto-revert-buffer t)

;; Enable flycheck
(global-flycheck-mode)

;; Highlight changes if version control enabled
(global-diff-hl-mode)

;; Enable yasnippet
(yas-global-mode)

;; Highlight TODO keywords
(global-hl-todo-mode)

;; Better work with trailing whitespaces and line endings
(setq mode-require-final-newline nil)
(add-hook 'org-mode-hook #'ethan-wspace-mode)
(add-hook 'prog-mode-hook #'ethan-wspace-mode)

(provide 'nano-defaults)
;;; nano-defaults.el ends here
