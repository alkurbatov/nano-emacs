;;; nano-defaults.el --- Global default settings -*- lexical-binding: t; -*-

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020-2024 - N Λ N O developers

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
(require 'company)
(require 'exec-path-from-shell)
(require 'orderless)
(require 'rfc-mode)
(require 'treesit-auto)

;; Move customization variables to a separate file, otherwise init.el will be used
(setq custom-file "~/.emacs.d/nano-custom.el")
(load custom-file 'noerror 'nomessage)

;; Typography
(setq-default fill-column 80                          ; Default line width
              sentence-end-double-space nil           ; Use a single space after dots
              bidi-paragraph-direction 'left-to-right ; Faster
              truncate-string-ellipsis "…")           ; Nicer ellipsis

; Increase read size per process
(setq-default read-process-output-max (* 1024 1024))

;; No confirmation for killing running process
(setq-default confirm-kill-processes nil)

;; No confirmation for visiting non-existent files
(setq-default confirm-nonexistent-file-or-buffer nil)

;; Follow symlinks without prompt
(setq vc-follow-symlinks t)

;; Replace yes/no prompts with y/n
(setq-default use-short-answers t)

(setq-default visible-bell nil             ; No visual bell
              ring-bell-function 'ignore)  ; No bell

;; No scroll bars
(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

;; Disable some byte compile warnings
(setq native-comp-async-report-warnings-errors 'silent)

;; Enable yasnippet
(yas-global-mode)

;; Enable autocompletion
(global-company-mode)

;; Show autocompletion as soon as possible
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)

;; Allow typing in characters that don’t match the candidates
(setq company-require-match nil)

;; Easy navigation to candidates with M-<n>
(setq company-show-quick-access t)

;; Controls the maximum number of the candidates shown simultaneously in the tooltip
(setq company-tooltip-limit 16)

;; Don't allow to shrink tooltip
(setq company-tooltip-width-grow-only t)

;; Complete `abbrev' only in current buffer and make dabbrev case-sensitive
(setq company-dabbrev-other-buffers nil
      company-dabbrev-ignore-case nil
      company-dabbrev-downcase nil)

;; Make dabbrev-code case-sensitive
(setq company-dabbrev-code-ignore-case nil
      company-dabbrev-code-everywhere t)

;; Show annotations to the right side of the tooltip
(setq company-tooltip-align-annotations t)

;; Completion style, see
;; gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
(setq completion-styles '(substring orderless basic)
      orderless-component-separator "[ &]" ; press & to type another query in company popup
      completion-category-overrides '((file (styles basic partial-completion)))
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; Improve appearance for several orderless queries.
;; See https://github.com/oantolin/orderless?tab=readme-ov-file#company
(defun just-one-face (fn &rest args)
  (let ((orderless-match-faces [completions-common-part]))
    (apply fn args)))
(advice-add 'company-capf--candidates :around #'just-one-face)

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Suggest snippets in autocompletion.
(add-to-list 'company-backends '(company-capf :with company-yasnippet))

;; Enable indentation+tab insertion using the TAB key.
;; completion-at-point is bound to M-TAB.
(setq tab-always-indent nil)

;; Enable useful region commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Mac specific
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t
        mac-use-title-bar nil))

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

;; Enable drugging of lines and regions like Eclipse does
(drag-stuff-global-mode t)
(drag-stuff-define-keys)

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

; Focus new help windows when opened
(setq help-window-select t)

;; Delete selected text when starting to type over it
(setq delete-selection-mode t)

;; Automatically revert buffers for changed files if they don't have unsaved changes
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t
      dired-auto-revert-buffer t)

;; Enable centered cursor
(global-centered-cursor-mode)

;; Enable selection highlighting
(selection-highlight-mode)

;; Highlight changes if version control enabled
(global-diff-hl-mode)

;; Highlight TODO keywords
(global-hl-todo-mode)

;; Better work with trailing whitespaces and line endings
(setq mode-require-final-newline nil)
(add-hook 'org-mode-hook #'ws-butler-mode)
(add-hook 'prog-mode-hook #'ws-butler-mode)

;; Enable convenient comment/uncomment shortcut
(evilnc-default-hotkeys t)

;; Skip system buffers when cycling through open buffers
(set-frame-parameter (selected-frame) 'buffer-predicate
                     (lambda (buf)
                       (let ((name (buffer-name buf)))
                         (not (or (string-prefix-p "*" name)
                                  (eq 'dired-mode (buffer-local-value 'major-mode buf)))))))

;; Enable tree-sitter integrated into Emacs >= 29
(setq treesit-auto-install 'prompt)
(global-treesit-auto-mode)
;; Reset langs list to fill it in the dedicated modules
;; except these that doesn't have one.
(setq treesit-auto-langs '(perl))

;; Request maximum detalization
(customize-set-variable 'treesit-font-lock-level 4)

;; Report something goes wrong during code auto formatting
(setq format-all-show-errors 'never)

;; Ensure that default formatter is selected
(add-hook 'format-all-mode-hook #'format-all-ensure-formatter)

;; Save abbreviations whenever files are saved
(setq save-abbrevs 'silently)

;; Tell Emacs where to read abbrev definitions from
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

;; Enable embedded abbrev mode
(setq-default abbrev-mode t)

;; Specify place to store downloaded RFCs
(setq rfc-mode-directory (expand-file-name nano-rfc-directory))

(defun nano-pulse-current-region (&rest _)
  "Pulse the current implicit or active region."
  (if mark-active
      (pulse-momentary-highlight-region (region-beginning) (region-end))
    (pulse-momentary-highlight-region (mark) (point))))

(advice-add #'kill-ring-save :before #'nano-pulse-current-region)

(provide 'nano-defaults)
;;; nano-defaults.el ends here
