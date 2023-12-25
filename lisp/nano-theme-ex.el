;;; nano-theme-ex.el --- Customizes various aspects of Nano theme

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

;;; Code:
(require 'nano-colors)
(require 'nano-faces)
(require 'nano-theme)

(defun nano-theme--basics ()
  "Customize basic Emacs faces and `nano-faces'."

  ;; Increase font size (default is 140, enforced by nano-theme)
  (set-face-attribute 'default nil :height 150)

  (set-face-attribute 'show-paren-match nil
                      :foreground nano-dark-popout))

(defun nano-theme--font-lock ()
  "Customize font-lock faces."
  (set-face-attribute 'font-lock-bracket-face nil
                      :foreground (nord-color "frost-0"))
  (set-face-attribute 'font-lock-builtin-face nil
                      :foreground (nord-color "frost-0"))
  (set-face-attribute 'font-lock-escape-face nil
                      :foreground (nord-color "aurora-2")
                      :weight 'regular)
  (set-face-attribute 'font-lock-function-name-face nil
                      :foreground (nord-color "frost-1")
                      :weight 'regular)
  (set-face-attribute 'font-lock-keyword-face nil
                      :foreground (nord-color "frost-2"))
  (set-face-attribute 'font-lock-number-face nil
                      :foreground (nord-color "aurora-4"))
  (set-face-attribute 'font-lock-operator-face nil
                      :foreground (nord-color "frost-2"))
  (set-face-attribute 'font-lock-property-use-face nil
                      :foreground (nord-color "frost-1"))
  (set-face-attribute 'font-lock-string-face nil
                      :foreground (nord-color "aurora-3"))
  (set-face-attribute 'font-lock-type-face nil
                      :foreground (nord-color "frost-0"))
  (set-face-attribute 'font-lock-variable-name-face nil
                      :weight 'regular))

(defun nano-theme--company ()
  "Customize company tooltip window."
  (with-eval-after-load 'company
    (set-face-attribute 'company-tooltip-common nil
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'company-tooltip-common-selection nil
                        :background nano-dark-faded
                        :foreground (nord-color "frost-0")
                        :weight 'bold)
    (set-face-attribute 'company-tooltip-selection nil
                        :background nano-dark-faded
                        :foreground nano-dark-strong
                        :weight 'bold)))

(defun nano-theme--ivy ()
  "Customize ivy faces."
  (with-eval-after-load 'ivy
    (set-face-attribute 'ivy-minibuffer-match-face-1 nil
                        :background 'unspecified
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'ivy-minibuffer-match-face-2 nil
                        :background 'unspecified
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'ivy-minibuffer-match-face-3 nil
                        :background 'unspecified
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'ivy-minibuffer-match-face-4 nil
                        :background 'unspecified
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'ivy-current-match nil
                        :background nano-dark-faded
                        :foreground nano-dark-strong
                        :weight 'bold)))

(defun nano-theme--makefile ()
  "Customize `makefile-mode' faces."
  (with-eval-after-load 'make-mode
    (set-face-attribute 'makefile-targets nil
                        :foreground (nord-color "frost-2"))))

(defun nano-theme--flymake ()
  "Derive flymake faces."
  (with-eval-after-load 'flymake
     (set-face-attribute 'flymake-error nil
                         :background (nord-color "aurora-0")
                         :foreground nano-dark-foreground
                         :underline 'unspecified)
     (set-face-attribute 'flymake-error-echo nil
                         :foreground (nord-color "aurora-0"))
     (set-face-attribute 'flymake-warning nil
                         :background nano-dark-critical
                         :foreground nano-dark-background
                         :underline 'unspecified)
     (set-face-attribute 'flymake-warning-echo nil
                         :foreground nano-dark-critical)
     (set-face-attribute 'flymake-note nil
                         :background nano-dark-salient
                         :foreground nano-dark-background
                         :underline 'unspecified)
     (set-face-attribute 'flymake-note-echo nil
                         :foreground nano-dark-salient)))

(defun nano-theme--diff-hl ()
  "Customize diff-hl colors."
  (with-eval-after-load 'diff-hl
    (set-face-attribute 'diff-hl-insert nil
                        :background (nord-color "aurora-3")
                        :foreground (nord-color "aurora-3"))
    (set-face-attribute 'diff-hl-change nil
                        :background (nord-color "aurora-2")
                        :foreground (nord-color "aurora-2"))
    (set-face-attribute 'diff-hl-delete nil
                        :background (nord-color "aurora-0")
                        :foreground (nord-color "aurora-0")))

  (with-eval-after-load 'diff-hl-margin
    (set-face-attribute 'diff-hl-margin-insert nil
                        :background nano-dark-background
                        :foreground (nord-color "aurora-3"))
    (set-face-attribute 'diff-hl-margin-change nil
                        :background nano-dark-background
                        :foreground (nord-color "aurora-2"))
    (set-face-attribute 'diff-hl-margin-delete nil
                        :background nano-dark-background
                        :foreground (nord-color "aurora-0"))
    ))

(defun nano-theme--hl-todo ()
  "Customize hl-todo colors."
  (with-eval-after-load 'hl-todo
    (setq hl-todo-highlight-punctuation ":"
          hl-todo-keyword-faces
          `(("TODO"       . nano-popout)
            ("FIXME"      . nano-critical)
            ("NB"         . nano-salient)
            ("NOTE"       . nano-salient)))))

(defun nano-theme--markdown ()
  "Customize markdown colors."
  (with-eval-after-load 'markdown-mode
    (set-face-attribute 'markdown-inline-code-face nil
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'markdown-header-delimiter-face nil
                        :foreground (nord-color "frost-2"))
    (set-face-attribute 'markdown-header-face-1 nil
                        :foreground (nord-color "frost-1"))
    (set-face-attribute 'markdown-header-face-2 nil
                        :foreground (nord-color "frost-1"))
    (set-face-attribute 'markdown-header-face-3 nil
                        :foreground (nord-color "frost-1"))
    (set-face-attribute 'markdown-language-keyword-face nil
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'markdown-link-face nil
                        :foreground (nord-color "frost-1"))
    (set-face-attribute 'markdown-markup-face nil
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'markdown-url-face nil
                        :foreground (nord-color "snow-storm-0")
                        :underline t)

    ))

(defun nano-theme--flyspell ()
  "Customize flyspell colors."
  (with-eval-after-load 'flyspell
    (set-face-attribute 'flyspell-duplicate nil
                         :background nano-dark-salient
                         :foreground nano-dark-background
                         :underline 'unspecified)
    (set-face-attribute 'flyspell-incorrect nil
                         :background nano-dark-salient
                         :foreground nano-dark-background
                         :underline 'unspecified)))

(defun nano-theme--pip-requirements ()
  "Customize pip-requirements mode faces."
  (with-eval-after-load 'pip-requirements
    (set-face-attribute 'font-lock-variable-name-face nil
                        :foreground (nord-color "frost-1"))
    (set-face-attribute 'font-lock-constant-face nil
                        :foreground (nord-color "aurora-2"))
    (set-face-attribute 'font-lock-builtin-face nil
                        :foreground nano-dark-foreground)))

(defun nano-theme--ethan-wspace ()
  "Customize ethan-wspace mode faces."
  (with-eval-after-load 'ethan-wspace
    (setq ethan-wspace-face-customized t)

    (set-face-attribute 'ethan-wspace-face nil
                        :background nano-dark-critical
                        :foreground 'unspecified)))

(defun nano-theme--highlight-indent-guides ()
  "Customize highlight-indent-guides mode colors."
  (with-eval-after-load 'highlight-indent-guides
    (setq highlight-indent-guides-auto-enabled nil)

    (setq highlight-indent-guides-method 'character
          highlight-indent-guides-character ?\|
          highlight-indent-guides-responsive 'top)

    (set-face-attribute 'highlight-indent-guides-character-face nil
                        :foreground (nord-color "polar-night-1"))
    (set-face-attribute 'highlight-indent-guides-top-character-face nil
                        :foreground (nord-color "polar-night-3"))))

(defun nano-theme--eglot ()
  "Derive eglot faces."
  (with-eval-after-load 'eglot
     (set-face-attribute 'eglot-diagnostic-tag-deprecated-face nil
                         :background nano-dark-faded
                         :foreground nano-dark-foreground
                         :underline 'unspecified)
     (set-face-attribute 'eglot-diagnostic-tag-unnecessary-face nil
                         :background nano-dark-salient
                         :foreground nano-dark-background
                         :underline 'unspecified)))

(defun nano-theme--dired ()
  "Derive Dired faces."
  (with-eval-after-load 'dired
     (set-face-attribute 'dired-directory nil
                         :foreground (nord-color "frost-2")
                         :weight 'bold)))

(defun nano-theme--whitespace ()
  "Derive `whitespace-mode' faces."
  (with-eval-after-load 'whitespace
    (set-face-attribute 'whitespace-newline  nil
                        :background nano-dark-background
                        :foreground nano-dark-faded)
    (set-face-attribute 'whitespace-missing-newline-at-eof nil
                        :background nano-dark-background
                        :foreground nano-dark-faded)
    (set-face-attribute 'whitespace-space nil
                        :background nano-dark-background
                        :foreground nano-dark-faded)
    (set-face-attribute 'whitespace-space-after-tab nil
                        :background nano-dark-background
                        :foreground nano-dark-faded)
    (set-face-attribute 'whitespace-space-before-tab nil
                        :background nano-dark-background
                        :foreground nano-dark-faded)
    (set-face-attribute 'whitespace-tab nil
                        :background nano-dark-background
                        :foreground nano-dark-faded)
    (set-face-attribute 'whitespace-trailing nil
                        :background nano-dark-critical
                        :foreground nano-dark-faded)))

(defun nano-theme--term ()
  "Derive `eat' faces."
  (with-eval-after-load 'term
    (set-face-attribute 'ansi-color-blue nil
                        :foreground (nord-color "frost-2"))
    (set-face-attribute 'ansi-color-bright-blue nil
                        :background (nord-color "frost-1"))
    (set-face-attribute 'ansi-color-cyan nil
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'ansi-color-bright-cyan nil
                        :background (nord-color "frost-0"))
    (set-face-attribute 'ansi-color-green nil
                        :foreground (nord-color "aurora-3"))
    (set-face-attribute 'ansi-color-bright-green nil
                        :background (nord-color "aurora-3"))
    (set-face-attribute 'ansi-color-magenta nil
                        :foreground (nord-color "aurora-4"))
    (set-face-attribute 'ansi-color-bright-magenta nil
                        :background (nord-color "aurora-4"))
    (set-face-attribute 'ansi-color-red nil
                        :foreground (nord-color "aurora-0"))
    (set-face-attribute 'ansi-color-bright-red nil
                        :background (nord-color "aurora-1"))
    (set-face-attribute 'ansi-color-yellow nil
                        :foreground (nord-color "aurora-2"))
    (set-face-attribute 'ansi-color-bright-yellow nil
                        :background (nord-color "aurora-2"))

    (set-face-attribute 'term-color-blue nil
                        :foreground (nord-color "frost-2")
                        :background (nord-color "frost-1"))
    (set-face-attribute 'term-color-cyan nil
                        :foreground (nord-color "frost-0")
                        :background (nord-color "frost-0"))
    (set-face-attribute 'term-color-green nil
                        :foreground (nord-color "aurora-3")
                        :background (nord-color "aurora-3"))
    (set-face-attribute 'term-color-magenta nil
                        :foreground (nord-color "aurora-4")
                        :background (nord-color "aurora-4"))
    (set-face-attribute 'term-color-red nil
                        :foreground (nord-color "aurora-0")
                        :background (nord-color "aurora-1"))
    (set-face-attribute 'term-color-yellow nil
                        :foreground (nord-color "aurora-2")
                        :background (nord-color "aurora-2"))))

(defun nano-theme--calendar ()
  "Derive `calendar' faces."
  (with-eval-after-load 'calendar
    (set-face-attribute 'holiday nil
                        :background 'unspecified
                        :foreground (nord-color "aurora-2"))))

(defun nano-theme-customize ()
  "Customize many, many faces."
  (nano-faces)
  (nano-theme--basics)
  (nano-theme--company)
  (nano-theme--diff-hl)
  (nano-theme--dired)
  (nano-theme--eglot)
  (nano-theme--ethan-wspace)
  (nano-theme--flymake)
  (nano-theme--flyspell)
  (nano-theme--font-lock)
  (nano-theme--highlight-indent-guides)
  (nano-theme--hl-todo)
  (nano-theme--ivy)
  (nano-theme--makefile)
  (nano-theme--markdown)
  (nano-theme--pip-requirements)
  (nano-theme--whitespace)
  (nano-theme--term)
  (nano-theme--calendar))

(provide 'nano-theme-ex)
;;; nano-theme-ex.el ends here
