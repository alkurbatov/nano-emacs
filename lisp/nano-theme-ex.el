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

(defun nano-theme--basics ()
  "Customize basic Emacs faces and nano-faces."

  ;; Increase font size (default is 140) enforced by nano-theme
  (set-face-attribute 'default nil :height 150)

  (set-face-attribute 'show-paren-match nil
                      :foreground nano-dark-popout))

(defun nano-theme--font-lock ()
  "Customize font-lock faces."
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
    (set-face-attribute 'company-tooltip-annotation-selection nil
                        :background nano-dark-salient)))

(defun nano-theme--ivy ()
  "Customize ivy faces."
  (with-eval-after-load 'ivy
    (set-face-attribute 'ivy-minibuffer-match-face-1 nil
                        :background nano-dark-background
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'ivy-minibuffer-match-face-2 nil
                        :background nano-dark-background
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'ivy-minibuffer-match-face-3 nil
                        :background nano-dark-background
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'ivy-minibuffer-match-face-4 nil
                        :background nano-dark-background
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'ivy-current-match nil
                        :background nano-dark-highlight
                        :foreground nano-dark-foreground)))

(defun nano-theme--tree-sitter ()
  "Customize tree-sitter faces."
  (with-eval-after-load 'tree-sitter-hl
    (set-face-attribute 'tree-sitter-hl-face:constructor nil
                        :foreground (nord-color "frost-1")
                        :weight 'regular)
    (set-face-attribute 'tree-sitter-hl-face:constant.builtin nil
                        :foreground (nord-color "aurora-2"))
    (set-face-attribute 'tree-sitter-hl-face:function.call nil
                        :foreground (nord-color "frost-1")
                        :weight 'regular)
    (set-face-attribute 'tree-sitter-hl-face:function.special nil
                        :foreground nano-dark-popout
                        :weight 'regular)
    (set-face-attribute 'tree-sitter-hl-face:method.call nil
                        :foreground (nord-color "frost-1"))
    (set-face-attribute 'tree-sitter-hl-face:number nil
                        :foreground (nord-color "aurora-4"))
    (set-face-attribute 'tree-sitter-hl-face:type.builtin nil
                        :foreground (nord-color "frost-0"))

    ;; Workaround bug in tree-sitter-langs causing wrong face on decorators
    (tree-sitter-hl-add-patterns 'python
      [(decorator (call (identifier) @function.special))])
    ))

(defun nano-theme--makefile ()
  "Customize makefile mode faces."
  (with-eval-after-load 'make-mode
    (set-face-attribute 'makefile-targets nil
                        :foreground (nord-color "frost-2"))
    ))

(defun nano-theme--flycheck ()
  "Derive flycheck faces."
  (with-eval-after-load 'flycheck
     (set-face-attribute 'flycheck-error nil
                         :background (nord-color "aurora-0")
                         :foreground nano-dark-background
                         :underline 'unspecified)
     (set-face-attribute 'flycheck-warning nil
                         :background nano-dark-critical
                         :foreground nano-dark-background
                         :underline 'unspecified)
     (set-face-attribute 'flycheck-info nil
                         :background nano-dark-salient
                         :foreground nano-dark-background
                         :underline 'unspecified)))

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
    (set-face-attribute 'markdown-header-delimiter-face nil
                        :foreground nano-dark-salient)
    (set-face-attribute 'markdown-language-keyword-face nil
                        :foreground (nord-color "frost-0"))
    (set-face-attribute 'markdown-link-face nil
                        :foreground (nord-color "aurora-3"))
    (set-face-attribute 'markdown-markup-face nil
                        :foreground nano-dark-salient)
    (set-face-attribute 'markdown-url-face nil
                        :foreground (nord-color "aurora-3"))))

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

(defun nano-theme-customize ()
  "Customize many, many faces."
  (nano-faces)
  (nano-theme--basics)
  (nano-theme--company)
  (nano-theme--ivy)
  (nano-theme--font-lock)
  (nano-theme--flycheck)
  (nano-theme--flyspell)
  (nano-theme--tree-sitter)
  (nano-theme--makefile)
  (nano-theme--markdown)
  (nano-theme--diff-hl)
  (nano-theme--hl-todo))


(provide 'nano-theme-ex)
;;; nano-theme-ex.el ends here
