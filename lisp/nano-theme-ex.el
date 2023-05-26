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

;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset FACE and make it inherit STYLE."
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    style))


(defun nano-theme--basics ()
  "Customize basic Emacs faces and nano-faces."
  (set-face 'show-paren-match 'nano-popout))

(defun nano-theme--font-lock ()
  "Customize font-lock faces from nano-faces."
  (set-face-attribute 'font-lock-string-face nil
                      :foreground (nord-color "aurora-3"))
  (set-face-attribute 'font-lock-variable-name-face nil
                      :weight 'regular))

(defun nano-theme--tree-sitter ()
  "Derive tree-sitter faces from nano-faces."
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

    ;; Workaround bug in tree-sitter-langs causing wrong face on decorators
    (tree-sitter-hl-add-patterns 'python
      [(decorator (call (identifier) @function.special))])
    ))

(defun nano-theme--flycheck ()
  "Derive flycheck faces from nano faces."
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

(defun nano-theme--hl-todo ()
  "Customize hl-todo colors."
  (with-eval-after-load 'hl-todo
    (setq hl-todo-highlight-punctuation ":"
          hl-todo-keyword-faces
          `(("TODO"       . nano-popout)
            ("FIXME"      . nano-critical)
            ("NB"         . nano-salient)
            ("NOTE"       . nano-salient)))))


(defun nano-theme-customize ()
  "Customize many, many faces from the core nano faces."
  (nano-theme--basics)
  (nano-theme--font-lock)
  (nano-theme--flycheck)
  (nano-theme--tree-sitter)
  (nano-theme--hl-todo))


(provide 'nano-theme-ex)
;;; nano-theme-ex.el ends here
