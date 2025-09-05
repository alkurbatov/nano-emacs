;;; nano-appearance.el --- Theme and faces settings -*- lexical-binding: t; -*-

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2023-2025 - N Λ N O developers

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

;; Theme
(require 'nord-theme)
(nord-load-theme)

(defface nano-critical `((t (:foreground ,nord13 :weight normal)))
  "Critical face is for information that requires immediate action.
It should be of high contrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group nil)

(defface nano-faded `((t (:foreground ,nord-comment)))
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group 'nano)

(defface nano-faded-i `((t (:foreground ,nord0 :background ,nord-comment)))
  "Faded face inversed."
  :group nil)

(defface nano-popout `((t (:foreground ,nord12)))
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group 'nano)

(defface nano-popout-i `((t (:foreground ,nord0 :background ,nord12)))
  "Popout face inversed."
  :group 'nano)

(defface nano-salient `((t (:foreground ,nord9)))
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group 'nano)

(defface nano-subtle `((t (:foreground ,nord2)))
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group nil)

;; Apply fonts
(custom-theme-set-faces 'nord
                        `(default ((t (:foreground ,nord6
                                                   :weight     ,(face-attribute 'nano-mono :weight)
                                                   :height     ,(face-attribute 'nano-mono :height)
                                                   :family     ,(face-attribute 'nano-mono :family)))))
                        `(italic ((t (:foreground ,nord6
                                                  :weight     ,(face-attribute 'nano-italic :weight)
                                                  :height     ,(face-attribute 'nano-italic :height)
                                                  :slant      ,(face-attribute 'nano-italic :slant)
                                                  :family     ,(face-attribute 'nano-italic :family))))))

;; Line spacing (in pixels)
(setq line-spacing 1.2)

;; Customize indent-bars mode
(with-eval-after-load 'indent-bars
  ;; Enable TreeSitter support
  (setq indent-bars-treesit-support t)
  (setq indent-bars-treesit-ignore-blank-lines-types '("module"))

  ;; Make the indent-bars package decide when to use the stipple attribute
  ;; Stripple doesn't work on most macOS and Windows builds.
  (setq indent-bars-prefer-character
        (if (memq initial-window-system '(pgtk ns)) t))

  (setq
   indent-bars-color '(nano-faded-i :face-bg t :blend 0.55)
   indent-bars-pattern "..."
   indent-bars-width-frac 0.1
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth nil
   indent-bars-highlight-current-depth nil
   indent-bars-display-on-blank-lines nil))

;; Customize hl-todo colors
(with-eval-after-load 'hl-todo
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       . nano-popout)
          ("XXX"        . nano-popout)
          ("CRUTCH"     . nano-popout)
          ("FIXME"      . nano-critical)
          ("NB"         . nano-salient)
          ("NOTE"       . nano-salient))))

(provide 'nano-appearance)
;;; nano-appearance.el ends here
