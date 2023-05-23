;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------
(require 'nano-base-colors)
(require 'nano-colors)

(defun nano-theme-set-toothpaste ()
  "Apply toothpaste Nano theme base."
  ;; Colors from Toothpaste theme at https://github.com/toothpaste-theme/toothpaste
  (setq frame-background-mode     'dark)
  (setq nano-color-foreground (toothpaste-color "blue-gray-2"))
  (setq nano-color-background (toothpaste-color "blue-gray-0"))
  (setq nano-color-highlight  (toothpaste-color "yellow-0"))
  (setq nano-color-critical   (toothpaste-color "red-0"))
  (setq nano-color-salient    (toothpaste-color "green-1"))
  (setq nano-color-strong     (toothpaste-color "blue-0"))
  (setq nano-color-popout     (toothpaste-color "yellow-3"))
  (setq nano-color-subtle     (toothpaste-color "yellow-0"))
  (setq nano-color-faded      (toothpaste-color "blue-gray-1"))
  ;; to allow for toggling of the themes.
  (setq nano-theme-var "toothpaste"))

(provide 'nano-theme-toothpaste)
