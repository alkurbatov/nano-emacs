;;; nano-faces.el --- Main faces of nano theme

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'nano-colors)

(defun nano-what-faces (pos)
  "Get the font faces at POS."
  (interactive "d")
  (let ((faces (remq nil
                     (list
                      (get-char-property pos 'read-face-name)
                      (get-char-property pos 'face)
                      (plist-get (text-properties-at pos) 'face)))))
    (message "Faces: %s" faces)))

(defface nano-face-org-green-project nil
  "Nano face for personal projects."
  :group 'nano)

(defface nano-face-org-red-project nil
  "Nano face for must-do-or-be-fired projects."
  :group 'nano)

(defun nano-faces ()
  "Derive face attributes for `nano-faces'."
  (set-face-attribute 'nano-face-org-green-project nil
                      :foreground (nord-color "aurora-3"))

  (set-face-attribute 'nano-face-org-red-project nil
                      :foreground (nord-color "aurora-0")))

(provide 'nano-faces)
;;; nano-faces.el ends here
