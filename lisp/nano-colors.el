;;; nano-colors.el --- Collection of color palettes

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

;; A collection of color palettes from Nord theme.

;; Usage:
;;  (require 'nano-colors)
;;  (nord-color "nord-0")

;;; Code:

;; See https://www.nordtheme.com/
(defconst nord-colors
  '( ("polar-night" . (list "#2E3440" "#3B4252" "#434C5E" "#4C566A"))
     ("snow-storm"  . (list "#D8DEE9" "#E5E9F0" "#ECEFF4"))
     ("frost"       . (list "#8FBCBB" "#88C0D0" "#81A1C1" "#5E81AC"))
     ("aurora"      . (list "#BF616A" "#D08770" "#EBCB8B" "#A3BE8C" "#B48EAD"))
     ("nord"        . (list "#2E3440" "#3B4252" "#434C5E" "#4C566A"
	                        "#D8DEE9" "#E5E9F0" "#ECEFF4" "#8FBCBB"
		                    "#88C0D0" "#81A1C1" "#5E81AC" "#BF616A"
		                    "#D08770" "#EBCB8B" "#A3BE8C" "#B48EAD"))))

(defun nord-color (color)
  "Pick RGB value of provided COLOR from the Nord theme."
  (interactive)
  (let ((hue   (substring color 0 -2))
        (level (string-to-number (substring color -1))))
    (nth (+ 1 level) (cdr (assoc hue nord-colors)))))

(provide 'nano-colors)
;;; nano-colors.el ends here
