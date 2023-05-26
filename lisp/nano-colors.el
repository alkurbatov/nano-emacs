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

;; A collection of color palettes from Material design
;; and Toothpaste theme

;; Usage:
;;  (require 'nano-colors)
;;  (material-color "deep-purple-2")
;;  (toothpaste-color "orange-0")

;;; Code:

;; See https://material.io/design/color/the-color-system.html
(defconst material-colors
  '(("red"         . (list "#FFEBEE" "#FFCDD2" "#EF9A9A" "#E57373" "#EF5350"
                           "#F44336" "#E53935" "#D32F2F" "#C62828" "#B71C1C"))
    ("pink"        . (list "#FCE4EC" "#F8BBD0" "#F48FB1" "#F06292" "#EC407A"
                           "#E91E63" "#D81B60" "#C2185B" "#AD1457" "#880E4F"))
    ("purple"      . (list "#F3E5F5" "#E1BEE7" "#CE93D8" "#BA68C8" "#AB47BC"
                           "#9C27B0" "#8E24AA" "#7B1FA2" "#6A1B9A" "#4A148C"))
    ("deep-purple" . (list "#EDE7F6" "#D1C4E9" "#B39DDB" "#9575CD" "#7E57C2"
                           "#673AB7" "#5E35B1" "#512DA8" "#4527A0" "#311B92"))
    ("indigo"      . (list "#E8EAF6" "#C5CAE9" "#9FA8DA" "#7986CB" "#5C6BC0"
                           "#3F51B5" "#3949AB" "#303F9F" "#283593" "#1A237E"))
    ("blue"        . (list "#E3F2FD" "#BBDEFB" "#90CAF9" "#64B5F6" "#42A5F5"
                           "#2196F3" "#1E88E5" "#1976D2" "#1565C0" "#0D47A1"))
    ("light-blue"  . (list "#E1F5FE" "#B3E5FC" "#81D4FA" "#4FC3F7" "#29B6F6"
                           "#03A9F4" "#039BE5" "#0288D1" "#0277BD" "#01579B"))
    ("cyan"        . (list "#E0F7FA" "#B2EBF2" "#80DEEA" "#4DD0E1" "#26C6DA"
                           "#00BCD4" "#00ACC1" "#0097A7" "#00838F" "#006064"))
    ("teal"        . (list "#E0F2F1" "#B2DFDB" "#80CBC4" "#4DB6AC" "#26A69A"
                           "#009688" "#00897B" "#00796B" "#00695C" "#004D40"))
    ("green"       . (list "#E8F5E9" "#C8E6C9" "#A5D6A7" "#81C784" "#66BB6A"
                           "#4CAF50" "#43A047" "#388E3C" "#2E7D32" "#1B5E20"))
    ("light-green" . (list "#F1F8E9" "#DCEDC8" "#C5E1A5" "#AED581" "#9CCC65"
                           "#8BC34A" "#7CB342" "#689F38" "#558B2F" "#33691E"))
    ("lime"        . (list "#F9FBE7" "#F0F4C3" "#E6EE9C" "#DCE775" "#D4E157"
                           "#CDDC39" "#C0CA33" "#AFB42B" "#9E9D24" "#827717"))
    ("yellow"      . (list "#FFFDE7" "#FFF9C4" "#FFF59D" "#FFF176" "#FFEE58"
                           "#FFEB3B" "#FDD835" "#FBC02D" "#F9A825" "#F57F17"))
    ("amber"       . (list "#FFF8E1" "#FFECB3" "#FFE082" "#FFD54F" "#FFCA28"
                           "#FFC107" "#FFB300" "#FFA000" "#FF8F00" "#FF6F00"))
    ("orange"      . (list "#FFF3E0" "#FFE0B2" "#FFCC80" "#FFB74D" "#FFA726"
                           "#FF9800" "#FB8C00" "#F57C00" "#EF6C00" "#E65100"))
    ("deep-orange" . (list "#FBE9E7" "#FFCCBC" "#FFAB91" "#FF8A65" "#FF7043"
                           "#FF5722" "#F4511E" "#E64A19" "#D84315" "#BF360C"))
    ("brown"       . (list "#EFEBE9" "#D7CCC8" "#BCAAA4" "#A1887F" "#8D6E63"
                           "#795548" "#6D4C41" "#5D4037" "#4E342E" "#3E2723"))
    ("grey"        . (list "#FAFAFA" "#F5F5F5" "#EEEEEE" "#E0E0E0" "#BDBDBD"
                           "#9E9E9E" "#757575" "#616161" "#424242" "#212121"))
    ("blue-grey"   . (list "#ECEFF1" "#CFD8DC" "#B0BEC5" "#90A4AE" "#78909C"
                           "#607D8B" "#546E7A" "#455A64" "#37474F" "#263238"))))

(defun material-color (color)
  (interactive)
  (let ((hue   (substring color 0 -2))
        (level (string-to-number (substring color -1))))
    (nth (+ 1 level) (cdr (assoc hue material-colors)))))



;; See http://toothpaste-theme.github.io/toothpaste/
(defconst toothpaste-colors
  '( ("gray" . (list "#C5C5C0"))
     ("blue-gray"  . (list "#222E33" "#2C3C42" "#465E68" "#DAE3E8"))
     ("yellow"     . (list "#3B3A32" "#3E3D32" "#49483E" "#DBCD7F"))
     ("blue"       . (list "#769EB3" "#96CBFE"))
     ("cyan"       . (list "#73B3C0"))
     ("green"      . (list "#97B853" "#9DC777" "#A8FF60"))
     ("orange"     . (list "#C88E4B" "#E9C062"))
     ("red"        . (list "#CC6666" "#E36868"))
     ("magenta"    . (list "#C5487A"))
     ("purple"     . (list "#9B82C9"))))
     
(defun toothpaste-color (color)
  (interactive)
  (let ((hue   (substring color 0 -2))
        (level (string-to-number (substring color -1))))
    (nth (+ 1 level) (cdr (assoc hue toothpaste-colors)))))

(provide 'nano-colors)
;;; nano-colors.el ends here
