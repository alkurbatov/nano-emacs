;;; nano-font.el --- Font configuration -*- lexical-binding: t; -*-

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2023-2024 - N Λ N O developers

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
(require 'ligature)

(defface nano-mono
  '((t (:family "JetBrains Mono"
        :height 130
        :weight light)))
  "Default monospaced font (JetBrains Mono Light, 13pt)."
  :group 'nano-fonts)

(defface nano-italic
  '((t (:family "JetBrains Mono"
        :slant italic
        :height 130
        :weight regular)))
  "Default italic font (JetBrains Mono Italic Light, 13pt)."
  :group 'nano-fonts)

;; Enable font ligatures
(defconst nano-jetbrains-ligatures
  '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
    "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
    "|||>" "<|||" ">>" "<<" nil nil "::=" "|]" "[|" "{|" "|}"
    "[<" ">]" ":?>" ":?" nil "/=" "[||]" "!!" "?:" "?." "::"
    "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
    "<:" ":<" ":>" ">:" "<>" nil ";;" "/==" ".=" ".-" "__"
    "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
    ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
    "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
    "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
    "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
    "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
    "&="))

(ligature-set-ligatures 'prog-mode nano-jetbrains-ligatures)

(provide 'nano-font)
;;; nano-font.el ends here
