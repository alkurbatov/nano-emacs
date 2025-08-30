;;; nano-spellcheck.el --- Setup spell checking -*- lexical-binding: t; -*-

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

;; Enable spellchecking with hunspell
;; Inspired by:
;; https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html

;;; Code:
(require 'bind-key)

(with-eval-after-load 'ispell
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US,ru_RU")

  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.hunspell_personal")

  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,ru_RU")

  ;; Do not ask for confirmation when modifying personal dictionary.
  (setq ispell-silently-savep t)

  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))

(with-eval-after-load 'jinx
  (setq jinx-languages "en_US ru_RU")

  (bind-key "M-4" #'jinx-correct))

;; Enable spellcheck in all modes
(add-hook 'emacs-startup-hook #'global-jinx-mode)

(provide 'nano-spellcheck)
;;; nano-spellcheck.el ends here
