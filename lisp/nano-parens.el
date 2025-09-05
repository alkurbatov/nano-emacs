;;; nano-parens.el --- Smartparents mode extended configuration -*- lexical-binding: t; -*-

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

;; Inspired by https://ebzzry.com/en/emacs-pairs/

;;; Code:
(require 'bind-key)
(require 'smartparens-config)

(defmacro nano-def-pairs (pairs)
  "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
  `(progn
     ,@(cl-loop for (key . val) in pairs
                collect
                `(defun ,(read (concat
                                "wrap-with-"
                                (prin1-to-string key)
                                "s"))
                     (&optional arg)
                   (interactive "p")
                   (sp-wrap-with-pair ,val)))))


(with-eval-after-load 'smartparens
  (nano-def-pairs ((paren . "(")
                   (bracket . "[")
                   (brace . "{")
                   (single-quote . "'")
                   (double-quote . "\"")))

  (bind-keys
   :map smartparens-mode-map
   ("C-c ("  . wrap-with-parens)
   ("C-c ["  . wrap-with-brackets)
   ("C-c {"  . wrap-with-braces)
   ("C-c '"  . wrap-with-single-quotes)
   ("C-c \"" . wrap-with-double-quotes)

   ("M-}"    . sp-backward-unwrap-sexp)
   ("M-]"    . sp-unwrap-sexp)))

;; Enable smarter parens handling
(show-smartparens-global-mode t)
(add-hook 'prog-mode-hook #'turn-on-smartparens-mode)

(provide 'nano-parens)
;;; nano-parens.el ends here
