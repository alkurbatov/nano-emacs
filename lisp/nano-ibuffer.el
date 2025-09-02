;;; nano-ibuffer.el --- iBuffer configuration -*- lexical-binding: t; -*-

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

;; Show all buffers grouped list in separate window.
;; Heavily inspired by https://olddeuteronomy.github.io/post/emacs-ibuffer-config/

;;; Code:

(use-package ibuffer :ensure nil
  :config
    (setq ibuffer-expert t
          ibuffer-display-summary nil
          ibuffer-use-other-window nil
          ibuffer-show-empty-filter-groups nil
          ibuffer-default-sorting-mode 'filename/process
          ibuffer-title-face 'font-lock-doc-face
          ibuffer-use-header-line t
          ibuffer-default-shrink-to-minimum-size nil)

    (setq ibuffer-formats
          '((mark modified read-only locked " "
                  (name 30 30 :left :elide)
                  " "
                  (size 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " " filename-and-process)
            (mark " "
                  (name 16 -1)
                  " " filename)))

    (setq ibuffer-saved-filter-groups
          '(("Main"
             ("Directories" (mode . dired-mode))
             ("C++" (or
                 (mode . c++-mode)
                 (mode . c++-ts-mode)
                 (mode . c-mode)
                 (mode . c-ts-mode)
                 (mode . c-or-c++-ts-mode)))
             ("Python" (or
                 (mode . python-ts-mode)
                 (mode . c-mode)
                 (mode . python-mode)))
             ("Build" (or
                 (mode . make-mode)
                 (mode . makefile-gmake-mode)
                 (name . "^CMakeLists.txt$")
                 (name . "^Makefile$")
                 (mode . change-log-mode)))
             ("Scripts" (or
                 (mode . shell-script-mode)
                 (mode . shell-mode)
                 (mode . sh-mode)
                 (mode . lua-mode)))
             ("Config" (or
                 (mode . conf-mode)
                 (mode . conf-toml-mode)
                 (mode . toml-ts-mode)
                 (name . "^\\.clangd$")
                 (name . "^\\.gitignore$")
                 (name . "^Doxyfile$")
                 (name . "^config\\.toml$")
                 (mode . yaml-mode)))
             ("Web" (or
                 (mode . mhtml-mode)
                 (mode . html-mode)
                 (mode . web-mode)
                 (mode . nxml-mode)))
             ("CSS" (or
                 (mode . css-mode)
                 (mode . sass-mode)))
             ("JS" (or
                 (mode . js-mode)
                 (mode . rjsx-mode)))
             ("Markup" (or
                 (mode . markdown-mode)
                 (mode . gfm-mode)
                 (mode . adoc-mode)))
             ("Org"  (mode . org-mode))
             ("Apps" (or
                 (mode . elfeed-search-mode)
                 (mode . elfeed-show-mode)))
             ("Emacs" (or
                 (mode . emacs-lisp-mode)
                 (name . "^\\*Async")
                 (name . "^\\*Backtrace\\*$")
                 (name . "^\\*Custom.*")
                 (name . "^\\*EGLOT")
                 (name . "^\\*Help\\*$")
                 (name . "^\\*Native-compile-Log\\*$")
                 (name . "^\\*Messages\\*$")
                 (name . "^\\*Org Agenda\\*$")
                 (name . "^\\*info\\*$")
                 (name . "^\\*scratch\\*$")
                 (name . "^\\*straight-process\\*$")))
             ("Fundamental" (or
                 (mode . fundamental-mode)
                 (mode . text-mode)))
             )))

    :hook
    (ibuffer-mode . (lambda ()
                      (ibuffer-switch-to-saved-filter-groups "Main")))

    :bind
    (("C-x C-b" . ibuffer)))

(provide 'nano-ibuffer)
;;; nano-ibuffer.el ends here
