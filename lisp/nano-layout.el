;;; nano-layout.el --- UI layout setup -*- lexical-binding: t; -*-

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020-2025 - N Λ N O developers

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
(require 'disp-table)
(require 'nano-modeline)
(require 'olivetti)

;; No startup  screen
(setq inhibit-startup-screen t)

;; No startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
(setq initial-buffer-choice nil)

;; No frame title
(setq frame-title-format nil)

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No empty line indicators
(setq indicate-empty-lines nil)

;; No cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; Text mode is initial mode
(setq initial-major-mode 'text-mode)

;; Text mode is default major mode
(setq default-major-mode 'text-mode)

;; Moderate font lock
(setq font-lock-maximum-decoration t)

;; No line break space points
(setq auto-fill-mode nil)

;; Fill column at 80
(setq fill-column 80)

;; Bar cursor
(setq-default cursor-type '(hbar .  2))
(setq-default cursor-in-non-selected-windows nil)
(setq blink-cursor-mode nil)

;; No toolbar
(tool-bar-mode -1)

;; Vertical window divider
(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; Nicer glyphs for continuation and wrap
(set-display-table-slot standard-display-table
                        'truncation (make-glyph-code ?… 'nano-faded))
(set-display-table-slot standard-display-table
                        'wrap (make-glyph-code ?- 'nano-faded))

;; Transparent Emacs titlebars on OSX (works only with emacs-plus)
(when (and os-macos (display-graphic-p))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; nil for dark text
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(add-hook 'prog-mode-hook     #'hl-line-mode)
(add-hook 'text-mode-hook     #'hl-line-mode)

;; Enable nano-modeline in different modes
(setq nano-modeline-alignment '(text . text)
      nano-modeline-borders '(nil . nil))
(nano-modeline nil "Top" t)

;; Display line numbers in most modes
(add-hook 'conf-unix-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook      #'display-line-numbers-mode)
(add-hook 'text-mode-hook      #'display-line-numbers-mode)

;; Enable margins for nice writing environment.
(defun setup-olivetti-mode ()
  "Setup and enable 'olivetti-mode'."
  (setq olivetti-minimum-body-width 120
        olivetti-body-width 120)

  (olivetti-mode))

(add-hook 'conf-unix-mode-hook #'setup-olivetti-mode)
(add-hook 'prog-mode-hook      #'setup-olivetti-mode)
(add-hook 'text-mode-hook      #'setup-olivetti-mode)

;; Enable dock integration with KDE/Gnome.
(when os-linux
  (require 'dock))

(provide 'nano-layout)
;;; nano-layout.el ends here
