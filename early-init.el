;;; early-init.el --- Early initialization routine -*- lexical-binding: t; -*-

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

;; The very first initialization code Emacs runs. Used for lowlevel
;; optimizations and special settings.

;;; Code:

(setq
 site-run-file nil                         ; No site-wide run-time initializations.
 inhibit-default-init t                    ; No site-wide default library
 package-enable-at-startup nil)            ; We'll use straight.el

;; Tweak garbage collector to make startup faster.
;; See https://emacsconf.org/2023/talks/gc/
(setq gc-cons-threshold (* 80 1024 1024))

;; Reset garbage collector limit after the init process has ended (800Kb)
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold 800000)))

(setq native-comp-eln-load-path
      (list (expand-file-name "eln-cache" user-emacs-directory)))

;; Start Emacs maximized without scrollbars.
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (tool-bar-lines . 0)))

;; During startup, Emacs doesn't require specific file handlers for every file
;; it opens or loads; thus, we should unset this list to optimize the startup process.
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist startup/file-name-handler-alist)
            (makunbound 'startup/file-name-handler-alist)))

;; No startup screen.
(setopt inhibit-startup-screen t)

;; No menu bar.
(menu-bar-mode -1)

;; Suppress the startup, scratch buffer will be home buffer.
(setq inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

;; Disable modeline while loading.
(setq mode-line-format nil)

;; No scroll bars.
(if (fboundp 'scroll-bar-mode)
    (setopt scroll-bar-mode nil))

;;; early-init.el ends here
