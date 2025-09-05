;;; nano-clipboard.el --- Copy/paste in terminal -*- lexical-binding: t; -*-

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
;; Linux part taken from https://www.emacswiki.org/emacs/CopyAndPaste

;;; Code:
(require 'nano-settings)

(defun copy-from-osx (text &optional _push)
  "Copy TEXT using macOS tools."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun paste-to-osx ()
  "Paste text using macOS tools."
  (shell-command-to-string "pbpaste"))

(defun copy-from-linux-wayland (text &optional _push)
  "Copy TEXT using wl-clipboard."
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "wl-copy" nil 0 nil "-f" "-n")))

(defun paste-to-linux-wayland ()
  "Paste text using wl-clipboard."
  (let ((tramp-mode nil)
        (default-directory "~"))
    (setq-local xsel-output (shell-command-to-string "wl-paste -n | tr -d \r"))
    (unless (string= (car kill-ring) xsel-output)
      xsel-output )))

(unless (display-graphic-p)
  (cond
   (os-macos
    (setq interprogram-cut-function 'copy-from-osx)
    (setq interprogram-paste-function 'paste-to-osx))
   (os-linux
    (setq interprogram-cut-function 'copy-from-linux-wayland)
    (setq interprogram-paste-function 'paste-to-linux-wayland))))

;; Save the existing system clipboard text into the kill ring before replacing it.
;; See https://srijan.ch/notes/2024-09-24-001
(setq save-interprogram-paste-before-kill t)

(setq select-enable-clipboard t)

(provide 'nano-clipboard)
;;; nano-clipboard.el ends here
