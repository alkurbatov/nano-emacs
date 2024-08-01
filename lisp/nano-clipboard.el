;;; nano-clipboard.el --- Copy/paste in terminal -*- lexical-binding: t; -*-

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
;; Linux part taken from https://www.emacswiki.org/emacs/CopyAndPaste

;;; Code:
(defun copy-from-osx (text &optional push)
  "Copy TEXT using macOS tools."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun paste-to-osx ()
  "Paste text using macOS tools."
  (shell-command-to-string "pbpaste"))

(setq wl-copy-process nil)

(defun copy-from-linux (text)
  "Copy TEXT using Wayland tools."
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe
                                      :noquery t))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))

(defun paste-to-linux ()
  "Paste text using Wayland tools."
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))

(unless (display-graphic-p)
  (cond
   ((string-equal system-type "darwin")
    (setq interprogram-cut-function 'copy-from-osx)
    (setq interprogram-paste-function 'paste-to-osx))
   ((string-equal system-type "gnu/linux")
    (setq interprogram-cut-function 'copy-from-linux)
    (setq interprogram-paste-function 'paste-to-linux))))

(provide 'nano-clipboard)
;;; nano-clipboard.el ends here
