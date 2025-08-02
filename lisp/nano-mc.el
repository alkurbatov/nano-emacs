;;; nano-mc.el --- Experimental Dired config inspiered by Midnight Commander -*- lexical-binding: t; -*-

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
(require 'bind-key)
(require 'dired-quick-sort)
(require 'ls-lisp)
(require 'nano-settings)

(defun nano-dired-find-file ()
  "Open file or directory and show its canonical path even if it was symlink.
Kudos to: https://www.youtube.com/watch?v=59XPGvJMggY&list=WL&index=95&t=12s"
  (interactive)

  (let ((original (dired-get-file-for-visit)))
    (if (file-directory-p original)
        (find-alternate-file (file-truename original))
      (find-file original))))

(defun nano-open-in-external-app (&optional Fname)
  "Open the current file or Dired marked files in external app.
When called in Emacs Lisp, if FNAME is given, open that.

Taken from:
`http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2019-11-04 2023-04-05 2023-06-26."
  (interactive)
  (let (xfileList xdoIt)
    (setq xfileList
          (if Fname
              (list Fname)
            (if (eq major-mode 'dired-mode)
                (dired-get-marked-files)
              (list buffer-file-name))))
    (setq xdoIt (if (<= (length xfileList) 10) t (y-or-n-p "Open more than 10 files? ")))
    (when xdoIt
      (cond
       (os-macos
        (mapc (lambda (xfpath) (shell-command (concat "open " (shell-quote-argument xfpath)))) xfileList))
       (os-linux
        (mapc (lambda (xfpath)
                (call-process shell-file-name nil 0 nil
                              shell-command-switch
                              (format "%s %s"
                                      "xdg-open"
                                      (shell-quote-argument xfpath))))
              xfileList))))))

(defun nano-open-dired ()
  "Open Dired in current folder with additional tweaks."
  (interactive)

  (dired "."))

;; Enable async commands processing in Dired, available since Emacs 29.
(setq dired-async-mode 1)

(with-eval-after-load 'dired
  ;; Assume that we should use another Dired buffer (opened in a window nearby)
  ;; as target of all operations
  (setq dired-dwim-target t)

  ;; Always do recursive copies without questions
  (setq dired-recursive-copies 'always)

  ;; Use GNU ls on OS X
  (when os-macos
    (setq ls-lisp-use-insert-directory-program t
          insert-directory-program (concat nano-brew-path "/opt/coreutils/bin/gls")))

  ;; Tweak displayed fields
  (setq dired-listing-switches
        (combine-and-quote-strings '("-GlahFk"
                                   "--time-style=+%d %b %Y")))

  ;; Setup quick sort
  (dired-quick-sort-setup)
  (setq dired-quick-sort-group-directories-last ?y) ; group directories together

  ;; Tweak files and folders deletion
  (setq dired-recursive-deletes 'always)

  ;; Auto refresh Dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)

  (bind-keys :map dired-mode-map
           ("RET" . nano-dired-find-file)
           ("M-u" . dired-up-directory)
           ("M-RET" . nano-open-in-external-app)))

;; Highlight current line for convenience
(add-hook 'dired-mode-hook #'hl-line-mode)

(bind-key "C-x d" #'nano-open-dired)

(provide 'nano-mc)
;;; nano-mc.el ends here
