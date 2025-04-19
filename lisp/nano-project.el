;;; nano-project.el --- Project management -*- lexical-binding: t; -*-

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

;; Project management for Emacs.

;;; Code:
(require 'bind-key)
(require 'project)

(defgroup project-local nil
  "Local, non-VC-backed project.el root directories.
Inspired by: https://christiantietze.de/posts/2022/03/mark-local-project.el-directories/"
  :group 'project)

(cl-defmethod project-root ((project (head local)))
  "Return root directory of current PROJECT."
  (cdr project))

(defun project-local-try-local (dir)
  "Determine if DIR is a non-VC project.
DIR must include a file .project"
  (if-let ((root (locate-dominating-file dir ".project")))
      (cons 'local root)))

(customize-set-variable 'project-find-functions
                        (list #'project-rootfile-try-detect
                              #'project-local-try-local))

;; Switch project in the same way as projectile does
(setq project-switch-commands 'project-find-file)

(defvar nano-project-test-command "make test")

(defun project-run-tests ()
  "Run unit tests in the project root."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
        (async-shell-command nano-project-test-command)))


;; Key bindings
(bind-keys :map project-prefix-map
           ("b" . consult-project-buffer)
           ("D" . project-find-dir)
           ("d" . project-dired)
           ("s" . consult-ripgrep)
           ("t" . eat-project)
           ("T" . project-run-tests))

(provide 'nano-project)
;;; nano-project.el ends here
