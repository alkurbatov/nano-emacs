;;; nano-terminal.el --- Terminal configurations

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2023 - N Λ N O developers

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

(defun nano-iterm-here ()
  "Open an iTerm (OSX) and go to the curent directory."
  (interactive)

  (shell-command "open -a iTerm $PWD" nil nil))

;; Mouse active in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (bind-keys*
   ("<mouse-4>" . 'scroll-down-line)
   ("<mouse-5>" . 'scroll-up-line)))

;; Default shell in term
(unless
    (or (eq system-type 'windows-nt)
        (not (file-exists-p "/bin/zsh")))
  (setq-default shell-file-name "/bin/zsh")
  (setq explicit-shell-file-name "/bin/zsh"))

;; Kill term buffer when exiting
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Fix bug on OSX in term mode & zsh (spurious % after each command)
(add-hook 'term-mode-hook
      (lambda () (setq buffer-display-table (make-display-table))))

(provide 'nano-terminal)
;;; nano-terminal.el ends here
