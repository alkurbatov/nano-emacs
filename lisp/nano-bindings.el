;;; nano-bindings.el --- Various keyboard shortcuts

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'bind-key)

;; Kill current buffer (instead of asking first buffer name)
(bind-key* "C-x k" 'kill-current-buffer)

;; M-n for new frame (M-n is unbound in vanilla emacs)
(defun new-frame ()
  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))
(bind-keys*
 ("M-n" . new-frame)
 ("M-`" . other-frame))

;; Cmd-return for frame maximization toggle
(bind-key "<s-return>" 'toggle-frame-maximized)

;; Close frame if not the last, kill emacs else
(defun nano--delete-frame-or-kill-emacs ()
  "Delete frame or kill Emacs if there is only one frame."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-terminal)))
(bind-key* "C-x C-c" 'nano--delete-frame-or-kill-emacs)

;; Open recent files
(bind-key* "C-c r" 'recentf-open-files)

;; Don't press shift when undoing things
(bind-key* "C--" 'undo)

(provide 'nano-bindings)
;;; nano-bindings.el ends here
