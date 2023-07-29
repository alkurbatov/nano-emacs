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

;; Show buffers list in separate window
(bind-key "C-x C-b" 'ibuffer)

;; Show recently visited buffer but not the current one
(bind-key* "C-c `" 'mode-line-other-buffer)

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

(bind-key "C-h f"   #'helpful-callable) ; Look up callable
(bind-key "C-h v"   #'helpful-variable) ; Look up variable
(bind-key "C-h k"   #'helpful-key)      ; Look up key
(bind-key "C-c C-d" #'helpful-at-point) ; Look up the current symbol at point
(bind-key "C-h F"   #'helpful-function) ; Look up *F*unctions (excludes macros).
(bind-key "C-h C"   #'helpful-command)  ; Look up *C*ommands.

;; Avy
(bind-key "M-g f" #'avy-goto-line)
(bind-key "M-g w" #'avy-goto-word-1)
(bind-key "M-g c" #'avy-goto-char)
(bind-key "C-c l" #'avy-copy-line)
(bind-key "C-c m" #'avy-move-line)

(provide 'nano-bindings)
;;; nano-bindings.el ends here
