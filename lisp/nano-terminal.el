;;; nano-terminal.el --- Terminal configurations

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

;; Mouse active in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (bind-keys*
   ("<mouse-4>" . scroll-down-line)
   ("<mouse-5>" . scroll-up-line)))

;; Mask Eat as xterm to make it compatible with OS X tools
(setq eat-term-name "xterm-256color")

;; Close terminal without questions
(setq eat-kill-buffer-on-exit t
      eat-query-before-killing-running-terminal nil)

(add-hook 'eat-mode-hook #'nano-modeline-eat-mode)
(add-hook 'eat-mode-hook #'compilation-shell-minor-mode)

(provide 'nano-terminal)
;;; nano-terminal.el ends here
