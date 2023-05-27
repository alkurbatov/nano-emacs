;; nano.el --- Main entry point

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
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))


;; Customize support for 'emacs -q' (Optional)
;; You can enable customizations by creating the nano-custom.el file
;; with e.g. `touch nano-custom.el` in the folder containing this file.
(let* ((this-file  (or load-file-name (buffer-file-name)))
       (this-dir  (file-name-directory this-file))
       (custom-path  (concat this-dir "nano-custom.el")))
  (when (and (eq nil user-init-file)
             (eq nil custom-file)
             (file-exists-p custom-path))
    (setq user-init-file this-file)
    (setq custom-file custom-path)
    (load custom-file)))

;; Theme
(require 'nano-theme-ex)
(load-theme 'nano-dark t)
(nano-mode)
(nano-theme-customize)

;; Default layout
(require 'nano-layout)

;; Nano default settings
(require 'nano-defaults)

;; Nano session saving
(require 'nano-session)

;; Nano header & mode lines
(require 'nano-modeline)

;; Nano key bindings modification
(require 'nano-bindings)

;; Nano counsel configuration
(require 'nano-counsel)

;; Welcome message
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O for devs edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

;; Splash
(unless (member "-no-splash" command-line-args)
  (require 'nano-splash)
  (nano-splash))

;; Mu4e (optional)
;;(require 'nano-mu4e)

;; Projects management
(require 'nano-projectile)

;; Org mode
(require 'nano-org)

;; Version control
(require 'nano-git)

;; IDE features
(require 'nano-lsp)

;; Tools
(require 'nano-docker)

;; Programming languages
(require 'nano-elisp)
(require 'nano-python)
(require 'nano-sh)
(require 'nano-toml)

(provide 'nano)
;;; nano.el ends here
