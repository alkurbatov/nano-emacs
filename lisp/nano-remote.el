;;; nano-remote.el --- Configures remote work support (SSH, Tramp, etc) -*- lexical-binding: t; -*-

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
;;
;; Regarding TRAMP optimizations see:
;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
;;
;; Hints
;;
;; To open file under root user on remote machine do:
;; /ssh:remote-host|sudo::/path/to-file

;;; Code:
(require 'nano-settings)
(require 'tramp)

;; Keep remote backups with local.
(setq tramp-auto-save-directory nano-auto-save-directory
      tramp-backup-directory-alist `(("." . ,nano-backup-directory)))

(setq tramp-verbose 2                       ;; print only warnings and errors.
      tramp-use-scp-direct-remote-copying t ;; use faster method
      tramp-copy-size-limit (* 1024 1024)   ;; 1MB buffer for faster copying
      enable-remote-dir-locals t)           ;; load .dir-locals.el of remote projects

;; Use async processes to speed up TRAMP.
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

;; Show line numbers when editing SSH configs
(add-hook 'ssh-authorized-keys-mode-hook #'display-line-numbers-mode)
(add-hook 'ssh-config-mode-hook #'display-line-numbers-mode)

(provide 'nano-remote)
;;; nano-remote.el ends here
