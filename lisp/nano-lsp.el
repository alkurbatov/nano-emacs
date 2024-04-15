;;; nano-lsp.el --- Language server integration via Lspce -*- lexical-binding: t; -*-

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

;; See https://github.com/zbelial/lspce

;;; Code:
(require 'lspce)

(setq lspce-send-changes-idle-time 0.2)

;; Reset list of Lspce LSP servers. It'll be filled in dedicated modules.
(setq lspce-server-programs '())

;; Uncomment to get debug log.
;; (lspce-set-log-level-trace)

;; Uncomment to enable Lspce logs.
;; (lspce-set-log-file (format "/tmp/lspce/lspce_%d.log" (emacs-pid)))

(provide 'nano-lsp)
;;; nano-lsp.el ends here
