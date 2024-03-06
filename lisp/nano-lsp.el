;;; nano-lsp.el --- Language server integration via Eglot

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

;; Disable debug log to speed up things a bit
(fset #'jsonrpc--log-event #'ignore) ; Remove laggy typing it probably reduces chatty json from lsp to eglot
(setq eglot-events-buffer-config '(:size 0 :format full))

(setq-default eglot-autoshutdown t       ; Automatically shutdown backend if last buffer was killed
              eglot-sync-connect nil     ; Otherwise, Elgot freezes the UI for ~3s when large file is opened
              eglot-connect-timeout nil) ; Never time out Eglot connection to make things faster

(setq eglot-ignored-server-capabilities '(:inlayHintProvider)) ; Disable annoying inlay hints

;; Specify explicitly to use Orderless for Eglot
(setq completion-category-overrides '((eglot (styles orderless))
                                      (eglot-capf (styles orderless))))

(with-eval-after-load "eglot"
  (bind-key "M-g s"   #'consult-eglot-symbols))

(provide 'nano-lsp)
;;; nano-lsp.el ends here
