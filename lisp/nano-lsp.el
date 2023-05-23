;;; -*- lexical-binding: t -*-
;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------
;; Configures programming environment for the Python language.

(use-package lsp-mode
  :defer t

  :config
  (use-package lsp-ivy
    :commands lsp-ivy-workspace-symbol)

  ;; Manually inject flycheck support as by some reason it is not enabled by default
  (require 'lsp-diagnostics)
  (lsp-diagnostics-flycheck-enable)

  ;; Refresh lenses faster
  (setq lsp-idle-delay 0.1)

  ;; Disable flymake in favor of flycheck
  (setq lsp-prefer-flymake nil)

  ;; See this guide for meaning of this section and additional features to disable:
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-headerline-breadcrumb-icons-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-headerline-breadcrumb-enable nil)

  :commands
  (lsp lsp-deferred)

  :custom
  (lsp-auto-guess-root +1))


(provide 'nano-lsp)
