;;; nano-layout.el --- UI layout setup

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

(require 'disp-table)

(setq default-frame-alist
      (append (list
               '(fullscreen . maximized)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe    . 1)
               '(right-fringe   . 1)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

;; on OSX, type the line below (in terminal) to get a 1 pixel border
;; defaults write com.apple.universalaccess increaseContrast -bool YES

;; To control anti-aliasing on OSX:
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 0 (none)
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 1 (light)
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 2 (medium)
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 3 (strong)

;; Fall back font for glyph missing in JetBrains Mono
(defface fallback '((t :family "Fira Code"
                       :inherit 'nano-face-faded)) "Fallback.")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                         (make-glyph-code ?↩ 'fallback))

;; (set-fontset-font t nil "Fira Code" nil 'append)

;; Fix bug on OSX in term mode & zsh (spurious % after each command)
(add-hook 'term-mode-hook
	  (lambda () (setq buffer-display-table (make-display-table))))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)
(when (fboundp 'tool-bar-mode) (tool-bar-mode nil))
(tooltip-mode 0)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode nil))
(menu-bar-mode 0)
(global-hl-line-mode 1)
(setq x-underline-at-descent-line t)

;; Vertical window divider
(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Hide org markup for README
(setq org-hide-emphasis-markers t)

;; Display line numbers in most modes
(add-hook 'conf-unix-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; Hide indicatiors in left-fringe (GUI mode)
(setq flycheck-indication-mode nil)

(provide 'nano-layout)
;;; nano-layout.el ends here
