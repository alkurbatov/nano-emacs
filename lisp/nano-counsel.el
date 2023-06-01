;; nano-councel.el --- Experimental

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
(require 'counsel)
(require 'smex)
(require 'swiper)

(setq ivy-height 20)
(setq ivy-count-format "")
(setq ivy-initial-inputs-alist: '((counsel-minor .            "^+")
                                  (counsel-package .          "^+")
                                  (counsel-org-capture .      "^")
                                  (counsel-M-x .              "^")
                                  (counsel-describe-symbol .  "^")
                                  (org-refile .               "")
                                  (org-agenda-refile .        "")
                                  (org-capture-refile .       "")
                                  (Man-completion-table .     "^")
                                  (woman .                    "^")))

(bind-keys
 ("M-x" . counsel-M-x)

 ;; Ivy-based interface to standard commands
 ("C-s" . swiper-isearch)
 ("M-x" . counsel-M-x)
 ("C-x C-f" . counsel-find-file)
 ("M-y" . counsel-yank-pop)
 ("<f1> f" . counsel-describe-function)
 ("<f1> v" . counsel-describe-variable)
 ("<f1> l" . counsel-find-library)
 ("<f2> i" . counsel-info-lookup-symbol)
 ("<f2> u" . counsel-unicode-char)
 ("<f2> j" . counsel-set-variable)
 ("C-x b" . ivy-switch-buffer)
 ("C-c v" . ivy-push-view)
 ("C-c V" . ivy-pop-view)

 ;; Ivy-based interface to shell and system tools
 ("C-c c" . counsel-compile)
 ("C-c g" . counsel-git)
 ("C-c j" . counsel-git-grep)
 ("C-c L" . counsel-git-log)
 ("C-c k" . counsel-rg)
 ("C-c m" . counsel-linux-app)
 ("C-c n" . counsel-fzf)
 ("C-x l" . counsel-locate)
 ("C-c J" . counsel-file-jump)
 ("C-S-o" . counsel-rhythmbox)
 ("C-c w" . counsel-wmctrl)

 ;; Ivy-resume and other commands
 ("C-c C-r" . ivy-resume)
 ("C-c b" . counsel-bookmark)
 ("C-c d" . counsel-descbinds)
 ("C-c g" . counsel-git)
 ("C-c o" . counsel-outline)
 ("C-c t" . counsel-load-theme)
 ("C-c F" . counsel-org-file))

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)


(provide 'nano-counsel)
;;; nano-counsel.el ends here
