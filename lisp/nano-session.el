;;; nano-session.el --- History and backups

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
(require 'nano-sensitive)

;; Save miscellaneous history
(setq savehist-additional-variables
      '(kill-ring
        command-history
        set-variable-value-history
        custom-variable-history
        query-replace-history
        read-expression-history
        minibuffer-history
        read-char-history
        face-name-history
        bookmark-history
        ivy-history
        counsel-M-x-history
        file-name-history
        counsel-minibuffer-history))

(setq history-length 250)
(setq kill-ring-max 50)
(put 'minibuffer-history         'history-length 50)
(put 'file-name-history          'history-length 50)
(put 'set-variable-value-history 'history-length 25)
(put 'custom-variable-history    'history-length 25)
(put 'query-replace-history      'history-length 25)
(put 'read-expression-history    'history-length 25)
(put 'read-char-history          'history-length 25)
(put 'face-name-history          'history-length 25)
(put 'bookmark-history           'history-length 25)
(put 'ivy-history                'history-length 25)
(put 'counsel-M-x-history        'history-length 25)
(put 'counsel-minibuffer-history 'history-length 25)

;; No duplicates in history
(setq history-delete-duplicates t)

(setq savehist-file "~/.nano-savehist")
(savehist-mode 1)

;; Remove text properties for kill ring entries. This saves a lot of time when loading it.
;; See https://emacs.stackexchange.com/questions/4187
(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook #'unpropertize-kill-ring)

;; Recentf files
(setq recentf-max-menu-items 25
      recentf-save-file (concat user-emacs-directory "recentf")
      recentf-exclude '(".elfeed/index$" ".git/COMMIT_EDITMSG$")) ; ignore some files in recentf
(recentf-mode 1)

;; Bookmarks
(setq bookmark-default-file "~/.nano-bookmark"
      bookmark-save-flag 1)   ; save bookmarks to disk as soon as possible (default: on exit)

;; Lock files
(setq lock-file-name-transforms `((".*" ,(concat user-emacs-directory "locks/") t)))

;; Backup
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/")))
      make-backup-files t     ; backup of a file the first time it is saved
      backup-by-copying t     ; don't clobber symlinks
      vc-make-backup-files t  ; backup version controlled files too as we don't commit on every save
      version-control t       ; version numbers for backup files
      delete-old-versions t   ; delete excess backup files silently
      kept-old-versions 6     ; oldest versions to keep when a new numbered
                              ;  backup is made (default: 2)
      kept-new-versions 9     ; newest versions to keep when a new numbered
                              ;  backup is made (default: 2)
      auto-save-default t     ; auto-save every buffer that visits a file
      auto-save-timeout 20    ; number of seconds idle time before auto-save
                              ;  (default: 30)
      auto-save-interval 200)  ; number of keystrokes between auto-saves
                              ;  (default: 300)

;; Exclude sensitive data from backups
(add-to-list 'auto-mode-alist '("\\.gpg$" . sensitive-mode))
(add-to-list 'auto-mode-alist '("\\.netrc$" . sensitive-mode))

;; Record cursor position from one session ot the other
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory)
      save-place-forget-unreadable-files t)
(save-place-mode 1)


(provide 'nano-session)
;;; nano-session.el ends here
