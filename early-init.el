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
;; The very first initialization code Emacs runs. Used for lowlevel
;; optimizations and special settings.
;; ---------------------------------------------------------------------

(setq
 site-run-file nil                         ; No site-wide run-time initializations. 
 inhibit-default-init t                    ; No site-wide default library
 gc-cons-threshold most-positive-fixnum    ; Very large threshold for garbage
                                           ; collector during init
 package-enable-at-startup nil)            ; We'll use straight.el

(setq native-comp-eln-load-path
      (list (expand-file-name "eln-cache" user-emacs-directory)))

;; Reset garbage collector limit after init process has ended (8Mo)
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold (* 8 1024 1024))))
