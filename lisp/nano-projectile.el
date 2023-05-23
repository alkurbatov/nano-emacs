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
;; Project interaction library for Emacs.
;; https://docs.projectile.mx/projectile/index.html
;; ---------------------------------------------------------------------

(require 'projectile)

;; Enable projectile mode globally
(projectile-mode +1)

;; Disable autodiscover for speedup, all projects will be added manually
(setq projectile-auto-discover nil)

;; No need in caching
(setq projectile-enable-caching nil)

;; Use indexing method working on all platforms
(setq projectile-indexing-method 'alien)

;; Key bindings
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

(provide 'nano-projectile)
