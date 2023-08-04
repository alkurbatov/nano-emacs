;;; nano-mc.el --- Experimental Dired config inspiered by Midnight Commander

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2023 - N Λ N O developers

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

;; Assume that we should use another Dired buffer (opened in a window nearby)
;; as target of all operations.
(setq dired-dwim-target t)

;; Always do recursive copies
(setq dired-recursive-copies t)

;; Tweak files and folders deletion.
(setq
   delete-by-moving-to-trash t
   dired-recursive-deletes 'always)

(provide 'nano-mc)
;;; nano-mc.el ends here
