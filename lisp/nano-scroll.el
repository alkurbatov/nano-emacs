;;; nano-scroll.el --- Setup smooth scrolling -*- lexical-binding: t; -*-

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
;;; The idea and the code of nano-pixel-scroll-precision are taken from
;;; https://maximzuriel.nl/physics-and-code/emacs-mac-smooth-scroll/article

;;; Code:
(require 'bind-key)

(defun nano-pixel-scroll-precision (event)
  "Scroll the display vertically by pixels according to EVENT.
ve the display up or down by the pixel deltas in EVENT to
roll the display according to the user's turning the mouse
eel."
  (interactive "e")
  (let ((window (mwheel-event-window event))
        (current-window (selected-window)))
    (when (framep window)
      (setq window (frame-selected-window window)))
    (if (and (nth 3 event))
        (let ((delta
               (* -1
                  (let ((dy (plist-get (nth 3 event) :scrolling-delta-y))
                        pending-events)
                    (if pending-events
                        (setq unread-command-events (nconc (nreverse pending-events)
                                                           unread-command-events)))
                    (round (- dy))))))
          (unless (zerop delta)
            (if (> (abs delta) (window-text-height window t))
                (mwheel-scroll event nil)
              (with-selected-window window
                (if (or (and pixel-scroll-precision-interpolate-mice
                             (eq (device-class last-event-frame
                                               last-event-device)
                                 'mouse))
                        (and pixel-scroll-precision-large-scroll-height
                             (> (abs delta)
                                pixel-scroll-precision-large-scroll-height)
                             (let* ((kin-state (pixel-scroll-kinetic-state))
                                    (ring (aref kin-state 0))
                                    (time (aref kin-state 1)))
                               (or (null time)
                                   (> (- (float-time) time) 1.0)
                                   (and (consp ring)
                                        (ring-empty-p ring))))))
                    (progn
                      (let ((kin-state (pixel-scroll-kinetic-state)))
                        (aset kin-state 0 (make-ring 30))
                        (aset kin-state 1 nil))
                      (pixel-scroll-precision-interpolate delta current-window))
                  (condition-case nil
                      (progn
                        (if (< delta 0)
                        (pixel-scroll-precision-scroll-down (- delta))
                          (pixel-scroll-precision-scroll-up delta))
                        (pixel-scroll-accumulate-velocity delta))
                    ;; Do not ding at buffer limits.  Show a message instead.
                    (beginning-of-buffer
                     (message (error-message-string '(beginning-of-buffer))))
                    (end-of-buffer
                     (message (error-message-string '(end-of-buffer))))))))))
      (mwheel-scroll event nil))))

;; Enable pixel scroll mode
(setq scroll-conservatively 101)
(pixel-scroll-precision-mode)

(with-eval-after-load 'pixel-scroll
  ;; Use smooth scrolling
  (when os-macos
    (bind-key "<wheel-down>" #'nano-pixel-scroll-precision)
    (bind-key "<wheel-up>" #'nano-pixel-scroll-precision)))

(provide 'nano-scroll)
;;; nano-scroll.el ends here
