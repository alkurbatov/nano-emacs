;;; nano-theme-support.el --- N Λ N O theme -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-theme

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; N Λ N O theme is a consistent dark theme that is based
;; on Nord (https://www.nordtheme.com/).
;;
;; A theme is fully defined by a set of (1+6) faces as
;; explained in this paper https://arxiv.org/abs/2008.06030:
;;
;; - Default face is the face for regular information.
;;
;; - Critical face is for information that requires immediate action.
;;
;;     It should be of high constrast when compared to other
;;     faces. This can be realized (for example) by setting an intense
;;     background color, typically a shade of red. It must be used
;;     scarcely.
;;
;; - Popout face is used for information that needs attention.
;;
;;     To achieve such effect, the hue of the face has to be
;;     sufficiently different from other faces such that it attracts
;;     attention through the popout effect.
;;
;; - Strong face is used for information of a structural nature.
;;
;;     It has to be the same color as the default color and only the
;;     weight differs by one level (e.g., light/regular or
;;     regular/bold). IT is generally used for titles, keywords,
;;     directory, etc.
;;
;; - Salient face is used for information that are important.
;;
;;     To suggest the information is of the same nature but important,
;;     the face uses a different hue with approximately the same
;;     intensity as the default face. This is typically used for
;;     links.

;; - Faded face is for information that are less important.
;;
;;     It is made by using the same hue as the default but with a
;;     lesser intensity than the default. It can be used for comments,
;;     secondary information and also replace italic (which is
;;     generally abused anyway
;;
;; - Subtle face is used to suggest a physical area on the screen.
;;
;;     It is important to not disturb too strongly the reading of
;;     information and this can be made by setting a very light
;;     background color that is barely perceptible.
;;

;; Usage example:
;;
;; You can use the theme as a regular theme.
;;
;; Optionally, you can use (nano-mode) to setup recommended settings for
;; the theme. Be careful since it will modify your configuration and
;; requires a set of specific fonts. This needs to be called before
;; setting the theme
;;
;; Recommended font is "JetBrains Mono" and "JetBrainsMono Nerd" if you want
;; to benefit from all the fancy glyphs. See https://www.nerdfonts.com.

;;; Code:
(require 'disp-table)
(require 'cl-macs)

(defgroup nano nil
  "N Λ N O"
  :group 'convenience)

(defgroup nano-theme nil
  "N Λ N O Theme."
  :group 'nano)

(defgroup nano-theme-dark nil
  "Dark color palette."
  :group 'nano-theme)

(defgroup nano-theme-fonts nil
  "Font stack."
  :group 'nano-theme)

(defcustom nano-fonts-use nil
  "Whether to use font stack."
  :type 'boolean :group 'nano-theme-fonts)

(defcustom nano-window-divider-show nil
  "Whether to show the vertical window-divider."
  :type 'boolean :group 'nano-theme)

(defface nano-mono
  '((t (:family "JetBrains Mono"
        :height 150
        :weight light)))
  "Default monospaced font (JetBrains Mono Light, 15pt)."
  :group 'nano-theme-fonts)

(defface nano-sans
  '((t (:family "Roboto"
        :height 150
        :weight light)))
  "Default proportional sans font (Roboto Light, 15pt)."
  :group 'nano-theme-fonts)

(defface nano-serif
  '((t (:family "Roboto Slab"
        :height 150
        :weight light)))
  "Default proportional serif font (Roboto Slab Light, 15pt)."
  :group 'nano-theme-fonts)

(defface nano-italic
  '((t (:family "JetBrains Mono"
        :slant italic
        :height 150
        :weight regular)))
  "Default italic font (JetBrains Mono Italic Light, 15pt)."
  :group 'nano-theme-fonts)

(defvar polar-night-0 "#2E3440")
(defvar polar-night-1 "#3B4252")
(defvar polar-night-2 "#434C5E")
(defvar polar-night-3 "#4C566A")
(defvar polar-night-4 "#677691"
  "Even lighter polar night to use as comments color, not part of the Nord palette.")
(defvar snow-storm-0  "#D8DEE9")
(defvar snow-storm-1  "#E5E9F0")
(defvar snow-storm-2  "#ECEFF4")
(defvar frost-0       "#8FBCBB")
(defvar frost-1       "#88C0D0")
(defvar frost-2       "#81A1C1")
(defvar frost-3       "#5E81AC")
(defvar aurora-0      "#BF616A")
(defvar aurora-1      "#D08770")
(defvar aurora-2      "#EBCB8B")
(defvar aurora-3      "#A3BE8C")
(defvar aurora-4      "#B48EAD")

(defface nano-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group nil)

(defface nano-critical-i nil
  "Critical face inversed."
  :group nil)

(defface nano-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group nil)

(defface nano-popout-i nil
  "Popout face inversed."
  :group nil)

(defface nano-strong nil
  "Strong face is used for information of a structural nature.
It is generally used for titles, keywords, directory, etc."
  :group nil)

(defface nano-strong-i nil
  "Strong face inversed."
  :group nil)

(defface nano-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group nil)

(defface nano-salient-i nil
  "Strong face inversed."
  :group nil)

(defface nano-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group nil)

(defface nano-faded-i nil
  "Faded face inversed."
  :group nil)

(defface nano-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group nil)

(defface nano-subtle-i nil
  "Subtle face inversed."
  :group nil)

(defface nano-default nil
  "Default face."
  :group nil)

(defface nano-default-i nil
  "Default face inversed."
  :group nil)

(defface nano-face-org-green-project
  `((t (:foreground ,aurora-3)))
  "Nano face for personal projects."
  :group 'nano-theme-fonts)

(defface nano-face-org-red-project
  `((t (:foreground ,aurora-0)))
  "Nano face for must-do-or-be-fired projects."
  :group 'nano-theme-fonts)

(defun nano-mode ()
  "Defaults settings for nano."
  (interactive)

  ;; Use nano fonts
  (setq nano-fonts-use t)

  ;; No startup  screen
  (setq inhibit-startup-screen t)

  ;; No startup message
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message t)

  ;; No message in scratch buffer
  (setq initial-scratch-message nil)

  ;; Initial buffer
  (setq initial-buffer-choice nil)

  ;; No frame title
  (setq frame-title-format nil)

  ;; No file dialog
  (setq use-file-dialog nil)

  ;; No dialog box
  (setq use-dialog-box nil)

  ;; No popup windows
  (setq pop-up-windows nil)

  ;; No empty line indicators
  (setq indicate-empty-lines nil)

  ;; No cursor in inactive windows
  (setq cursor-in-non-selected-windows nil)

  ;; Text mode is initial mode
  (setq initial-major-mode 'text-mode)

  ;; Text mode is default major mode
  (setq default-major-mode 'text-mode)

  ;; Moderate font lock
  (setq font-lock-maximum-decoration t)

  ;; No line break space points
  (setq auto-fill-mode nil)

  ;; Fill column at 80
  (setq fill-column 80)

  ;; Bar cursor
  (setq-default cursor-type '(hbar .  2))
  (setq-default cursor-in-non-selected-windows nil)
  (setq blink-cursor-mode nil)

  ;; No tooltips
  (tooltip-mode -1)

  ;; No scroll bars
  (scroll-bar-mode -1)

  ;; No toolbar
  (tool-bar-mode -1)

  ;; Default frame settings
  (setq default-frame-alist
        (append (list
                 '(min-height . 1)  '(height . 45)
                 '(min-width  . 1)  '(width  . 81)
                 '(vertical-scroll-bars . nil)
                 '(internal-border-width . 24)
                 '(left-fringe . 4)
                 '(right-fringe . 0)
                 '(undecorated-round . t) ;; emacs-plu@29 only
                 '(tool-bar-lines . 0)
                 '(menu-bar-lines . 0))))

  ;; Line spacing (in pixels)
  ;; (setq line-spacing 0)

  ;; Vertical window divider
  (setq window-divider-default-right-width 24)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1)

  ;; Nicer glyphs for continuation and wrap
  (set-display-table-slot standard-display-table
                          'truncation (make-glyph-code ?… 'nano-faded))
  (set-display-table-slot standard-display-table
                          'wrap (make-glyph-code ?- 'nano-faded))

  ;; Nerd font for glyph icons
  (let ((jetbrains-nerd (font-spec :name "JetBrainsMono Nerd Font Mono")))
    (if (find-font jetbrains-nerd)
        (set-fontset-font t '(#xe000 . #xffdd) jetbrains-nerd)
      (message "JetBrainsMono Nerd font has not been found on your system")))

  ;; Customize ethan-wspace mode faces
  (with-eval-after-load 'ethan-wspace
    (setq ethan-wspace-face-customized t))

  ;; Customize highlight-indent-guides mode
  (with-eval-after-load 'highlight-indent-guides
    (setq highlight-indent-guides-auto-enabled nil)

    (setq highlight-indent-guides-method 'character
          highlight-indent-guides-character ?\|
          highlight-indent-guides-responsive 'top))

  ;; Customize hl-todo colors
  (with-eval-after-load 'hl-todo
    (setq hl-todo-highlight-punctuation ":"
          hl-todo-keyword-faces
          `(("TODO"       . nano-popout)
            ("XXX"        . nano-popout)
            ("FIXME"      . nano-critical)
            ("NB"         . nano-salient)
            ("NOTE"       . nano-salient))))

)


;; (defun inherit (face &optional inherit)
;;   "Extract face properties as a property list"

;;   (let ((tags (list :family :foundry :width :height :weight :slant :underline
;;                     :overline :strike-through :box :inverse-video :foreground
;;                     :background :stipple :extend :inherit))
;;  (properties))
;;     (if inherit
;;  `(:inherit ,face)
;;       (progn
;;  (dolist (tag tags)
;;    (let ((attribute (face-attribute face tag)))
;;     (when (not (eq attribute 'unspecified))
;;       (push attribute properties)
;;       (push tag properties)))))
;;       properties)))


(defun nano-frame-list-advice-selected (_frames)
  (list (selected-frame)))

(defun nano-frame-list-advice-normal (frames)
  (seq-filter (lambda (f) (not (frame-parameter f 'nano-theme-standalone))) frames))

(defun nano-theme ()
  "Apply the nano dark theme."

  (advice-add 'frame-list :filter-return #'nano-frame-list-advice-normal)

  (let ((dark '((background dark))))

    (setq default-frame-alist
          (assq-delete-all 'foreground-color
                           (assq-delete-all 'background-color
                                            (assq-delete-all 'background-mode default-frame-alist))))
    (add-to-list 'default-frame-alist `(background-mode . ,'dark))
    (add-to-list 'default-frame-alist `(background-color . ,polar-night-0))
    (add-to-list 'default-frame-alist `(foreground-color . ,snow-storm-2))
    (custom-theme-set-variables 'nano '(widget-image-enable nil)
                                '(x-underline-at-descent-line t))
    (setq frame-background-mode 'dark)
    (mapc #'frame-set-background-mode (frame-list))

    (when nano-fonts-use
        (custom-theme-set-faces 'nano
         `(default ((t (:foreground ,snow-storm-2
                        :weight     ,(face-attribute 'nano-mono :weight)
                        :height     ,(face-attribute 'nano-mono :height)
                        :family     ,(face-attribute 'nano-mono :family)))))
         `(italic ((t (:foreground ,snow-storm-2
                       :weight     ,(face-attribute 'nano-italic :weight)
                       :height     ,(face-attribute 'nano-italic :height)
                       :slant      ,(face-attribute 'nano-italic :slant)
                       :family     ,(face-attribute 'nano-italic :family)))))
         `(nano-strong ((t (:foreground ,snow-storm-0 :weight normal))))
         `(variable-pitch ((t (:weight ,(face-attribute 'nano-sans :weight)
                               :height ,(face-attribute 'nano-sans :height)
                               :family ,(face-attribute 'nano-sans :family)))))))

    (unless nano-fonts-use
        (custom-theme-set-faces 'nano
         `(default ((t (:foreground ,snow-storm-2))))
         `(nano-strong ((t (:weight bold :foreground ,snow-storm-0))))))

    ;; --- Window divider ----------------------------------------------
    (if nano-window-divider-show
        (custom-theme-set-faces 'nano
         `(window-divider ((t (:foreground ,snow-storm-2))))
         `(vertical-border ((t (:foreground ,snow-storm-2)))))
      (custom-theme-set-faces 'nano
       `(window-divider ((t (:foreground ,polar-night-0))))
       `(vertical-border ((t (:foreground ,polar-night-0))))))
    (custom-theme-set-faces 'nano
     '(window-divider-first-pixel ((t (:inherit window-divider))))
     '(window-divider-last-pixel ((t (:inherit window-divider)))))

    (custom-theme-set-faces 'nano

   ;; --- Base ---------------------------------------------------------

   `(default ((t (:background ,polar-night-0
                  :foreground ,snow-storm-2))))

   `(cursor ((t (:foreground ,polar-night-0
                 :background ,snow-storm-2))))

   `(mouse ((t (:foreground ,snow-storm-2
                :background ,polar-night-0))))

   `(highlight ((t (:background ,polar-night-1))))

   `(nano-subtle ((t (:background ,polar-night-2))))

   `(nano-subtle-i ((t (:foreground ,polar-night-2))))

   `(nano-faded ((t (:foreground ,polar-night-4))))

   `(nano-faded-i ((t (:foreground ,polar-night-0
                       :background ,polar-night-4))))

   `(nano-default ((t (:foreground ,snow-storm-2))))

   `(nano-default-i ((t (:foreground ,polar-night-0
                         :background ,snow-storm-2))))


   `(nano-salient ((t (:foreground ,frost-2))))

   `(nano-salient-i ((t (:foreground ,polar-night-0
                         :background ,frost-2))))


   `(nano-strong-i ((t (:foreground ,polar-night-0
                        :background ,snow-storm-0
                        :weight normal))))

   `(nano-popout ((t (:foreground ,aurora-1))))

   `(nano-popout-i ((t (:foreground ,polar-night-0
                        :background ,aurora-1))))

   `(nano-critical ((t (:foreground ,aurora-2
                        :weight normal))))

   `(nano-critical-i ((t (:foreground ,polar-night-0
                          :background ,aurora-2
                          :weight normal))))

   ;; --- Header & mode line -------------------------------------------

   `(mode-line ((t (:foreground ,snow-storm-2
                    :background ,polar-night-4
                    :box (:line-width 3
                          :color ,polar-night-4
                          :style nil)))))
   `(mode-line-highlight ((t (:inherit nano-popout))))
   `(mode-line-buffer-id ((t (:weight regular))))
   `(mode-line-emphasis  ((t (:weight regular))))

   `(mode-line-inactive ((t (:foreground ,polar-night-4
                             :background ,polar-night-2
                             :box (:line-width 3
                                  :color ,polar-night-2
                                  :style nil)))))

   `(header-line ((t (:foreground ,snow-storm-2
                      :background ,polar-night-2
                      :inherit nil
                      :box nil))))

   ;; --- Structural ---------------------------------------------------
   '(bold                        ((t (:inherit nano-strong))))
   '(italic                      ((t (:inherit nano-faded))))
   '(bold-italic                 ((t (:inherit nano-strong))))
   '(region                      ((t (:inherit nano-subtle :distant-foreground unspecified))))
   '(fringe                      ((t (:inherit (nano-faded)))))
   '(hl-line                     ((t (:inherit highlight))))
   '(link                        ((t (:inherit nano-salient))))
   '(fixed-pitch                 ((t (:inherit default))))
   '(fixed-pitch-serif           ((t (:inherit default))))

   ;; --- Semantic -----------------------------------------------------
   '(shadow                        ((t (:inherit nano-faded))))
   '(success                       ((t (:inherit nano-salient))))
   '(warning                       ((t (:inherit nano-popout))))
   '(error                         ((t (:inherit nano-critical))))
   '(match                         ((t (:inherit nano-popout))))

   ;; --- General ------------------------------------------------------
   '(buffer-menu-buffer            ((t (:inherit nano-strong))))
   '(minibuffer-prompt             ((t (:inherit nano-strong))))
   '(isearch                       ((t (:inherit nano-strong))))
   '(isearch-fail                  ((t (:inherit nano-faded))))
   '(show-paren-match              ((t (:inherit nano-popout))))
   '(show-paren-mismatch           ((t (:inherit nano-critical))))
   '(lazy-highlight                ((t (:inherit nano-subtle))))
   '(trailing-whitespace           ((t (:inherit nano-subtle))))
   '(secondary-selection           ((t (:inherit nano-subtle))))
   '(completions-annotations       ((t (:inherit nano-faded))))
   '(completions-common-part       ((t (:inherit nano-strong))))
   '(completions-first-difference  ((t (:inherit nano-default))))
   '(tooltip                       ((t (:inherit nano-subtle))))
   '(read-multiple-choice-face     ((t (:inherit nano-strong))))
   '(nobreak-hyphen                ((t (:inherit nano-popout))))
   '(nobreak-space                 ((t (:inherit nano-popout))))
   '(help-argument-name            ((t (:inherit nano-faded))))
   '(tabulated-list-fake-header    ((t (:inherit nano-strong))))
   '(tool-bar                      ((t (:inherit nano-faded-i))))

   ;; --- TTY faces ----------------------------------------------------
   '(tty-menu-disabled-face        ((t (:inherit nano-faded-i))))
   '(tty-menu-enabled-face         ((t (:inherit nano-default-i))))
   '(tty-menu-selected-face        ((t (:inherit nano-salient-i))))

   ;; --- Tab bar ------------------------------------------------------
   '(tab-bar                       ((t (:inherit default))))
   '(tab-bar-tab                   ((t (:inherit default))))
   '(tab-bar-tab-inactive          ((t (:inherit nano-faded))))
   '(tab-line                      ((t (:inherit default))))

   ;; --- Line numbers -------------------------------------------------
   '(line-number                  ((t (:inherit nano-faded))))
   '(line-number-current-line     ((t (:inherit nil))))
   `(line-number-major-tick       ((t (:inherit nano-faded))))
   '(line-number-minor-tick       ((t (:inherit nano-faded))))

   ;; --- Font lock ----------------------------------------------------
   `(font-lock-bracket-face          ((t (:foreground ,frost-0))))
   `(font-lock-builtin-face          ((t (:foreground ,frost-0))))
   '(font-lock-comment-face          ((t (:inherit nano-faded))))
   '(font-lock-constant-face         ((t (:inherit nano-salient))))
   '(font-lock-doc-face              ((t (:inherit nano-faded))))
   `(font-lock-escape-face           ((t (:foreground ,aurora-2
                                          :weight regular))))
   `(font-lock-function-name-face    ((t (:foreground ,frost-1
                                          :weight regular))))
   `(font-lock-keyword-face          ((t (:foreground ,frost-2))))
   `(font-lock-misc-punctuation-face ((t (:foreground ,frost-1))))
   `(font-lock-number-face           ((t (:foreground ,aurora-4))))
   `(font-lock-operator-face         ((t (:foreground ,frost-2))))
   `(font-lock-property-use-face     ((t (:foreground ,frost-1))))
   `(font-lock-string-face           ((t (:foreground ,aurora-3))))
   `(font-lock-type-face             ((t (:foreground ,frost-0))))
   `(font-lock-variable-name-face    ((t (:foreground ,snow-storm-0
                                          :weight regular))))
   '(font-lock-warning-face          ((t (:inherit nano-popout))))

   ;; --- Custom edit --------------------------------------------------
   '(widget-field                  ((t (:inherit nano-subtle))))
   '(widget-button                 ((t (:inherit nano-strong))))
   '(widget-single-line-field      ((t (:inherit nano-subtle))))
   '(custom-group-subtitle         ((t (:inherit nano-strong))))
   '(custom-group-tag              ((t (:inherit nano-strong))))
   '(custom-group-tag-1            ((t (:inherit nano-strong))))
   '(custom-comment                ((t (:inherit nano-faded))))
   '(custom-comment-tag            ((t (:inherit nano-faded))))
   '(custom-changed                ((t (:inherit nano-salient))))
   '(custom-modified               ((t (:inherit nano-salient))))
   '(custom-face-tag               ((t (:inherit nano-strong))))
   '(custom-variable-tag           ((t (:inherit nano-strong))))
   '(custom-invalid                ((t (:inherit nano-popout))))
   '(custom-visibility             ((t (:inherit nano-salient))))
   '(custom-state                  ((t (:inherit nano-salient))))
   '(custom-link                   ((t (:inherit nano-salient))))
   '(custom-variable-obsolete      ((t (:inherit nano-faded))))

   ;; --- Company tooltip ----------------------------------------------
   '(company-tooltip                      ((t (:inherit nano-subtle))))
   '(company-tooltip-mouse                ((t (:inherit nano-faded-i))))
   '(company-tooltip-selection            ((t (:inherit nano-salient-i))))

   '(company-scrollbar-fg                 ((t (:inherit nano-default-i))))
   '(company-scrollbar-bg                 ((t (:inherit nano-faded-i))))

   '(company-tooltip-scrollbar-thumb      ((t (:inherit nano-default-i))))
   '(company-tooltip-scrollbar-track      ((t (:inherit nano-faded-i))))

   '(company-tooltip-common               ((t (:inherit nano-strong))))
   '(company-tooltip-common-selection     ((t (:inherit nano-salient-i
                                                :weight normal))))
   '(company-tooltip-annotation           ((t (:inherit nano-default))))
   '(company-tooltip-annotation-selection ((t (:inherit nano-subtle))))

   ;; --- Compilation --------------------------------------------------
   '(compilation-error ((t (:inherit nano-critical))))
   '(compilation-info ((t (:inherit nano-default))))
   '(compilation-warning ((t (:inherit nano-popout))))
   '(compilation-line-number ((t (:inherit nano-default))))
   '(compilation-column-number ((t (:inherit nano-default))))
   '(compilation-mode-line-run ((t (:inherit nano-default-i))))
   '(compilation-mode-line-exit ((t (:inherit nano-default-i))))
   '(compilation-mode-line-fail ((t (:inherit nano-critical))))

   ;; --- Buttons ------------------------------------------------------
   `(custom-button
     ((t (:foreground ,polar-night-4
          :background ,polar-night-1
          :box nil))))

   `(custom-button-mouse
     ((t (:foreground ,snow-storm-2
          :background ,polar-night-2
          :box nil))))

   `(custom-button-pressed
     ((t (:foreground ,polar-night-0
          :background ,snow-storm-2
          :box nil))))

   ;; --- Packages -----------------------------------------------------
   '(package-description            ((t (:inherit nano-default))))
   '(package-help-section-name      ((t (:inherit nano-default))))
   '(package-name                   ((t (:inherit nano-salient))))
   '(package-status-avail-obso      ((t (:inherit nano-faded))))
   '(package-status-available       ((t (:inherit nano-default))))
   '(package-status-built-in        ((t (:inherit nano-salient))))
   '(package-status-dependency      ((t (:inherit nano-salient))))
   '(package-status-disabled        ((t (:inherit nano-faded))))
   '(package-status-external        ((t (:inherit nano-default))))
   '(package-status-held            ((t (:inherit nano-default))))
   '(package-status-incompat        ((t (:inherit nano-faded))))
   '(package-status-installed       ((t (:inherit nano-salient))))
   '(package-status-new             ((t (:inherit nano-default))))
   '(package-status-unsigned        ((t (:inherit nano-default))))

   ;; --- Info ---------------------------------------------------------
   '(info-node                      ((t (:inherit nano-strong))))
   '(info-menu-header               ((t (:inherit nano-strong))))
   '(info-header-node               ((t (:inherit nano-default))))
   '(info-index-match               ((t (:inherit nano-salient))))
   '(Info-quoted                    ((t (:inherit nano-faded))))
   '(info-title-1                   ((t (:inherit nano-strong))))
   '(info-title-2                   ((t (:inherit nano-strong))))
   '(info-title-3                   ((t (:inherit nano-strong))))
   '(info-title-4                   ((t (:inherit nano-strong))))

   ;; --- Helpful ------------------------------------------------------
   '(helpful-heading                ((t (:inherit nano-strong))))

   ;; --- Nano modeline ------------------------------------------------
   '(nano-modeline-active               ((t (:inherit nano-subtle))))
   '(nano-modeline-active-name          ((t (:inherit (nano-strong nano-modeline-active)))))
   '(nano-modeline-active-primary       ((t (:inherit (nano-default nano-modeline-active)))))
   '(nano-modeline-active-secondary     ((t (:inherit (nano-faded nano-modeline-active)))))
   '(nano-modeline-active-status-RO     ((t (:inherit (nano-subtle nano-strong)))))
   '(nano-modeline-active-status-RW     ((t (:inherit (nano-faded-i nano-strong)))))
   '(nano-modeline-active-status-**     ((t (:inherit (nano-popout-i nano-strong)))))

   '(nano-modeline-inactive             ((t (:inherit nano-subtle))))
   '(nano-modeline-inactive-name        ((t (:inherit (nano-faded nano-modeline-inactive)))))
   '(nano-modeline-inactive-primary     ((t (:inherit (nano-faded nano-modeline-inactive)))))
   '(nano-modeline-inactive-secondary   ((t (:inherit (nano-faded nano-modeline-inactive)))))
   '(nano-modeline-inactive-status-RO   ((t (:inherit (nano-faded
                                                       nano-strong nano-modeline-inactive)))))
   '(nano-modeline-inactive-status-RW   ((t (:inherit (nano-faded
                                                       nano-strong nano-modeline-inactive)))))
   '(nano-modeline-inactive-status-**   ((t (:inherit (nano-popout
                                                       nano-strong nano-modeline-inactive)))))

   ;; --- nano agenda ---------------------------------------------------------
   '(nano-agenda-button               ((t (:inherit (nano-faded)))))
   '(nano-agenda-day-name             ((t (:inherit (nano-faded)))))
   '(nano-agenda-default              ((t (:inherit (nano-default)))))
   '(nano-agenda-holidays             ((t (:inherit (nano-faded)))))
   '(nano-agenda-month-name           ((t (:inherit (nano-strong)))))
   '(nano-agenda-mouse                ((t (:inherit (nano-highlight)))))
   '(nano-agenda-outday               ((t (:inherit (nano-subtle-i)))))
   '(nano-agenda-selected             ((t (:inherit (nano-default-i)))))
   '(nano-agenda-selected-today       ((t (:inherit (nano-popout-i nano-strong)))))
   '(nano-agenda-today                ((t (:inherit (nano-popout nano-strong)))))
   '(nano-agenda-weekend              ((t (:inherit (nano-faded)))))

   ;; --- EPA ----------------------------------------------------------
   '(epa-field-body                 ((t (:inherit nano-default))))
   '(epa-field-name                 ((t (:inherit nano-strong))))
   '(epa-mark                       ((t (:inherit nano-salient))))
   '(epa-string                     ((t (:inherit nano-popout))))
   '(epa-validity-disabled          ((t (:inherit nano-faded))))
   '(epa-validity-high              ((t (:inherit nano-strong))))
   '(epa-validity-medium            ((t (:inherit nano-default))))
   '(epa-validity-low               ((t (:inherit nano-faded))))

   ;; --- Popup --------------------------------------------------------
   '(popup-face                       ((t (:inherit highlight))))
   '(popup-isearch-match              ((t (:inherit nano-popout))))
   '(popup-menu-face                  ((t (:inherit nano-subtle))))
   '(popup-menu-mouse-face            ((t (:inherit nano-faded-i))))
   '(popup-menu-selection-face        ((t (:inherit nano-salient-i))))
   '(popup-menu-summary-face          ((t (:inherit nano-faded))))
   '(popup-scroll-bar-background-face ((t (:inherit nano-subtle))))
   '(popup-scroll-bar-foreground-face ((t (:inherit nano-subtle))))
   '(popup-summary-face               ((t (:inherit nano-faded))))
   '(popup-tip-face                   ((t (:inherit nano-popout-i))))

   ;; --- Diff ---------------------------------------------------------
   '(diff-header                    ((t (:inherit nano-faded))))
   '(diff-file-header               ((t (:inherit nano-strong))))
   '(diff-context                   ((t (:inherit nano-default))))
   '(diff-removed                   ((t (:inherit nano-faded))))
   '(diff-changed                   ((t (:inherit nano-popout))))
   '(diff-added                     ((t (:inherit nano-salient))))
   '(diff-refine-added              ((t (:inherit (nano-salient
                                                   nano-strong)))))
   '(diff-refine-changed            ((t (:inherit nano-popout))))
   '(diff-refine-removed            ((t (:inherit nano-faded
                                         :strike-through t))))
   ;; --- Vertico --------------------------------------------------------
   '(vertico-current                       ((t (:inherit (nano-strong
                                                          nano-subtle)))))
   '(vertico-group-separator               ((t (:inherit nano-faded))))
   '(vertico-group-title                   ((t (:inherit nano-faded))))
   '(vertico-multiline                     ((t (:inherit nano-faded))))

   ;; --- Citar --------------------------------------------------------
   '(citar                          ((t (:inherit nano-faded))))
   '(citar-highlight                ((t (:inherit nano-default))))

   ;; --- Corfu --------------------------------------------------------
   '(corfu-annotations              ((t (:inherit nano-faded))))
   '(corfu-bar                      ((t (:inherit nano-default-i))))
   `(corfu-border                   ((t (:foreground ,polar-night-0
                                         :background ,polar-night-3))))
   `(corfu-current                  ((t (:background ,polar-night-3))))
   `(corfu-default                  ((t (:background ,polar-night-1))))
   '(corfu-deprecated               ((t (:inherit nano-faded))))
   '(corfu-echo                     ((t (:inherit nano-faded))))

   ;; --- Orderless ----------------------------------------------------
   '(orderless-match-face-0         ((t (:inherit (nano-salient
                                                   nano-strong)))))
   '(orderless-match-face-1         ((t (:inherit (nano-strong)))))
   '(orderless-match-face-2         ((t (:inherit (nano-strong)))))
   '(orderless-match-face-3         ((t (:inherit (nano-strong)))))

   ;; --- Message ------------------------------------------------------
   '(message-cited-text-1           ((t (:inherit nano-faded))))
   '(message-cited-text-2           ((t (:inherit nano-faded))))
   '(message-cited-text-3           ((t (:inherit nano-faded))))
   '(message-cited-text-4           ((t (:inherit nano-faded))))
   '(message-cited-text             ((t (:inherit nano-faded))))
   '(message-header-cc              ((t (:inherit nano-default))))
   '(message-header-name            ((t (:inherit nano-strong :weight bold))))
   '(message-header-newsgroups      ((t (:inherit nano-default))))
   '(message-header-other           ((t (:inherit nano-default))))
   '(message-header-subject         ((t (:inherit nano-salient))))
   '(message-header-to              ((t (:inherit nano-salient))))
   '(message-header-xheader         ((t (:inherit nano-default))))
   '(message-mml                    ((t (:inherit nano-popout))))
   '(message-separator              ((t (:inherit nano-faded))))

   ;; --- Outline ------------------------------------------------------
   '(outline-1                      ((t (:inherit nano-strong))))
   '(outline-2                      ((t (:inherit nano-strong))))
   '(outline-3                      ((t (:inherit nano-strong))))
   '(outline-4                      ((t (:inherit nano-strong))))
   '(outline-5                      ((t (:inherit nano-strong))))
   '(outline-6                      ((t (:inherit nano-strong))))
   '(outline-7                      ((t (:inherit nano-strong))))
   '(outline-8                      ((t (:inherit nano-strong))))

   ;; --- Flyspell ----------------------------------------------------
   `(flyspell-duplicate             ((t (:background ,frost-2
                                         :foreground ,polar-night-0
                                         :underline unspecified))))
   `(flyspell-incorrect             ((t (:background ,frost-2
                                         :foreground ,polar-night-0
                                         :underline unspecified))))

   ;; --- Org agenda ---------------------------------------------------
   '(org-agenda-calendar-event      ((t (:inherit nano-default))))
   '(org-agenda-calendar-sexp       ((t (:inherit nano-salient))))
   '(org-agenda-clocking            ((t (:inherit nano-faded))))
   '(org-agenda-column-dateline     ((t (:inherit nano-faded))))
   '(org-agenda-current-time        ((t (:inherit (nano-strong
                                                   nano-salient)))))
   '(org-agenda-date                ((t (:inherit nano-strong))))
   '(org-agenda-date-today          ((t (:inherit (nano-salient
                                                   nano-strong)))))
   '(org-agenda-date-weekend        ((t (:inherit nano-faded))))
   '(org-agenda-diary               ((t (:inherit nano-faded))))
   '(org-agenda-dimmed-todo-face    ((t (:inherit nano-faded))))
   '(org-agenda-done                ((t (:inherit nano-faded))))
   '(org-agenda-filter-category     ((t (:inherit nano-faded))))
   '(org-agenda-filter-effort       ((t (:inherit nano-faded))))
   '(org-agenda-filter-regexp       ((t (:inherit nano-faded))))
   '(org-agenda-filter-tags         ((t (:inherit nano-faded))))
   '(org-agenda-property-face       ((t (:inherit nano-faded))))
   '(org-agenda-restriction-lock    ((t (:inherit nano-faded))))
   '(org-agenda-structure           ((t (:inherit nano-strong))))

   ;; --- Org ----------------------------------------------------------
   '(org-archived                            ((t (:inherit nano-faded))))
   '(org-block                               ((t (:inherit highlight))))
   `(org-block-begin-line                    ((t (:inherit nano-faded
                                                 :underline ,(face-background 'nano-subtle)))))
   `(org-block-end-line                      ((t (:inherit nano-faded
                                                 :overline ,(face-background 'nano-subtle)))))
   '(org-checkbox                            ((t (:inherit nano-faded))))
   '(org-checkbox-statistics-done            ((t (:inherit nano-faded))))
   '(org-checkbox-statistics-todo            ((t (:inherit nano-faded))))
   '(org-clock-overlay                       ((t (:inherit nano-faded))))
   '(org-code                                ((t (:inherit nano-salient))))
   '(org-column                              ((t (:inherit nano-faded))))
   '(org-column-title                        ((t (:inherit nano-faded))))
   '(org-date                                ((t (:inherit nano-faded))))
   '(org-date-selected                       ((t (:inherit nano-faded))))
   '(org-default                             ((t (:inherit nano-faded))))
   '(org-document-info                       ((t (:inherit nano-faded))))
   '(org-document-info-keyword               ((t (:inherit nano-faded))))
   '(org-document-title                      ((t (:inherit nano-faded))))
   '(org-done                                ((t (:inherit nano-faded))))
   '(org-drawer                              ((t (:inherit nano-faded))))
   '(org-ellipsis                            ((t (:inherit nano-faded))))
   '(org-footnote                            ((t (:inherit nano-faded))))
   '(org-formula                             ((t (:inherit nano-faded))))
   '(org-headline-done                       ((t (:inherit nano-faded))))
   ;; '(org-hide                                ((t (:inherit nano-subtle-i))))
   ;; '(org-indent                              ((t (:inherit nano-subtle-i))))
   '(org-latex-and-related                   ((t (:inherit nano-faded))))
   '(org-level-1                             ((t (:inherit nano-strong :weight bold))))
   '(org-level-2                             ((t (:inherit nano-strong))))
   '(org-level-3                             ((t (:inherit nano-strong))))
   '(org-level-4                             ((t (:inherit nano-strong))))
   '(org-level-5                             ((t (:inherit nano-strong))))
   '(org-level-6                             ((t (:inherit nano-strong))))
   '(org-level-7                             ((t (:inherit nano-strong))))
   '(org-level-8                             ((t (:inherit nano-strong))))
   '(org-link                                ((t (:inherit nano-salient))))
   '(org-list-dt                             ((t (:inherit nano-faded))))
   '(org-macro                               ((t (:inherit nano-faded))))
   '(org-meta-line                           ((t (:inherit nano-faded))))
   '(org-mode-line-clock                     ((t (:inherit nano-faded))))
   '(org-mode-line-clock-overrun             ((t (:inherit nano-faded))))
   '(org-priority                            ((t (:inherit nano-faded))))
   '(org-property-value                      ((t (:inherit nano-faded))))
   '(org-quote                               ((t (:inherit nano-faded))))
   '(org-scheduled                           ((t (:inherit nano-faded))))
   '(org-scheduled-previously                ((t (:inherit nano-faded))))
   '(org-scheduled-today                     ((t (:inherit nano-faded))))
   '(org-sexp-date                           ((t (:inherit nano-faded))))
   '(org-special-keyword                     ((t (:inherit nano-faded))))
   '(org-table                               ((t (:inherit nano-faded))))
   '(org-tag                                 ((t (:inherit nano-popout))))
   '(org-tag-group                           ((t (:inherit nano-faded))))
   '(org-target                              ((t (:inherit nano-faded))))
   '(org-time-grid                           ((t (:inherit nano-faded))))
   '(org-todo                                ((t (:inherit nano-salient :weight bold))))
   '(org-upcoming-deadline                   ((t (:inherit nano-popout))))
   '(org-verbatim                            ((t (:inherit nano-popout))))
   '(org-verse                               ((t (:inherit nano-faded))))
   '(org-warning                             ((t (:inherit nano-popout))))

   ;; --- Mu4e ---------------------------------------------------------
   '(mu4e-attach-number-face                ((t (:inherit nano-strong))))
   '(mu4e-cited-1-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-2-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-3-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-4-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-5-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-6-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-7-face                       ((t (:inherit nano-faded))))
   '(mu4e-compose-header-face                ((t (:inherit nano-faded))))
   '(mu4e-compose-separator-face             ((t (:inherit nano-faded))))
   '(mu4e-contact-face                     ((t (:inherit nano-salient))))
   '(mu4e-context-face                       ((t (:inherit nano-faded))))
   '(mu4e-draft-face                         ((t (:inherit nano-faded))))
   '(mu4e-flagged-face                     ((t (:inherit nano-salient))))
   '(mu4e-footer-face                        ((t (:inherit nano-faded))))
   '(mu4e-forwarded-face                   ((t (:inherit nano-default))))
   '(mu4e-header-face                      ((t (:inherit nano-default))))
   '(mu4e-header-highlight-face               ((t (:inherit highlight))))
   '(mu4e-header-key-face                   ((t (:inherit nano-strong))))
   '(mu4e-header-marks-face                  ((t (:inherit nano-faded))))
   '(mu4e-header-title-face                 ((t (:inherit nano-strong))))
   '(mu4e-header-field-face                 ((t (:inherit nano-strong))))
   '(mu4e-header-value-face                ((t (:inherit nano-default))))
   '(mu4e-highlight-face                    ((t (:inherit nano-popout))))
   '(mu4e-link-face                        ((t (:inherit nano-salient))))
   '(mu4e-modeline-face                      ((t (:inherit nano-faded))))
   '(mu4e-moved-face                         ((t (:inherit nano-faded))))
   '(mu4e-ok-face                            ((t (:inherit nano-faded))))
   '(mu4e-region-code                        ((t (:inherit nano-faded))))
   '(mu4e-replied-face                     ((t (:inherit nano-default))))
   '(mu4e-special-header-value-face        ((t (:inherit nano-default))))
   '(mu4e-system-face                        ((t (:inherit nano-faded))))
   '(mu4e-related-face                       ((t (:inherit nano-faded))))
   '(mu4e-title-face                        ((t (:inherit nano-strong))))
   '(mu4e-trashed-face                       ((t (:inherit nano-faded))))
   '(mu4e-unread-face                       ((t (:inherit nano-strong))))
   '(mu4e-url-number-face                    ((t (:inherit nano-faded))))
   '(mu4e-view-body-face                   ((t (:inherit nano-default))))
   '(mu4e-warning-face                      ((t (:inherit nano-popout))))

   ;; --- GNUS ---------------------------------------------------------
   '(gnus-button                            ((t (:inherit nano-salient))))
   '(gnus-cite-1                            ((t (:inherit nano-faded))))
   '(gnus-cite-10                           ((t (:inherit nano-faded))))
   '(gnus-cite-11                           ((t (:inherit nano-faded))))
   '(gnus-cite-2                            ((t (:inherit nano-faded))))
   '(gnus-cite-3                            ((t (:inherit nano-faded))))
   '(gnus-cite-4                            ((t (:inherit nano-faded))))
   '(gnus-cite-5                            ((t (:inherit nano-faded))))
   '(gnus-cite-6                            ((t (:inherit nano-faded))))
   '(gnus-cite-7                            ((t (:inherit nano-faded))))
   '(gnus-cite-8                            ((t (:inherit nano-faded))))
   '(gnus-cite-9                            ((t (:inherit nano-faded))))
   '(gnus-cite-attribution                  ((t (:inherit nano-faded))))
   '(gnus-emphasis-bold                     ((t (:inherit nano-faded))))
   '(gnus-emphasis-bold-italic              ((t (:inherit nano-faded))))
   '(gnus-emphasis-highlight-words          ((t (:inherit nano-faded))))
   '(gnus-emphasis-italic                   ((t (:inherit nano-faded))))
   '(gnus-emphasis-strikethru               ((t (:inherit nano-faded))))
   '(gnus-emphasis-underline                ((t (:inherit nano-faded))))
   '(gnus-emphasis-underline-bold           ((t (:inherit nano-faded))))
   '(gnus-emphasis-underline-bold-italic    ((t (:inherit nano-faded))))
   '(gnus-emphasis-underline-italic         ((t (:inherit nano-faded))))
   '(gnus-group-mail-1                      ((t (:inherit nano-faded))))
   '(gnus-group-mail-1-empty                ((t (:inherit nano-faded))))
   '(gnus-group-mail-2                      ((t (:inherit nano-faded))))
   '(gnus-group-mail-2-empty                ((t (:inherit nano-faded))))
   '(gnus-group-mail-3                      ((t (:inherit nano-faded))))
   '(gnus-group-mail-3-empty                ((t (:inherit nano-faded))))
   '(gnus-group-mail-low                    ((t (:inherit nano-faded))))
   '(gnus-group-mail-low-empty              ((t (:inherit nano-faded))))
   '(gnus-group-news-1                      ((t (:inherit nano-faded))))
   '(gnus-group-news-1-empty                ((t (:inherit nano-faded))))
   '(gnus-group-news-2                      ((t (:inherit nano-faded))))
   '(gnus-group-news-2-empty                ((t (:inherit nano-faded))))
   '(gnus-group-news-3                      ((t (:inherit nano-faded))))
   '(gnus-group-news-3-empty                ((t (:inherit nano-faded))))
   '(gnus-group-news-4                      ((t (:inherit nano-faded))))
   '(gnus-group-news-4-empty                ((t (:inherit nano-faded))))
   '(gnus-group-news-5                      ((t (:inherit nano-faded))))
   '(gnus-group-news-5-empty                ((t (:inherit nano-faded))))
   '(gnus-group-news-6                      ((t (:inherit nano-faded))))
   '(gnus-group-news-6-empty                ((t (:inherit nano-faded))))
   '(gnus-group-news-low                    ((t (:inherit nano-faded))))
   '(gnus-group-news-low-empty              ((t (:inherit nano-faded))))

   '(gnus-header-content                    ((t (:inherit nano-faded))))
   '(gnus-header-from                       ((t (:inherit nano-strong))))
   '(gnus-header-name                       ((t (:inherit nano-strong))))
   '(gnus-header-newsgroups                 ((t (:inherit nano-faded))))
   '(gnus-header-subject                    ((t (:inherit nano-default))))

   '(gnus-signature                         ((t (:inherit nano-faded))))
   '(gnus-splash                            ((t (:inherit nano-faded))))
   '(gnus-summary-cancelled                 ((t (:inherit nano-faded))))
   '(gnus-summary-high-ancient              ((t (:inherit nano-faded))))
   '(gnus-summary-high-read                 ((t (:inherit nano-faded))))
   '(gnus-summary-high-ticked               ((t (:inherit nano-faded))))
   '(gnus-summary-high-undownloaded         ((t (:inherit nano-faded))))
   '(gnus-summary-high-unread               ((t (:inherit nano-faded))))
   '(gnus-summary-low-ancient               ((t (:inherit nano-faded))))
   '(gnus-summary-low-read                  ((t (:inherit nano-faded))))
   '(gnus-summary-low-ticked                ((t (:inherit nano-faded))))
   '(gnus-summary-low-undownloaded          ((t (:inherit nano-faded))))
   '(gnus-summary-low-unread                ((t (:inherit nano-faded))))
   '(gnus-summary-normal-ancient            ((t (:inherit nano-faded))))
   '(gnus-summary-normal-read               ((t (:inherit nano-faded))))
   '(gnus-summary-normal-ticked             ((t (:inherit nano-faded))))
   '(gnus-summary-normal-undownloaded       ((t (:inherit nano-faded))))
   '(gnus-summary-normal-unread             ((t (:inherit nano-faded))))
   '(gnus-summary-selected                  ((t (:inherit nano-faded))))

   ;; --- Marginalia ---------------------------------------------------
   '(marginalia-archive                     ((t (:inherit nano-faded))))
   '(marginalia-char                        ((t (:inherit nano-faded))))
   '(marginalia-date                        ((t (:inherit nano-faded))))
   '(marginalia-documentation               ((t (:inherit nano-faded))))
   '(marginalia-file-name                   ((t (:inherit nano-faded))))
   '(marginalia-file-owner                  ((t (:inherit nano-faded))))
   '(marginalia-file-priv-dir               ((t (:inherit nano-faded))))
   '(marginalia-file-priv-exec              ((t (:inherit nano-faded))))
   '(marginalia-file-priv-link              ((t (:inherit nano-faded))))
   '(marginalia-file-priv-no                ((t (:inherit nano-faded))))
   '(marginalia-file-priv-other             ((t (:inherit nano-faded))))
   '(marginalia-file-priv-rare              ((t (:inherit nano-faded))))
   '(marginalia-file-priv-read              ((t (:inherit nano-faded))))
   '(marginalia-file-priv-write             ((t (:inherit nano-faded))))
   '(marginalia-function                    ((t (:inherit nano-faded))))
   '(marginalia-installed                   ((t (:inherit nano-faded))))
   '(marginalia-key                         ((t (:inherit nano-faded))))
   '(marginalia-lighter                     ((t (:inherit nano-faded))))
   '(marginalia-list                        ((t (:inherit nano-faded))))
   '(marginalia-mode                        ((t (:inherit nano-faded))))
   '(marginalia-modified                    ((t (:inherit nano-faded))))
   '(marginalia-null                        ((t (:inherit nano-faded))))
   '(marginalia-number                      ((t (:inherit nano-faded))))
   '(marginalia-off                         ((t (:inherit nano-faded))))
   '(marginalia-on                          ((t (:inherit nano-faded))))
   '(marginalia-size                        ((t (:inherit nano-faded))))
   '(marginalia-string                      ((t (:inherit nano-faded))))
   '(marginalia-symbol                      ((t (:inherit nano-faded))))
   '(marginalia-true                        ((t (:inherit nano-faded))))
   '(marginalia-type                        ((t (:inherit nano-faded))))
   '(marginalia-value                       ((t (:inherit nano-faded))))
   '(marginalia-version                     ((t (:inherit nano-faded))))

   ;; --- Elfeed -------------------------------------------------------
    '(elfeed-log-date-face                    ((t (:inherit nano-faded))))
    '(elfeed-log-info-level-face            ((t (:inherit nano-default))))
    '(elfeed-log-debug-level-face           ((t (:inherit nano-default))))
    '(elfeed-log-warn-level-face             ((t (:inherit nano-popout))))
    '(elfeed-log-error-level-face            ((t (:inherit nano-popout))))
    '(elfeed-search-tag-face                  ((t (:inherit nano-faded))))
    '(elfeed-search-date-face                 ((t (:inherit nano-faded))))
    '(elfeed-search-feed-face               ((t (:inherit nano-salient))))
    '(elfeed-search-filter-face               ((t (:inherit nano-faded))))
    '(elfeed-search-last-update-face        ((t (:inherit nano-salient))))
    '(elfeed-search-title-face              ((t (:inherit nano-default))))
    '(elfeed-search-tag-face                  ((t (:inherit nano-faded))))
    '(elfeed-search-unread-count-face        ((t (:inherit nano-strong))))
    '(elfeed-search-unread-title-face        ((t (:inherit nano-strong :weight bold))))

    ;; --- Deft --------------------------------------------------------
    '(deft-filter-string-error-face         ((t (:inherit nano-popout))))
    '(deft-filter-string-face              ((t (:inherit nano-default))))
    '(deft-header-face                     ((t (:inherit nano-salient))))
    '(deft-separator-face                    ((t (:inherit nano-faded))))
    '(deft-summary-face                      ((t (:inherit nano-faded))))
    '(deft-time-face                       ((t (:inherit nano-salient))))
    '(deft-title-face                       ((t (:inherit nano-strong))))

    ;; --- imenu-list ---------------------------------------------------
    '(imenu-list-entry-face                 ((t (:inherit nano-default))))
    '(imenu-list-entry-face-0                ((t (:inherit nano-strong))))
    '(imenu-list-entry-face-1               ((t ( ))))
    '(imenu-list-entry-face-2               ((t ( ))))
    '(imenu-list-entry-face-3               ((t ( ))))
    '(imenu-list-entry-subalist-face-0      ((t (:inherit nano-strong))))
    '(imenu-list-entry-subalist-face-1      ((t ( ))))
    '(imenu-list-entry-subalist-face-2      ((t ( ))))
    '(imenu-list-entry-subalist-face-3      ((t ( ))))

    ;; --- Restructured text -------------------------------------------
    '(rst-adornment                           ((t (:inherit nano-faded))))
    '(rst-block                             ((t (:inherit nano-default))))
    '(rst-comment                             ((t (:inherit nano-faded))))
    '(rst-definition                        ((t (:inherit nano-salient))))
    '(rst-directive                         ((t (:inherit nano-salient))))
    '(rst-emphasis1                           ((t (:inherit nano-faded))))
    '(rst-emphasis2                          ((t (:inherit nano-strong))))
    '(rst-external                          ((t (:inherit nano-salient))))
    '(rst-level-1                            ((t (:inherit nano-strong))))
    '(rst-level-2                            ((t (:inherit nano-strong))))
    '(rst-level-3                            ((t (:inherit nano-strong))))
    '(rst-level-4                            ((t (:inherit nano-strong))))
    '(rst-level-5                            ((t (:inherit nano-strong))))
    '(rst-level-6                            ((t (:inherit nano-strong))))
    '(rst-literal                           ((t (:inherit nano-salient))))
    '(rst-reference                         ((t (:inherit nano-salient))))
    '(rst-transition                        ((t (:inherit nano-default))))

    ;; ---SHR ---------------------------------------------------------
    '(shr-abbreviation                    ((t (:inherit nano-popout))))
    '(shr-h1                              ((t (:inherit nano-strong))))
    '(shr-h2                              ((t (:inherit nano-strong))))
    '(shr-h3                              ((t (:inherit nano-strong))))
    '(shr-h4                              ((t (:inherit nano-strong))))
    '(shr-h5                              ((t (:inherit nano-strong))))
    '(shr-h6                              ((t (:inherit nano-strong))))
    '(shr-link                           ((t (:inherit nano-salient))))
    '(shr-selected-link      ((t (:inherit (nano-salient nano-subtle)))))
    '(shr-strike-through                   ((t (:inherit nano-faded))))

    ;; --- Markdown ----------------------------------------------------
    '(markdown-blockquote-face              ((t (:inherit nano-default))))
    '(markdown-bold-face                    ((t (:inherit nano-strong))))
    '(markdown-code-face                    ((t (:inherit nano-default))))
    '(markdown-comment-face                 ((t (:inherit nano-faded))))
    '(markdown-footnote-marker-face         ((t (:inherit nano-default))))
    '(markdown-footnote-text-face           ((t (:inherit nano-default))))
    '(markdown-gfm-checkbox-face            ((t (:inherit nano-default))))
    `(markdown-header-delimiter-face        ((t (:foreground ,frost-2))))
    '(markdown-header-face                  ((t (:inherit nano-strong))))
    `(markdown-header-face-1                ((t (:foreground ,frost-1))))
    `(markdown-header-face-2                ((t (:foreground ,frost-1))))
    `(markdown-header-face-3                ((t (:foreground ,frost-1))))
    `(markdown-header-face-4                ((t (:foreground ,frost-1))))
    `(markdown-header-face-5                ((t (:foreground ,frost-1))))
    `(markdown-header-face-6                ((t (:foreground ,frost-1))))
    '(markdown-header-rule-face             ((t (:inherit nano-default))))
    '(markdown-highlight-face               ((t (:inherit nano-default))))
    '(markdown-hr-face                      ((t (:inherit nano-default))))
    '(markdown-html-attr-name-face          ((t (:inherit nano-default))))
    '(markdown-html-attr-value-face         ((t (:inherit nano-default))))
    '(markdown-html-entity-face             ((t (:inherit nano-default))))
    '(markdown-html-tag-delimiter-face      ((t (:inherit nano-default))))
    '(markdown-html-tag-name-face           ((t (:inherit nano-default))))
    `(markdown-inline-code-face             ((t (:foreground ,frost-0))))
    '(markdown-italic-face                  ((t (:inherit nano-faded))))
    '(markdown-language-info-face           ((t (:inherit nano-default))))
    `(markdown-language-keyword-face        ((t (:foreground ,frost-0))))
    '(markdown-line-break-face              ((t (:inherit nano-default))))
    `(markdown-link-face                    ((t (:foreground ,frost-1))))
    '(markdown-link-title-face              ((t (:inherit nano-default))))
    '(markdown-list-face                    ((t (:inherit nano-faded))))
    `(markdown-markup-face                  ((t (:foreground ,frost-0))))
    '(markdown-math-face                    ((t (:inherit nano-default))))
    '(markdown-metadata-key-face            ((t (:inherit nano-faded))))
    '(markdown-metadata-value-face          ((t (:inherit nano-faded))))
    '(markdown-missing-link-face            ((t (:inherit nano-default))))
    '(markdown-plain-url-face               ((t (:inherit nano-default))))
    '(markdown-pre-face                     ((t (:inherit nano-default))))
    '(markdown-reference-face               ((t (:inherit nano-salient))))
    '(markdown-strike-through-face          ((t (:inherit nano-faded))))
    '(markdown-table-face                   ((t (:inherit nano-default))))
    `(markdown-url-face                     ((t (:foreground ,snow-storm-0
                                                 :underline t))))
    ;; --- Magit (WIP) ---------------------------------------------------
    '(magit-blame-highlight                  ((t (:inherit (highlight)))))
    '(magit-diff-added-highlight             ((t (:inherit (highlight nano-salient nano-strong)))))
    '(magit-diff-base-highlight              ((t (:inherit (highlight)))))
    '(magit-diff-context-highlight           ((t (:inherit (highlight nano-faded)))))
    '(magit-diff-file-heading-highlight      ((t (:inherit (highlight nano-strong)))))
    '(magit-diff-hunk-heading-highlight      ((t (:inherit (nano-default)))))
    '(magit-diff-our-highlight               ((t (:inherit (highlight)))))
    '(magit-diff-removed-highlight           ((t (:inherit (highlight nano-popout nano-strong)))))
    '(magit-diff-revision-summary-highlight  ((t (:inherit ()))))
    '(magit-diff-their-highlight             ((t (:inherit (highlight)))))
    '(magit-section-highlight                ((t (:inherit (highlight)))))

    '(magit-blame-heading                    ((t (:inherit (nano-subtle nano-strong)))))
    '(magit-diff-conflict-heading            ((t (:inherit (nano-subtle nano-strong)))))
    '(magit-diff-file-heading                ((t (:inherit (nano-strong)))))
    '(magit-diff-hunk-heading                ((t (:inherit (nano-subtle nano-default)))))
    '(magit-diff-lines-heading               ((t (:inherit (nano-subtle nano-strong)))))
    '(magit-section-heading                  ((t (:inherit (nano-salient nano-strong)))))

    '(magit-bisect-bad                       ((t (:inherit nano-default))))
    '(magit-bisect-good                      ((t (:inherit nano-default))))
    '(magit-bisect-skip                      ((t (:inherit nano-default))))
    '(magit-blame-date                       ((t (:inherit nano-default))))
    '(magit-blame-dimmed                     ((t (:inherit nano-default))))
    '(magit-blame-hash                       ((t (:inherit nano-faded))))

    '(magit-blame-margin                     ((t (:inherit nano-default))))
    '(magit-blame-name                       ((t (:inherit nano-default))))
    '(magit-blame-summary                    ((t (:inherit nano-default))))

    '(magit-branch-current                   ((t (:inherit (nano-strong nano-salient)))))
    '(magit-branch-local                     ((t (:inherit nano-salient))))
    '(magit-branch-remote                    ((t (:inherit (nano-salient)))))
    '(magit-branch-remote-head               ((t (:inherit (nano-salient)))))
    '(magit-branch-upstream                  ((t (:inherit (nano-salient)))))

    '(magit-cherry-equivalent                ((t (:inherit nano-default))))
    '(magit-cherry-unmatched                 ((t (:inherit nano-default))))

    '(magit-diff-added                       ((t (:inherit (highlight nano-salient nano-strong)))))
    '(magit-diff-base                        ((t (:inherit nano-default))))
    '(magit-diff-context                     ((t (:inherit (highlight nano-faded)))))
    '(magit-diff-file-heading-selection      ((t (:inherit nano-default))))
    '(magit-diff-hunk-heading-selection      ((t (:inherit nano-default))))
    '(magit-diff-hunk-region                 ((t (:inherit nano-default))))
    '(magit-diff-lines-boundary              ((t (:inherit nano-default))))
    '(magit-diff-our                         ((t (:inherit nano-default))))
    '(magit-diff-removed                     ((t (:inherit (highlight nano-popout nano-strong)))))
    '(magit-diff-revision-summary            ((t (:inherit nano-popout))))
    '(magit-diff-their                       ((t (:inherit nano-default))))
    '(magit-diff-whitespace-warning          ((t (:inherit nano-subtle))))
    '(magit-diffstat-added                   ((t (:inherit nano-default))))
    '(magit-diffstat-removed                 ((t (:inherit nano-default))))

    '(magit-dimmed                           ((t (:inherit nano-faded))))
    '(magit-filename                         ((t (:inherit nano-default))))
    '(magit-hash                             ((t (:inherit nano-faded))))
    '(magit-head                             ((t (:inherit nano-default))))
    '(magit-header-line                      ((t (:inherit nano-default))))
    '(magit-header-line-key                  ((t (:inherit nano-default))))
    '(magit-header-line-log-select           ((t (:inherit nano-default))))

    '(magit-keyword                          ((t (:inherit nano-salient))))
    '(magit-keyword-squash                   ((t (:inherit nano-salient))))

    '(magit-log-author                       ((t (:inherit nano-default))))
    '(magit-log-date                         ((t (:inherit nano-default))))
    '(magit-log-graph                        ((t (:inherit nano-default))))

    '(magit-mode-line-process                ((t (:inherit nano-default))))
    '(magit-mode-line-process-error          ((t (:inherit nano-critical))))

    '(magit-process-ng                       ((t (:inherit nano-default))))
    '(magit-process-ok                       ((t (:inherit nano-default))))

    '(magit-reflog-amend                     ((t (:inherit nano-default))))
    '(magit-reflog-checkout                  ((t (:inherit nano-default))))
    '(magit-reflog-cherry-pick               ((t (:inherit nano-default))))
    '(magit-reflog-commit                    ((t (:inherit nano-default))))
    '(magit-reflog-merge                     ((t (:inherit nano-default))))
    '(magit-reflog-other                     ((t (:inherit nano-default))))
    '(magit-reflog-rebase                    ((t (:inherit nano-default))))
    '(magit-reflog-remote                    ((t (:inherit nano-default))))
    '(magit-reflog-reset                     ((t (:inherit nano-default))))
    '(magit-refname                          ((t (:inherit nano-default))))
    '(magit-refname-pullreq                  ((t (:inherit nano-default))))
    '(magit-refname-stash                    ((t (:inherit nano-default))))
    '(magit-refname-wip                      ((t (:inherit nano-default))))

    '(magit-section-heading-selection        ((t (:inherit nano-default))))
    '(magit-section-secondary-heading        ((t (:inherit nano-default))))
    '(magit-sequence-done                    ((t (:inherit nano-default))))
    '(magit-sequence-drop                    ((t (:inherit nano-default))))
    '(magit-sequence-exec                    ((t (:inherit nano-default))))
    '(magit-sequence-head                    ((t (:inherit nano-default))))
    '(magit-sequence-onto                    ((t (:inherit nano-default))))
    '(magit-sequence-part                    ((t (:inherit nano-default))))
    '(magit-sequence-pick                    ((t (:inherit nano-default))))
    '(magit-sequence-stop                    ((t (:inherit nano-default))))

    '(magit-signature-bad                    ((t (:inherit nano-default))))
    '(magit-signature-error                  ((t (:inherit nano-default))))
    '(magit-signature-expired                ((t (:inherit nano-default))))
    '(magit-signature-expired-key            ((t (:inherit nano-default))))
    '(magit-signature-good                   ((t (:inherit nano-default))))
    '(magit-signature-revoked                ((t (:inherit nano-default))))
    '(magit-signature-untrusted              ((t (:inherit nano-default))))

    '(magit-tag                              ((t (:inherit nano-strong))))

    ;; --- Transient ------------------------------------------------------
    ;; Set only faces that influence Magit.  See:
    ;; <https://github.com/rougier/nano-theme/issues/43>
    '(transient-value                        ((t (:inherit default))))

    ;; --- ANSI colors ----------------------------------------------------
    '(ansi-color-black          ((t (:inherit nano-default))))
    '(ansi-color-bold           ((t (:inherit nano-strong))))
    '(ansi-color-bright-black   ((t (:inherit nano-strong))))
    '(ansi-color-faint          ((t (:inherit nano-faded))))
    '(ansi-color-fast-blink     ((t (:inherit nano-faded))))
    '(ansi-color-slow-blink     ((t (:inherit nano-faded))))
    '(ansi-color-inverse        ((t (:inherit nano-default-i))))
    '(ansi-color-italic         ((t (:inherit italic))))
    '(ansi-color-underline      ((t (:inherit nano-faded))))
    `(ansi-color-blue           ((t (:foreground ,frost-2))))
    `(ansi-color-bright-blue    ((t (:background ,frost-1))))
    `(ansi-color-cyan           ((t (:foreground ,frost-0))))
    `(ansi-color-bright-cyan    ((t (:background ,frost-0))))
    `(ansi-color-green          ((t (:foreground ,aurora-3))))
    `(ansi-color-bright-green   ((t (:background ,aurora-3))))
    `(ansi-color-magenta        ((t (:foreground ,aurora-4))))
    `(ansi-color-bright-magenta ((t (:background ,aurora-4))))
    `(ansi-color-red            ((t (:foreground ,aurora-0))))
    `(ansi-color-bright-red     ((t (:background ,aurora-1))))
    '(ansi-color-white          ((t (:inherit nano-subtle))))
    '(ansi-color-bright-white   ((t (:inherit default))))
    `(ansi-color-yellow         ((t (:foreground ,aurora-2))))
    `(ansi-color-bright-yellow  ((t (:background ,aurora-2))))

    ;; --- Terminal ----------------------------------------------------
    '(term-bold          ((t (:inherit nano-strong))))
    '(term-color-black   ((t (:inherit default))))
    `(term-color-blue    ((t (:foreground ,frost-2
                              :background ,frost-1))))
    `(term-color-cyan    ((t (:foreground ,frost-0
                              :background ,frost-0))))
    `(term-color-green   ((t (:foreground ,aurora-3
                              :background ,aurora-3))))
    `(term-color-magenta ((t (:foreground ,aurora-4
                              :background ,aurora-4))))
    `(term-color-red     ((t (:foreground ,aurora-0
                              :background ,aurora-1))))
    `(term-color-yellow  ((t (:foreground ,aurora-2
                              :background ,aurora-2))))

    ;; --- Avy ---------------------------------------------------------
    `(avy-lead-face   ((t (:background ,aurora-0))))
    `(avy-lead-face-0 ((t (:background ,frost-2))))
    `(avy-lead-face-1 ((t (:background ,frost-3))))
    `(avy-lead-face-2 ((t (:background ,aurora-4))))

    ;; --- Calendar ----------------------------------------------------
    `(calendar-today ((t (:background unspecified))))
    `(holiday        ((t (:background unspecified
                          :foreground ,aurora-2))))

    ;; --- Diff-hl -----------------------------------------------------
    `(diff-hl-insert        ((t (:background ,aurora-3
                                 :foreground ,aurora-3))))
    `(diff-hl-change        ((t (:background ,aurora-2
                                 :foreground ,aurora-2))))
    `(diff-hl-delete        ((t (:background ,aurora-0
                                 :foreground ,aurora-0))))
    `(diff-hl-margin-insert ((t (:foreground ,aurora-3))))
    `(diff-hl-margin-change ((t (:foreground ,aurora-2))))
    `(diff-hl-margin-delete ((t (:foreground ,aurora-0))))

    ;; --- Dired -------------------------------------------------------
    `(dired-directory ((t (:foreground ,frost-2
                           :weight bold))))

    ;; --- Eglot -------------------------------------------------------
    `(eglot-diagnostic-tag-deprecated-face  ((t (:background ,polar-night-4
                                                 :foreground ,snow-storm-2
                                                 :underline unspecified))))
    `(eglot-diagnostic-tag-unnecessary-face ((t (:background ,frost-2
                                                 :foreground ,polar-night-0
                                                 :underline unspecified))))

    ;; --- Ethan-wspace ------------------------------------------------
    `(ethan-wspace-face ((t (:background ,aurora-2
                             :foreground unspecified))))

    ;; --- Flymake -----------------------------------------------------
    `(flymake-error        ((t (:background ,aurora-0
                                :foreground ,snow-storm-2
                                :underline unspecified))))
    `(flymake-error-echo   ((t (:foreground ,aurora-0))))
    `(flymake-warning      ((t (:background ,aurora-2
                                :foreground ,polar-night-0
                                :underline unspecified))))
    `(flymake-warning-echo ((t (:foreground ,aurora-2))))
    `(flymake-note         ((t (:background ,frost-2
                                :foreground ,polar-night-0
                                :underline unspecified))))
    `(flymake-note-echo    ((t (:foreground ,frost-2))))

    ;; --- Highlight-indent --------------------------------------------
    `(highlight-indent-guides-character-face     ((t (:foreground ,polar-night-1))))
    `(highlight-indent-guides-top-character-face ((t (:foreground ,polar-night-3))))

    ;; --- Ivy ---------------------------------------------------------
    `(ivy-minibuffer-match-face-1 ((t (:background unspecified
                                       :foreground ,frost-0))))
    `(ivy-minibuffer-match-face-2 ((t (:background unspecified
                                       :foreground ,frost-0))))
    `(ivy-minibuffer-match-face-3 ((t (:background unspecified
                                       :foreground ,frost-0))))
    `(ivy-minibuffer-match-face-4 ((t (:background unspecified
                                       :foreground ,frost-0))))
    `(ivy-current-match           ((t (:background ,polar-night-3
                                       :foreground ,snow-storm-0
                                       :weight bold))))

    ;; --- Make --------------------------------------------------------
    '(makefile-ts-mode-target-face ((t (:inherit nano-salient))))

    ;; --- Pip-requirements --------------------------------------------
    '(pip-requirements-name-regex-face      ((t (:inherit snow-storm-0))))
    `(pip-requirements-version-regex-1-face ((t (:foreground ,frost-2))))
    `(pip-requirements-version-regex-2-face ((t (:foreground ,aurora-4))))

    ;; --- Whitespace --------------------------------------------------
    `(whitespace-newline                ((t (:background ,polar-night-0
                                             :foreground ,polar-night-4))))
    `(whitespace-missing-newline-at-eof ((t (:background ,polar-night-0
                                             :foreground ,polar-night-4))))
    `(whitespace-space                  ((t (:background ,polar-night-0
                                             :foreground ,polar-night-4))))
    `(whitespace-space-after-tab        ((t (:background ,polar-night-0
                                             :foreground ,polar-night-4))))
    `(whitespace-space-before-tab       ((t (:background ,polar-night-0
                                             :foreground ,polar-night-4))))
    `(whitespace-tab                    ((t (:background ,polar-night-0
                                             :foreground ,polar-night-4))))
    `(whitespace-trailing               ((t (:background ,polar-night-0
                                             :foreground ,polar-night-4))))

    ))

  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'nano-faded))))
  (advice-remove 'frame-list #'nano-frame-list-advice-selected))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'nano-theme-support)
;;; nano-theme-support.el ends here
