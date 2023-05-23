;; Libraries load path
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path
             "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e")

;; Setup dependencies
(require 'nano-package)


(setq package-list
      '(ag                     ; Integration of ag search utility
        cape                   ; Completion At Point Extensions
        company                ; Modular auto-completion framework
        consult                ; Consulting completing-read
        consult-recoll         ; Consult interface for recoll query
        corfu                  ; COmpletion Overlay Region Function
        counsel                ; Improves ivy integration
        deft                   ; Quickly browse, filter, and edit plain text notes
        f                      ; Modern API for working with files and directories
        flycheck               ; Modern on-the-fly syntax checking extension
        flyspell-correct-popup ; Correcting words with flyspell via popup interface
        flyspell-popup         ; Correcting words with flyspell in popup menus
        guess-language         ; Robust automatic language detection
        helpful                ; A better help buffer
        htmlize                ; Convert buffer text and decorations to HTML
        imenu-list             ; Show imenu entries in a separate buffer
        ivy                    ; A generic completion frontend for Emacs
        lsp-ivy                ; Provides an ivy interface to the workspace symbol functionality offered by lsp-mode
        lsp-mode               ; Emacs client/library for the Language Server Protocol
        lsp-pyright            ; Integration of Pyright with LSP mode
        magit                  ; A Git porcelain inside Emacs.
        marginalia             ; Enrich existing commands with completion annotations
        markdown-mode          ; Major mode for Markdown-formatted text
        mini-frame             ; Show minibuffer in child frame on read-from-minibuffer
        multi-term             ; Managing multiple terminal buffers in Emacs.
        orderless              ; Completion style for matching regexps in any order
        org-auto-tangle        ; Tangle org file when it is saved
        pinentry               ; GnuPG Pinentry server implementation
        poetry                 ; Python dependency management and packaging in Emacs
        projectile             ; Project interaction library for Emacs
        py-isort               ; Reformat python-mode buffer with isort
        python-black           ; Reformat Python using black-macchiato
        smex                   ; A smart M-x enhancement for Emacs
        swiper                 ; Alternative to isearch that uses Ivy to show an overview of all matches
        tree-sitter            ; An incremental parsing system for programming tools
        tree-sitter-langs      ; Language bundle for Emacs's tree-sitter package
        use-package            ; A configuration macro for simplifying your .emacs
        vertico                ; VERTical Interactive Completion
        yaml-mode              ; YAML mode
        yasnippet              ; A template system for Emacs
        which-key))            ; Display available keybindings in popup

;; Install packages that are not yet installed
(dolist (package package-list)
  (straight-use-package package))

;; Special case for pdf-tools that has recently (2022) changed maintainer
(straight-use-package
 '(pdf-tools :type git :host github :repo "vedang/pdf-tools"))

;; Denote not yet on ELPA (2022-06-19)
(straight-use-package
 '(denote :type git :host github :repo "protesilaos/denote"))

;; Display org properties in the agenda buffer (modified version)
(straight-use-package
 '(org-agenda-property :type git :host github :repo "Malabarba/org-agenda-property"
                       :fork (:host github :repo "rougier/org-agenda-property")))

;; SVG tags, progress bars & icons
(straight-use-package
 '(svg-lib :type git :host github :repo "rougier/svg-lib"))

;; Replace keywords with SVG tags
(straight-use-package
 '(svg-tag-mode :type git :host github :repo "rougier/svg-tag-mode"))

;; Dashboard for mu4e
(straight-use-package
 '(mu4e-dashboard :type git :host github :repo "rougier/mu4e-dashboard"))

;; Folding mode for mu4e
(straight-use-package
 '(mu4e-folding :type git :host github :repo "rougier/mu4e-folding"))

;; Relative date formatting
(straight-use-package
 '(relative-date :type git :host github :repo "rougier/relative-date"))

;; org imenu
(straight-use-package
 '(org-imenu :type git :host github :repo "rougier/org-imenu"))

;; pdf-drop-mode
(straight-use-package
 '(pdf-drop-mode :type git :host github :repo "rougier/pdf-drop-mode"))

;; Load settings
(require 'nano)
