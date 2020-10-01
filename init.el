;;; personal-init --- Micah

;;; Commentary:
;;
;; Editor Features/Subsystems/Windows:
;; - help system/browser
;; - terminal (vterm)
;; - menu nav (imenu)
;; - git (magit)
;; - buffer browser
;; - tree viewer (neotree)
;; - popup search (ivy etc)
;; - workspace switcher (perspectives)


;;; Code:


;; https://emacs.stackexchange.com/a/4258/11025
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))


;;; PACKAGING

;; https://emacs.stackexchange.com/a/16832/11025
;; package.el config from https://github.com/flyingmachine/emacs.d
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ;;("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages
  '(
    ace-window
    all-the-icons
    all-the-icons-dired
    all-the-icons-ivy-rich
    ag
    ample-theme
    auto-dim-other-buffers
    avy
    beacon
    buffer-move
    cider
    cider-eval-sexp-fu
    cljr-ivy
    clj-refactor
    clojure-essential-ref
    clojure-essential-ref-nov
    clojure-mode
    clojure-mode-extra-font-locking
    comment-dwim-2
    company
    company-box
    company-flx
    company-fuzzy
    company-quickhelp
    company-posframe
    company-terraform
    counsel
    counsel-projectile
    crux
    csv
    cucumber-goto-step
    cycle-quotes
    delight
    diff-hl
    diminish
    discover-clj-refactor
    doom-modeline
    dot-mode
    dumb-jump
    edbi
    easy-kill
    edit-indirect
    envrc
    eyebrowse
    exec-path-from-shell
    expand-region
    feature-mode
    fic-mode
    flx-ido
    flycheck-clj-kondo
    flycheck-clojure
    flycheck-joker
    flycheck-pos-tip
    flycheck-yamllint
    flymd
    focus
    git-identity
    git-link
    git-messenger
    git-timemachine
    github-browse-file
    helpful
    highlight-parentheses
    highlight-thing
    httprepl
    ibuffer-vc
    ido
    ido-completing-read+
    imenu-anywhere
    imenu-list
    ivy
    ivy-hydra
    ivy-rich
    jump-char
    key-chord
    kibit-helper
    nlinum-relative
    magit
    markdown-mode
    mic-paren
    mixed-pitch
    mode-icons
    move-text
    neotree
    nov
    org-bullets
    org-preview-html
    outshine
    navi-mode
    page-break-lines
    paren-face
    popup
    popup-imenu
    projectile
    rainbow-delimiters
    rainbow-identifiers
    restclient
    ripgrep
    rubocop
    shrink-whitespace
    smartparens
    string-inflection
    smex
    symbol-overlay
    terraform-doc
    terraform-mode
    toggle-quotes
    total-lines
    typo
    undo-tree
    unfill
    visible-mark
    vterm
    vterm-toggle
    which-key
    yaml-mode
    zoom))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/vendor")




;; '(cursor-type (quote (bar . 2)))
;; '(blink-cursor-blinks 2)
;; '(blink-cursor-interval 0.2)

;; '(default ((t (:inherit nil :foreground "#F8F8F2" :height 60 :family "Fantasque Sans Mono"))))
;; '(default ((t (:inherit nil :stipple nil :background "gray16" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 60 :width normal :foundry "nil" :family "Fantasque Sans Mono"))))
;; '(org-block ((t (:background "#3E3D31" :foreground "#F8F8F0" :family "Fantasque Sans Mono"))))
;; '(org-code ((t (:foreground "#75715E" :family "Fantasque Sans Mono"))))
;; '(page-break-lines ((t (:slant normal :weight normal :height 180 :width condensed :family "Fantasque Sans Mono"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray16" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "nil" :family "Fira Code"))))
 '(auto-dim-other-buffers-face ((t (:background "gray29"))))
 '(aw-leading-char-face ((t (:foreground "red" :height 5.0))))
 '(company-box-background ((t (:background "black" :inverse-video nil))))
 '(cursor ((t (:background "red" :foreground "#272822"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#75715E"))))
 '(font-lock-comment-face ((t (:foreground "#75715E"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "aquamarine3" :slant italic :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#A6E22E" :underline t :weight ultra-bold))))
 '(font-lock-type-face ((t (:foreground "#66D9EF" :slant italic :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#A6E22E"))))
 '(hl-line ((t (:background "#000000"))))
 '(markdown-code-face ((t (:inherit code-face))))
 '(markdown-header-delimiter-face ((t (:inherit markdown-markup-face))))
 '(markdown-header-face ((t (:inherit variable-pitch))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :foreground "pale turquoise" :weight bold :height 1.5))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :slant normal :weight bold :height 1.3))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :slant italic :height 1.05))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :slant italic :height 1.0))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))))
 '(markdown-italic-face ((t (:inherit italic :slant italic))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face))))
 '(org-block ((t (:background "#3E3D31" :foreground "#F8F8F0" :family "Fira Code"))))
 '(org-code ((t (:foreground "#75715E" :family "Fira Code"))))
 '(org-document-title ((t (:inherit variable-pitch :foreground "pale turquoise" :weight bold :height 2.0))))
 '(org-level-1 ((t (:inherit (outline-1 variable-pitch) :height 2.0))))
 '(org-level-2 ((t (:inherit (outline-2 variable-pitch) :slant italic :height 1.6))))
 '(org-level-3 ((t (:inherit (outline-3 variable-pitch)))))
 '(page-break-lines ((t (:slant normal :weight normal :height 180 :width condensed :family "Fira Code"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "chartreuse" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep sky blue" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :weight bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "chartreuse" :weight bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "deep sky blue" :weight bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "yellow" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "chartreuse" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "deep sky blue" :weight bold))))
 '(region ((t (:inherit highlight :background "slate blue"))))
 '(swiper-line-face ((t (:background "purple4"))))
 '(variable-pitch ((t (:height 1.0 :family "Fira Sans"))))
 '(visible-mark-face1 ((t (:background "DarkOrange3"))))
 '(visible-mark-face2 ((t (:background "burlywood4"))))
 '(which-key-command-description-face ((t nil)))
 '(whitespace-tab ((t (:background "purple4" :foreground "#757575")))))

;; ((((((((()))))))))

;; '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1" :weight bold))))
;; '(font-lock-comment-delimiter-face ((t (:foreground "#75715E"))))
;; '(font-lock-comment-face ((t (:foreground "#75715E" :height 0.9 :family "Delius"))))
;; '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "dodger blue" :height 1.1 :family "Alegreya Sans"))))
;; '(font-lock-function-name-face ((t (:foreground "#A6E22E" :underline t :weight ultra-bold))))
;; '(font-lock-type-face ((t (:foreground "#66D9EF" :slant italic :weight bold))))


;;; BINDINGS

(require 'key-chord)
(key-chord-mode +1)
(setq key-chord-two-keys-delay .1 ; default is .1
      key-chord-one-key-delay  .6) ; default is .2

;; Special
(key-chord-define-global "q'" 'crux-smart-open-line-above)
(key-chord-define-global "q-" 'text-scale-decrease)
(key-chord-define-global "q+" 'text-scale-increase)
(key-chord-define-global "qq" 'aw-flip-window)
(key-chord-define-global "''" 'aw-flip-window)
;; TODO: multiple cursors or

;; A — EMPTY

;; B — Buffer/file
(let ((my-buffer-keymap (make-sparse-keymap)))
  ;; Popups and immediate changes lower case
  (define-key my-buffer-keymap "b" 'crux-switch-to-previous-buffer) ; default
  (define-key my-buffer-keymap "a" 'counsel-switch-buffer) ; all
  (define-key my-buffer-keymap "p" 'counsel-projectile-switch-to-buffer) ; project
  (define-key my-buffer-keymap "P" 'projectile-ibuffer)
  (define-key my-buffer-keymap "i" 'counsel-ibuffer) ; ibuffer
  (define-key my-buffer-keymap "e" 'counsel-buffer-or-recentf)
  (define-key my-buffer-keymap "f" 'counsel-recentf) ; file
  (key-chord-define-global "qb" my-buffer-keymap)
  (key-chord-define-global "'b" my-buffer-keymap))

;; C — Character goto (fast)
(key-chord-define-global "'c" 'avy-goto-char-timer) ; character
(key-chord-define-global "qc" 'avy-goto-char-timer)
(key-chord-define-global "\"C" 'beacon-blink) ; cursor

;; D — neotree
(key-chord-define-global "'d" 'neotree-show)
(key-chord-define-global "\"D" 'neotree-toggle)

;; E — Errors
(let ((my-flycheck-keymap (make-sparse-keymap)))
  (define-key my-flycheck-keymap "e" 'flycheck-next-error) ; default
  (define-key my-flycheck-keymap "c" 'flycheck-buffer) ; not working?
  (define-key my-flycheck-keymap "E" 'flycheck-explain-error-at-point) ; not useful
  (define-key my-flycheck-keymap "l" 'flycheck-list-errors)
  (define-key my-flycheck-keymap "n" 'flycheck-next-error)
  (define-key my-flycheck-keymap "p" 'flycheck-previous-error)
  (define-key my-flycheck-keymap "C" 'flycheck-compile)
  (key-chord-define-global "qe" my-flycheck-keymap))

;; F - Find/search
(let ((my-find-keymap (make-sparse-keymap)))
  (define-key my-find-keymap "f" 'swiper-isearch)
  (define-key my-find-keymap "o" 'ivy-occur)
  (key-chord-define-global "qf" my-find-keymap)
  (key-chord-define-global "'f" my-find-keymap))

;; G — maGit
(let ((my-git-keymap (make-sparse-keymap)))
  (define-key my-git-keymap "g" 'my-magit-status) ; default
  (define-key my-git-keymap "B" 'git-link) ; browse
  (define-key my-git-keymap "a" 'vc-annotate)
  (define-key my-git-keymap "b" 'magit-blame)
  (define-key my-git-keymap "h" 'github-browse-file)
  (define-key my-git-keymap "i" 'git-messenger:popup-message)
  (define-key my-git-keymap "l" 'magit-log-buffer-file)
  (define-key my-git-keymap "m" 'diff-hl-mark-hunk)
  (define-key my-git-keymap "n" 'diff-hl-next-hunk)
  (define-key my-git-keymap "p" 'diff-hl-previous-hunk)
  (define-key my-git-keymap "r" 'diff-hl-revert-hunk)
  (define-key my-git-keymap "t" 'git-timemachine-toggle)
  (define-key my-git-keymap "u" 'github-browse-file)
  (key-chord-define-global "'g" my-git-keymap)
  (key-chord-define-global "qg" my-git-keymap))

;; H — Help system
;; maybe
;; - cheat-sh here
;; - clojure docs bindings
;; - emacs help system (C-h is pretty easy though)

;; I — Imenu
(let ((my-imenu-keymap (make-sparse-keymap)))
  (define-key my-imenu-keymap "i" 'counsel-semantic-or-imenu) ; default
  (define-key my-imenu-keymap "I" 'ivy-imenu-anywhere) ; across all project buffers
  (define-key my-imenu-keymap "s" 'imenu-list-smart-toggle) ; sidebar
  (define-key my-imenu-keymap "p" 'popup-imenu) ; popup
  (define-key my-imenu-keymap "O" 'outshine-imenu) ; outline
  (define-key my-imenu-keymap "o" 'outshine-navi) ; outline
  (key-chord-define-global "qi" my-imenu-keymap))

;; J
(key-chord-define-global "'j" 'jump-to-register)
(key-chord-define-global "qj" 'jump-to-register)


;; K — Kill (and minimize)
(let ((my-kill-keymap (make-sparse-keymap)))
  (define-key my-kill-keymap "z" 'delete-window-balancedly) ; like backgrounding
  (define-key my-kill-keymap "Z" 'kill-window-balancedly)
  (define-key my-kill-keymap "k" 'delete-window-balancedly)
  (define-key my-kill-keymap "K" 'kill-current-buffer)
  ;; (define-key my-kill-keymap "x" 'kill-current-buffer)
  ;; (define-key my-kill-keymap "X" 'kill-current-buffer)
  (define-key my-kill-keymap "p" 'projectile-kill-buffers)
  (key-chord-define-global "'k" my-kill-keymap)
  (key-chord-define-global "qk" my-kill-keymap))

;; L — Line-numbering/viewing
(let ((my-lines-keymap (make-sparse-keymap)))
  (define-key my-lines-keymap "r" 'nlinum-relative-toggle)
  (define-key my-lines-keymap "l" 'nlinum-mode)
  (define-key my-lines-keymap "t" 'toggle-truncate-lines)
  (key-chord-define-global "'l" my-lines-keymap)
  (key-chord-define-global "ql" my-lines-keymap))

;; M — Mark
(key-chord-define-global "'m" 'point-to-register)
(key-chord-define-global "qm" 'point-to-register)
(key-chord-define-global "QM" 'counsel-bookmark)

;; N — New windows
(key-chord-define-global "'n" 'split-window-balancedly)
(key-chord-define-global "qn" 'split-window-balancedly)
(key-chord-define-global "\"N" 'split-window-vertically-balancedly)
(key-chord-define-global "QN" 'split-window-vertically-balancedly)

;; O — Open,Other
(key-chord-define-global "qo" 'crux-smart-open-line)
;; (key-chord-define-global "QO" 'my-clj-open-above-let)

;; P — Projectile
(let ((my-projectile-keymap (make-sparse-keymap)))
  (define-key my-projectile-keymap "a" 'projectile-add-known-project)
  (define-key my-projectile-keymap "b" 'counsel-projectile-switch-to-buffer)
  (define-key my-projectile-keymap "d" 'projectile-find-dir)
  (define-key my-projectile-keymap "e" 'projectile-recentf)
  (define-key my-projectile-keymap "f" 'projectile-find-file)
  (define-key my-projectile-keymap "g" 'projectile-grep)
  (define-key my-projectile-keymap "i" 'projectile-invalidate-cache)
  (define-key my-projectile-keymap "o" 'projectile-multi-occur)
  (define-key my-projectile-keymap "p" 'projectile-switch-project)
  (define-key my-projectile-keymap "s" 'projectile-ag)
  (define-key my-projectile-keymap "t" 'projectile-toggle-between-implementation-and-test)
  (define-key my-projectile-keymap "v" 'projectile-run-vterm)
  (define-key my-projectile-keymap "D" 'projectile-dired)
  (define-key my-projectile-keymap "E" 'projectile-edit-dir-locals)
  (define-key my-projectile-keymap "I" 'ivy-imenu-anywhere)
  (key-chord-define-global "'p" my-projectile-keymap)
  (key-chord-define-global "qp" my-projectile-keymap))

;; Q

;; R — Refactoring
;; (key-chord-define-global "'r" 'makey-key-mode-popup-clj-refactor)
(let ((my-cljr-keymap (make-sparse-keymap)))
  (define-key my-cljr-keymap "c" 'cljr-clean-ns)
  (define-key my-cljr-keymap "d" 'cljr-destructure-keys)
  (define-key my-cljr-keymap "f" 'cljr-rename-file-or-dir)
  (define-key my-cljr-keymap "h" 'cljr-hotload-dependency)
  (define-key my-cljr-keymap "u" 'cljr-find-usages)
  (define-key my-cljr-keymap "p" 'cljr-add-project-dependency)
  (define-key my-cljr-keymap "r" 'cljr-add-require-to-ns)
  (define-key my-cljr-keymap "s" 'cljr-auto-sort-ns)
  (define-key my-cljr-keymap "x" 'cljr-toggle-debug-mode)
  (define-key my-cljr-keymap "S" 'cljr-sort-project-dependencies)
  (key-chord-define-global "'r" my-cljr-keymap))

;; S — Spaces (perspectives)
(let ((my-spaces-keymap (make-sparse-keymap)))
  (define-key my-spaces-keymap "s" 'persp-switch)
  (define-key my-spaces-keymap "r" 'persp-rename)
  (define-key my-spaces-keymap "S" 'persp-state-save)
  (define-key my-spaces-keymap "L" 'persp-state-load)
  (key-chord-define-global "'s" my-spaces-keymap))
;; (key-chord-define-global "'s" 'persp-switch)

;; T — Typography
;; —  ’ “ ‘ ‘ ’ “
(let ((my-typo-keymap (make-sparse-keymap)))
  (define-key my-typo-keymap "`" "‘")
  (define-key my-typo-keymap "'" "’")
  (define-key my-typo-keymap "~" "“")
  (define-key my-typo-keymap "\"" "”")
  (define-key my-typo-keymap "-"  "—")
  (define-key my-typo-keymap ">"  "→")
  (define-key my-typo-keymap "+"  "±")
  (define-key my-typo-keymap "v"  "✓")
  (define-key my-typo-keymap "x"  "✗")
  (define-key my-typo-keymap "f"  "☞")
  (define-key my-typo-keymap "t"  "†")
  (define-key my-typo-keymap "a"  "★")
  (define-key my-typo-keymap "o"  "□")
  (define-key my-typo-keymap "r"  "⏎")
  (define-key my-typo-keymap "l"  "λ")
  (define-key my-typo-keymap "e"  "∴")
  (define-key my-typo-keymap "m"  "μ")
  (define-key my-typo-keymap "p"  "π")
  (define-key my-typo-keymap "n"  "♫")
  (define-key my-typo-keymap "d"  "♢")
  (define-key my-typo-keymap "h"  "♡")
  (define-key my-typo-keymap "s"  "♤")
  (define-key my-typo-keymap "c"  "♧")
  (define-key my-typo-keymap "b"  "₿")
  (define-key my-typo-keymap "t" 'typo-mode)
  (key-chord-define-global "'t" my-typo-keymap)
  (key-chord-define-global "qt" my-typo-keymap))

;; U
;; (key-chord-define-global ",u" 'undo-tree-visualize)

;; V — Vterm
(let ((my-vterm-keymap (make-sparse-keymap)))
  (define-key my-vterm-keymap "v" 'vterm-toggle)
  (define-key my-vterm-keymap "c" 'vterm-toggle-cd-show)
  (define-key my-vterm-keymap "n" 'vterm)
  (key-chord-define-global "'v" my-vterm-keymap))

;; W — Windowing
(key-chord-define-global "'w" 'ace-window)
(key-chord-define-global "qw" 'ace-window)

;; X

;; Y
;; (key-chord-define-global "qy" 'scroll-down-stay)
(key-chord-define-global "'y" 'prev-window)
(key-chord-define-global "qw" 'other-window)

;; Z — folding/hide-show
;; Hide-Show custom prefix. This trick works for setting any key-chord prefix!
;; https://emacs.stackexchange.com/questions/33684/proper-way-to-change-prefix-key-for-minor-mode-map
(let ((my-hs-keymap (make-sparse-keymap)))
  (define-key my-hs-keymap "c" 'hs-hide-block)
  (define-key my-hs-keymap "C" 'hs-hide-all)
  (define-key my-hs-keymap "o" 'hs-show-block)
  (define-key my-hs-keymap "O" 'hs-show-all)
  (define-key my-hs-keymap "z" 'hs-toggle-hiding)
  (key-chord-define-global "'z" my-hs-keymap))



;;; Other Bindings

(global-set-key (kbd "C-S-b") 'counsel-ibuffer)

(global-set-key (kbd "C-S-c") 'beacon-blink)

(global-set-key (kbd "C-S-e") 'scroll-up-stay)
(global-set-key (kbd "C-S-f") 'counsel-recentf)

(global-set-key (kbd "C-S-g") 'magit-status)

;; ISpell (I)
(global-set-key (kbd "C-S-i") 'flycheck-next-error)

;; Hmm, M-J is needed for sp-join-sexp
;; (global-set-key (kbd "M-J") 'jump-to-register)
(global-set-key (kbd "C-S-j") 'jump-to-register)

;; Kill (K)
(global-set-key (kbd "C-S-k") 'kill-window-balancedly)

;; Register marking/jumping, closer to vim
(global-set-key (kbd "C-S-m") 'point-to-register)

(global-set-key (kbd "C-S-n") 'split-window-balancedly)
;; Window buffer switching (O: Only)
(global-set-key (kbd "C-S-o") 'delete-other-windows) ; think "Only"
;; Just use C-c left-arrow

;; Just use C-c left-arrow
;; (global-set-key (kbd "C-S-g") 'winner-undo)
;; (global-set-key (kbd "C-S-+") 'balance-windows)

(global-set-key (kbd "C-S-v H") 'hs-hide-all)
(global-set-key (kbd "C-S-v S") 'hs-show-all)
(global-set-key (kbd "C-S-v h") 'hs-hide-block)
(global-set-key (kbd "C-S-v s") 'hs-show-block)
(global-set-key (kbd "C-S-v t") 'hs-toggle-hiding)
(global-set-key (kbd "C-S-v v") 'hs-toggle-hiding)
;; (global-set-key (kbd "C-S-x") 'avy-goto-word-1)
(global-set-key (kbd "C-S-y") 'scroll-down-stay)

;; (global-set-key (kbd "C-z")   'delete-window-balancedly)
;; Background window (Z: like shell's C-z)
(global-set-key (kbd "C-S-z") 'delete-window-balancedly)




(global-set-key (kbd "C-p") 'previous-line)

(global-set-key (kbd "C-M-_") 'text-scale-decrease)
(global-set-key (kbd "C-M-+") 'text-scale-increase)

;; Magit: came with Super-based shortcuts; use C-c g ... instead
;; maGit (G)
;; (global-set-key (kbd "C-c C-g B") 'github-browse-file)
(global-set-key (kbd "C-c C-g B") 'git-link)
(global-set-key (kbd "C-c C-g a") 'vc-annotate)
(global-set-key (kbd "C-c C-g b") 'magit-blame)
(global-set-key (kbd "C-c C-g g") 'magit-status)
(global-set-key (kbd "C-c C-g h") 'github-browse-file)
(global-set-key (kbd "C-c C-g i") 'git-messenger:popup-message)
(global-set-key (kbd "C-c C-g l") 'magit-log-buffer-file)
(global-set-key (kbd "C-c C-g m") 'diff-hl-mark-hunk)
(global-set-key (kbd "C-c C-g n") 'diff-hl-next-hunk)
(global-set-key (kbd "C-c C-g p") 'diff-hl-previous-hunk)
(global-set-key (kbd "C-c C-g r") 'diff-hl-revert-hunk)
(global-set-key (kbd "C-c C-g t") 'git-timemachine-toggle)
;; (global-set-key (kbd "C-c C-g p") 'git-messenger:popup-message)

(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "<f7>") 'symbol-overlay-mode)
(global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)

;; mimic popular IDEs binding, note that it doesn't work in a terminal session
(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
(global-set-key [(shift return)] 'crux-smart-open-line)
;; (global-set-key (kbd "M-o") 'crux-smart-open-line)
;; (global-set-key (kbd "M-O") 'crux-smart-open-line-above)
;; (global-set-key [(control shift return)] 'crux-smart-open-line-above)
(global-set-key (kbd "C-c e") 'crux-eval-and-replace)
(global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "C-c t") 'crux-visit-term-buffer)
(global-set-key (kbd "C-c I") 'crux-find-user-init-file)

(global-set-key [(meta shift up)] 'move-text-up)
(global-set-key [(meta shift down)] 'move-text-down)

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-p") 'previous-line)
(global-set-key (kbd "C-c T") 'typo-mode)

;; Simpler attempt at typography.
(global-set-key (kbd "C-c '") "’")
(global-set-key (kbd "C-c `'") "‘")
(global-set-key (kbd "C-c \"") "“")
(global-set-key (kbd "C-c /") "”")
(global-set-key (kbd "C-c -") "—")

;; (global-set-key (kbd "C-'") 'toggle-quotes)
;; TEST: Can't "do" this.
;; (global-set-key (kbd "C-c C") 'hide/show-comments-toggle)

;; Planck-friendly
(global-set-key (kbd "M-{") 'backward-paragraph)
(global-set-key (kbd "M-<") 'forward-paragraph)
(global-set-key (kbd "M->") 'end-of-buffer)
(global-set-key (kbd "M-}") 'beginning-of-buffer)
(global-set-key (kbd "C-S-L") 'nlinum-mode)
(global-set-key (kbd "C-S-M-L") 'nlinum-relative-toggle)
(global-set-key (kbd "C-?") 'undo-tree-redo) ; GUIONLY
(global-set-key (kbd "M-Q") 'unfill-paragraph)
;; (global-set-key (kbd "M-S-q") 'unfill-paragraph)

;; Disable prompt for kill
;; http://superuser.com/questions/354849/emacs-kill-buffer-without-prompt
;; (global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-c k") 'kill-current-buffer)
(global-set-key [(control ?.)] (lambda () (interactive) (dot-mode 1)
                                       (message "Dot mode activated.")))

;; Prelude badly sets C-- to zoom out, so keep as negative argument
(global-set-key (kbd "C--") 'negative-argument)

(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; http://stackoverflow.com/questions/6464738/how-can-i-switch-focus-after-buffer-split-in-emacs
(global-set-key "\C-x2"
                (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive) (split-window-horizontally) (other-window 1)))


(global-set-key (kbd "C-<tab>") 'aw-flip-window) ; GUIONLY
(global-set-key (kbd "M-<tab>") 'ace-window) ; GUIONLY


(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)

;; Fastest window switching: http://emacs.stackexchange.com/a/3471/11025
;; (global-set-key (kbd "C-.") 'other-window)
;; (global-set-key (kbd "C-,") 'prev-window)
;; Switch windows; lower case version is used in too many shells/menus
(global-set-key (kbd "M-P") 'prev-window)
(global-set-key (kbd "M-N") 'other-window)
(defun prev-window () (interactive) (other-window -1))
;; other-window
;; Default
(global-set-key (kbd "C-x o") (lambda () (interactive) (other-window 1)))
;; C-x C-o is common an easer for switching back window
(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window -1)))
(global-set-key "\C-xO"    (lambda () (interactive) (delete-blank-lines)))


(global-set-key (kbd "C-S-<up>")     'buf-move-up)
(global-set-key (kbd "C-S-<left>")   'buf-move-left)
(global-set-key (kbd "C-S-<down>")   'buf-move-down)
(global-set-key (kbd "C-S-<right>")  'buf-move-right)
(global-set-key (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
(global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
(global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle) ;; Cycle through Java styles
(global-set-key (kbd "C-c K") 'string-inflection-kebab-case) ;; Cycle through Java styles

;; smartparens overrides M-r, so changing default
(global-set-key "\M-R" 'move-to-window-line-top-bottom)
(global-set-key "\M-\C-R" 'jump-to-bottom)
;; Since already holding M-S-R, enable recenter (usually C-l) to also be M-S
(global-set-key (kbd "M-L") 'recenter-top-bottom)
(global-set-key (kbd "M-;") 'comment-dwim-2)

(global-set-key [(meta m)] 'jump-char-forward)
;; (global-set-key [(shift meta m)] 'jump-char-backward)
;; (global-set-key (kbd "C-=") 'er/expand-region)
;; (global-set-key (kbd "C-'") 'toggle-quotes)
;; TEST: Can't "do" this.
;; (global-set-key (kbd "C-c T") 'typo-mode)
;; ISSUE: Need to auto-enter typo-mode only while inside strings.
;; (global-set-key [remap mark-sexp] 'easy-mark) ; TRIAL
(global-set-key [remap kill-ring-save] 'easy-kill)
;; (global-set-key [remap kill-ring-save] 'easy-mark)
;; (global-set-key (kbd "M-%") 'anzu-query-replace)
;; (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(global-set-key (kbd "C-c '") 'imenu-list-smart-toggle)
(global-set-key (kbd "C-'") 'counsel-semantic-or-imenu)
(global-set-key (kbd "C-\"") 'popup-imenu)
;; (global-set-key (kbd "C-M-a") 'beginning-of-defun) ; replaced
(global-set-key (kbd "C-M-a") 'my-beginning-of-defun)
(global-set-key (kbd "C-c c") 'my-copy-filename)
(global-set-key (kbd "C-x ]") 'my-forward-jump-to-line-break)

(global-set-key (kbd "C-x [") 'my-backward-jump-to-line-break)
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)


;;; UI

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen nil)

;; Stop cl deprecated warnings
;; https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))

;; ;; nice scrolling
;; ENABLE
;; (setq scroll-margin 0
;;       scroll-conservatively 100000
;;       scroll-preserve-screen-position 1)

;; mode line settings
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " MDE - " (:eval (if (buffer-file-name)
                                                    (abbreviate-file-name (buffer-file-name))
                                                  "%b"))))
(require 'ample-theme)
(load-theme 'ample t t)
;; (load-theme 'ample-flat t t)
;; (load-theme 'ample-light t t)
(enable-theme 'ample)

;; https://seagle0128.github.io/doom-modeline/
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-height 10)

;; Put total lines into modeline.
;; https://github.com/hinrik/total-lines
(require 'total-lines)
(global-total-lines-mode)
(setq global-mode-string
      `(((12 "%l" "/" (:eval (format "%d,%d" total-lines (1+ (current-column)))))
	 (-3 "%p"))))

;; (set-face-attribute 'mode-line nil :family "Alegreya Sans" :height 75)
;; (set-face-attribute 'mode-line-inactive nil :family "Alegreya Sans" :height 75)

;; (setq flycheck-set-indication-mode 'left-margin)
;; (flycheck-set-indication-mode 'left-fringe)

;; show available keybindings after you start typing
;; Need to decide between discover-my-major, guide-key, plain-old describe-bindings (C-h b)
;; https://emacs.stackexchange.com/questions/
(require 'which-key)
(which-key-mode +1)

;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(scroll-bar-mode -1)
(setq scroll-bar-width 2)

;; Stop Emacs from processing .Xresources/.Xdefaults
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Resources.html#Resources
(setq inhibit-x-resources t)

;; No splash screen
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen nil)
(setq inhibit-splash-screen nil)

;; Newline at end of file
(setq require-final-newline t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))

;; show the cursor when moving after big movements in the window
(require 'beacon)
(beacon-mode +1)

;; Highlight word matching point without doing anything
;; https://github.com/nonsequitur/idle-highlight-mode/blob/master/idle-highlight-mode.el
;; Disabling since might play badly with org-mode
;; Also screws with visible-mark.
;; ENABLE
;; (prelude-require-package 'idle-highlight-mode)
;; (add-hook 'prog-mode-hook 'idle-highlight-mode)

;; ;; Highlight at point
;; (require 'highlight-thing)
;; (global-highlight-thing-mode)
;; (setq highlight-thing-delay-seconds 0.9)
;; (setq highlight-thing-exclude-thing-under-point t)

;; Highlight symbols with keymap-enabled overlays (search, find)
;; https://github.com/wolray/symbol-overlay/
(require 'symbol-overlay)

;; ENABLE??
;; https://github.com/magnars/expand-region.el
;; (require 'expand-region)

;;; Perpectives/Sessions
;; https://github.com/Bad-ptr/persp-mode.el
(setq wg-morph-on nil)


;; https://github.com/cyrus-and/zoom
(require 'zoom)
;; (zoom-mode)
(global-set-key (kbd "C-x +") 'zoom)


;;; TUNING

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Follow symlinks.
(setq find-file-visit-truename t)

;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

;; https://www.reddit.com/r/emacs/comments/7wezb4/how_can_i_make_line_rendering_faster/du1mige/
(setq-default bidi-display-reordering nil)

;; https://github.com/hlissner/doom-emacs/issues/2217#issuecomment-568037014
;; https://www.facebook.com/notes/daniel-colascione/buttery-smooth-emacs/10155313440066102/
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq savehist-autosave-interval 300)

;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 1MB
(setq large-file-warning-threshold 1000000)

;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag
(setq auto-window-vscroll nil)

;; config changes made through the customize UI will be store here
;; (setq custom-file (expand-file-name "custom.el" prelude-personal-dir))


(require 'crux)

;; move line up/down (already enabled) -- M-S-up
;; move-text-up, move-text-down
(require 'move-text)




;;; LOOK-N-FEEL

;; Displaying of tab widith; like vim's tabstop
(setq tab-width 20)

;; This was a widespread practice in the days of typewriters. I actually prefer
;; it when writing prose with monospace fonts, but it is obsolete otherwise.
(setq sentence-end-double-space nil)

;; Default to soft line-wrapping in text modes. It is more sensibile for text
;; modes, even if hard wrapping is more performant.
(add-hook 'text-mode-hook #'visual-line-mode)

;; Use monospaced font faces in current buffer
(defun my-buffer-face-mode-fixed ()
  "Set a fixed width (monospace) font in current buffer."
  (interactive)
  (typo-mode 0)
  ;; (setq buffer-face-mode-face '(:family "Fantasque Sans Mono" :height 180))
  (setq buffer-face-mode-face '(:family "Fira Code" :height 50))
  ;; (setq buffer-face-mode-face '(:family "Fantasque Sans Mono" :height 60))
  (buffer-face-mode))

(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer."
  (interactive)
  ;; (setq buffer-face-mode-face '(:family "Alegreya Sans" :height 180 :width semi-condensed))
  (setq buffer-face-mode-face '(:family "Alegreya Sans" :height 60 :width semi-condensed))
  (buffer-face-mode))
;; Font face overrides via hooks
;; Enable these if you wanna go with variable-width fonts as default.
;; (add-hook 'prog-mode-hook 'my-buffer-face-mode-fixed)
;; (add-hook 'dired-mode-hook 'my-buffer-face-mode-fixed)
;; (add-hook 'magit-mode-hook 'my-buffer-face-mode-fixed)
;; (add-hook 'eshell-mode-hook 'my-buffer-face-mode-fixed)
;; (add-hook 'yaml-mode-hook 'my-buffer-face-mode-fixed)
;; (add-hook 'markdown-mode-hook 'my-buffer-face-mode-variable)

;; Focus
(require 'focus)

;; Mixed pitch fonts (variable width)
;; https://gitlab.com/jabranham/mixed-pitch
(require 'mixed-pitch)
;; FIXME: seems to need to be run manually
(add-hook 'text-mode 'mixed-pitch-mode)


;;; IVY, COUNSEL, SWIPER

(require 'ivy)
(ivy-mode 1)
(counsel-mode)
(setq ivy-height 20)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq projectile-completion-system 'ivy)

;; (require 'ivy-posframe)
;; display at `ivy-posframe-style'
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
;; (ivy-posframe-mode 1)

;; Enable counsel replacements for projectile.
;; https://github.com/ericdanan/counsel-projectile
(require 'counsel-projectile)
(counsel-projectile-mode)

;; Projectile
;; Use C-u to alter grepping behavior.
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-S-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
;; https://github.com/nlamirault/ripgrep.el
(require 'ripgrep)

;;; imenu
;; (require 'imenu-anywhere)
(require 'imenu-list)
(require 'popup-imenu)

;;; Outshine (like "outline")
;; Like imenu, but for pages/sections nav/highlighting.
;; Has `outshine-imenu' as my main use
(require 'outshine)
;; Had to install navi-mode for parts to work.
;; (outshine-mode t)
(add-hook 'prog-mode-hook 'outshine-mode)


(require 'ido)
(require 'ido-completing-read+)
(require 'flx-ido)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ;; ido-save-directory-list-file (expand-file-name "ido.hist" prelude-savefile-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)
(ido-mode +1)
(ido-ubiquitous-mode +1)

;;; smarter fuzzy matching for ido
(flx-ido-mode +1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)



;;; LINTERS

(require 'flycheck-yamllint)


;; Typopunct: fancy/pretty quotes, etc: — ‘’ “”
;; enable: M-x typopunct-mode
;; https://www.emacswiki.org/emacs/TypographicalPunctuationMarks
;; (prelude-require-package 'typopunct)
;; (typopunct-change-language 'english t)
;; (typopunct-mode 1)
;; https://github.com/jorgenschaefer/typoel
(setq-default typo-language "English")
;; ISSUE: Need to auto-enter typo-mode only while inside strings.
;; M-x typo-mode

;; Edit in new buffer
;; Better than poporg-edit?  Great for markdown.  Can even eval code.
;; Keys: C-c ' (start), C-c C-c (commit)
(require 'typo) ; C-c T
;; https://github.com/emacsmirror/cycle-quotes/blob/master/cycle-quotes.el
;; (require 'cycle-quotes)
;; https://github.com/toctan/toggle-quotes.el
;; (require 'toggle-quotes)


;; Typo fancy typography/punctuation
;; https://github.com/jorgenschaefer/typoel

;; Defvault to using typo mode for better/fancy typography
(typo-global-mode 1)
(add-hook 'text-mode-hook 'typo-mode)








;; special treatment of FIXME, etc
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)

;; Wow, hide comments!! Just blanks them out.
;; (require 'hide-comnt) ; in vendor/ since not in melpa



;; auto-dim
;; https://github.com/mina86/auto-dim-other-buffers.el
(add-hook 'after-init-hook
          (lambda ()
            (when (fboundp 'auto-dim-other-buffers-mode)
              (auto-dim-other-buffers-mode t))))

;; BUGGY
;; Line numbers
;; https://github.com/xcodebuild/nlinum-relative
;; Supposedly faster than linum
(require 'nlinum-relative) ; SLOW (for high LOC)
(global-nlinum-relative-mode 1)
;; (global-display-line-numbers-mode)
(setq nlinum-relative-redisplay-delay 1)
(setq nlinum-relative-offset 0)

;; (setq nlinum-relative-current-symbol "->")      ; or "" for display current line number

;; Visible mark
;; http://pragmaticemacs.com/emacs/regions-marks-and-visual-mark/
(setq visible-mark-max 2)
(setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))
(defface visible-mark-active ;; put this before (require 'visible-mark)
  '((((type tty) (class mono)))
    (t (:background "magenta"))) "")
(require 'visible-mark)
(global-visible-mark-mode)


;;; BEHAVIOR

;; Undo-tree: C-/ undo, M-_ redo
;; http://pragmaticemacs.com/emacs/advanced-undoredo-with-undo-tree/
;; FIXME: this is really needed package, but have to figure out now to clean its mess!
(global-undo-tree-mode 1)
;; Config from prelude:
;; autosave the undo-tree history
;; (setq undo-tree-history-directory-alist
;;       `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; Auto-save
;; (add-hook 'focus-out-hook 'save-buffer)
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;; FIXME: C-d C-d d should pop up docs for various modes.

;; Extra file extensions to support
(push '("/LICENSE\\'" . text-mode) auto-mode-alist)
(push '("\\.log\\'" . text-mode) auto-mode-alist)
(push '("rc\\'" . conf-mode) auto-mode-alist)

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
;; easier to peruse (with `counsel-yank-pop' or `helm-show-kill-ring'.
(setq kill-do-not-save-duplicates t)

;; Allow UTF or composed text from the clipboard, even in the terminal or on
;; non-X systems (like Windows or macOS), where only `STRING' is used.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Make Emacs use the $PATH set up by the user's shell
(require 'exec-path-from-shell)

;; Whitespace removal DWIM key for emacs.
;; maybe use in future, not bound to anything
;; https://github.com/jcpetkovich/shrink-whitespace.el
(require 'shrink-whitespace)

;; whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 100) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; Automatically remove all trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Inverse of Emacs' fill-paragraph and fill-region
;; https://github.com/purcell/unfill
;; http://stackoverflow.com/questions/6707758/inverse-of-m-q-an-unfill-paragraph-function
(require 'unfill)

;; Spelling/grammar help
;; https://github.com/mhayashi1120/Emacs-langtool
;; ENABLE
;; (prelude-require-package 'langtool)

(setq tab-stop-list (number-sequence 2 200 2))

;; Zsh, hopefully
(setq indent-tabs-mode t)
(setq tab-width 2)


;; Don't want to suspend emacs!
;; http://superuser.com/questions/349943/how-to-awake-emacs-gui-after-pressing-ctrlz#349997
(global-unset-key (kbd "C-z"))

(require 'dot-mode)
;; https://www.emacswiki.org/emacs/dot-mode.el
;; C-.  C-M-.  C-c.
(autoload 'dot-mode "dot-mode" nil t)
;; (dot-mode t)
(global-dot-mode t)
;; FIXME: prbly not working since flyspell rebinds

;; Make number colorful.
;; ENABLE??
;; (require 'highlight-numbers)
;; (add-hook 'prog-mode-hook 'highlight-numbers-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; TODO: hydra for visible things
;; - toggle-truncate-lines
;; - crosshairs
;; - hl-line
;; - nlinum-relative-mode
;; - font sizing?
;; - visible whitespace

;; Line wrap is called "truncate" in emacs
(setq-default truncate-lines t)

;; Line breaks (C-l, ^L) are shown as pretty horizontal lines
;; Navigate: C-x ] and C-x [
;; https://stackoverflow.com/a/7577628/326516
(require 'page-break-lines)
(global-page-break-lines-mode)

;; Highlight across multiple buffers
;; https://stackoverflow.com/questions/15415504/highlighting-multiple-buffers-in-emacs
;; https://www.emacswiki.org/emacs/HighlightLibrary
;; ENABLE??
;; (require 'highlight)


;; https://github.com/ryuslash/mode-icons
;(require 'mode-icons)
;(mode-icons-mode)

;; https://github.com/domtronn/all-the-icons.el
(require 'all-the-icons)

(require 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; http://emacs.stackexchange.com/questions/13662/a-confirmation-after-c-x-c-c
;; (setq confirm-kill-emacs 'yes-or-no-p)
(global-unset-key (kbd "C-x C-c"))

;; TRIAL
(add-hook 'neotree-mode-hook
	  (lambda ()
	    ;; already s/S
	    (define-key neotree-mode-map (kbd "N") 'neotree-select-next-sibling-node)))


;; (require 'tldr)
;; (require 'cheat-sh)

;; https://github.com/wbolster/emacs-direnv
(require 'direnv)
(direnv-mode)


;;; HELP SYSTEM



;;; WINDOWING

;; Jump to help window when opened
;; http://stackoverflow.com/questions/36506141/emacs-dispatch-help-window-from-original-buffer
(setq help-window-select t)

;; https://github.com/Wilfred/helpful
(require 'helpful)
;; (global-set-key (kbd "C-h k") #'helpful-key)
(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
;; (global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Winner mode: C-<left> C-<right>
(winner-mode)

;; Ace Window
;; https://github.com/abo-abo/ace-window/wiki
;; https://github.com/abo-abo/ace-window
(require 'ace-window)
(ace-window-display-mode t)

(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
    (?m aw-swap-window "Swap Window")
    (?M aw-move-window "Move Window")
    (?j aw-switch-buffer-in-window "Select Buffer")
    (?n aw-flip-window)
    (?\t aw-flip-window)
    (?c aw-split-window-fair "Split Fair Window")
    (?v aw-split-window-vert "Split Vert Window")
    (?b aw-split-window-horz "Split Horz Window")
    (?i delete-other-windows "Delete Other Windows")
    (?o delete-other-windows)
    (?? aw-show-dispatch-help))
  "List of actions for `aw-dispatch-default'.")


;; Nice size for the default window to match screen height
;; https://stackoverflow.com/questions/17362999/setting-both-fullheight-and-width-in-emacs-on-os-x
(defun get-default-height ()
       (/ (- (display-pixel-height) 120)
          (frame-char-height)))
(add-to-list 'default-frame-alist '(width . 440))
(add-to-list 'default-frame-alist (cons 'height (get-default-height)))


(defun joe-scroll-other-window()
  (interactive)
  (scroll-other-window 1))
(defun joe-scroll-other-window-down ()
  (interactive)
  (scroll-other-window-down 1))

;; (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
;;       '((?x aw-delete-window "Ace - Delete Window")
;; 	(?c aw-swap-window "Ace - Swap Window")
;; 	(?n aw-flip-window)
;; 	(?v aw-split-window-vert "Ace - Split Vert Window")
;; 	(?h aw-split-window-horz "Ace - Split Horz Window")
;; 	(?m delete-other-windows "Ace - Maximize Window")
;; 	(?g delete-other-windows)
;; 	(?b balance-windows)
;; 	(?u (lambda ()
;; 	      (progn
;; 		(winner-undo)
;; 		(setq this-command 'winner-undo))))
;; 	(?r winner-redo)))

;; Window Manager (C-c C-w)

;; Eyebrowse is just a fancier winner-mode, so not bothering yet.
;; (require 'eyebrowse)
;; (eyebrowse-mode t)

;; New mode in v27! (C-x t)
;; Don't know how to save on restart!
;; (tab-bar-mode)

;; (with-eval-after-load "persp-mode-autoloads"
;;   (setq wg-morph-on nil) ;; switch off animation
;;   (setq persp-autokill-buffer-on-remove 'kill-weak)
;;   (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))

;; This is actually perspective, not persp.
(persp-mode)
;; Maybe this causes problems??
;; (add-hook 'kill-emacs-hook #'persp-state-save)
(setq persp-state-default-file "~/.emacs.d/persp-mde")


;; HYDRAS
;; Consider also: Discover menus:
;; https://github.com/mickeynp/discover.el

(require 'hydra)

;; Tiny example
(defhydra hydra-zoom (:color blue)
  "
Press _g_ to zoom.
"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))
(global-set-key (kbd "<f5>") 'hydra-zoom/body)

;;** Example 7: toggle with Ruby-style docstring
(defvar whitespace-mode nil)
(defhydra hydra-toggle (:color pink)
  "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
_t_ truncate-lines:    %`truncate-lines
_w_ whitespace-mode:   %`whitespace-mode
"
  ("a" abbrev-mode nil)
  ("d" toggle-debug-on-error nil)
  ("f" auto-fill-mode nil)
  ("t" toggle-truncate-lines nil)
  ("w" whitespace-mode nil)
  ("q" nil "quit"))
;; Recommended binding:
(global-set-key (kbd "C-c C-v") 'hydra-toggle/body)




;; Colemak
;; (setq aw-keys '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?o))
(setq aw-keys '(?q ?w ?f ?p ?b ?j ?l ?u ?y))
(setq aw-dispatch-always t)
(setq aw-scope 'frame) ; or 'global

;; (ace-window-display-mode t)


(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Shuffle/swap windows around
(require 'buffer-move)
;; https://github.com/bbatsov/prelude/issues/106





(defun delete-window-balancedly ()
  (interactive)
  ;; (save-buffer)
  (delete-window)
  (balance-windows))
(defun kill-window-balancedly ()
  (interactive)
  ;; (save-buffer)
  (kill-current-buffer)
  (delete-window)
  (balance-windows))


;; Should change focus to new window.
(defun split-window-balancedly ()
  (interactive)
  (split-window-horizontally)
  (balance-windows)
  (other-window 1))
(defun split-window-vertically-balancedly ()
  (interactive)
  (split-window-vertically)
  (balance-windows)
  (other-window 1))

;; Scroll without moving point; like Vim's C-y, C-e
;; http://stackoverflow.com/a/10541426/326516
(defun scroll-up-stay (arg)
  (interactive "p")
  (forward-line (* -1 arg))
  (scroll-up arg))
(defun scroll-down-stay (arg)
  (interactive "p")
  (scroll-down arg)
  (forward-line arg))







;; camel, kebab cases
;; https://stackoverflow.com/a/27422814/326516
(require 'string-inflection)


(defun jump-to-bottom ()
  (interactive)
  (move-to-window-line-top-bottom)
  (move-to-window-line-top-bottom))


;;; Commenter: still stuggling with this
(require 'comment-dwim-2)




;;; GIT

;; https://github.com/emacsorphanage/git-messenger
(require 'popup)
(require 'git-messenger)
(require 'magit)
(require 'git-timemachine)
(require 'diff-hl)
(global-diff-hl-mode)

(setq git-identity-default-username "Micah Elliott")
(require 'git-identity)
;; (git-identity-magit-mode 1)
(define-key magit-status-mode-map (kbd "I") 'git-identity-info)

(defun my-magit-status ()
  (interactive)
  (magit-status)
  (balance-windows))


;;; RESTCLIENT
(require 'restclient)

(require 'httprepl)


;; Select/highlight with easy-kill
;; https://github.com/leoliu/easy-kill
;; http://stackoverflow.com/a/36631886/326516
(require 'easy-kill)


;;; Hide-Show (V: visible), like folding
(defun my-hs-hide-block ()
  (interactive)
  (my-beginning-of-defun)
  (hs-hide-block))
(add-hook 'prog-mode-hook 'hs-minor-mode)


;;; SEARCH/JUMP/MOVEMENT

(require 'jump-char)
;; But what about `back-to-indentation' (bound to M-m by default)?
;; You should customize C-a to toggle between indentation and
;; beginning of line like a civilized human being.


;;; Key Chords

;; Decide on using key-chords
;; Key-chords slow down typing of some natural characters; oh well.
;; It's a built-in feature of emacs, so shoud be pretty first-class.
;; Best to just use with a leader -like key for now.  Note that the
;; delay is really short, so combos on the same hand might be too slow
;; to type.



;; avy allows us to effectively navigate to visible things
(require 'avy)
(setq avy-background t)
(setq avy-style 'at-full)

;; anzu-mode enhances isearch & query-replace by showing total matches and current match position
;; If we ever want to abandon swiper, anzu will be useful
;; (require 'anzu)
;; (global-anzu-mode)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
;; https://www.emacswiki.org/emacs/MidnightMode
(require 'midnight)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))


;; AVY: https://github.com/abo-abo/avy/wiki/defcustom
;; confine avy's characters for ease of typing/finding
;; (setq avy-keys (number-sequence ?a ?f))
;; Use only easiest left and right keys
(setq avy-keys (string-to-list "asdfwerkluioghqtypvcxz,.'j"))
;; (setq avy-keys (string-to-list "asdfwerjklasdfwerjklasdfwerjkl"))
;; only search in current window
(setq avy-all-windows nil)
;; make case-sensitive
(setq avy-case-fold-search nil)


;;; PARENS

`';; Rainbow all the things

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(rainbow-delimiters-mode +1)

;; https://github.com/Fanael/rainbow-identifiers
(require 'rainbow-identifiers)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

;; https://www.emacswiki.org/emacs/ShowParenMode
;; (setq show-paren-delay 0)
;; (show-paren-mode 0)
;; Highlight matching parens
;; https://github.com/Fuco1/smartparens/wiki/Show-smartparens-mode
;; Can be SLOW with long lines!
;; (show-smartparens-mode t)
;; More matching parens: colors block you're in red (SLOW?)
(require 'highlight-parentheses)
(highlight-parentheses-mode t)

;; Smartparens (some of these are from prelude)

;; From smartparens.el
;; '(("C-M-f" . sp-backward-sexp)
;;   ("C-M-b" . sp-backward-sexp)
;;   ("C-M-u" . sp-backward-up-sexp)
;;   ("C-M-d" . sp-down-sexp)
;;   ("C-M-p" . sp-backward-down-sexp)
;;   ("C-M-n" . sp-up-sexp)
;;   ("M-s"   . sp-splice-sexp) ;; depth-changing commands
;;   ("M-r"   . sp-splice-sexp-killing-around)
;;   ("M-("   . sp-wrap-round)
;;   ("C-)"   . sp-forward-slurp-sexp)
;;   ("C-}"   . sp-forward-barf-sexp)
;;   ("C-("   . sp-backward-slurp-sexp)
;;   ("C-{"   . sp-backward-barf-sexp)
;;   ("M-S"   . sp-split-sexp) ;; misc
;;   ("M-j"   . sp-join-sexp)
;;   ("M-?"   . sp-convolute-sexp)
;;   ("M-<up>" . sp-splice-sexp-killing-backward)
;;   ("M-<down>" . sp-splice-sexp-killing-forward)
;;   ("C-M-<left>" . sp-backward-slurp-sexp)


;; TODO: why are these 3 set?
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(require 'smartparens)  ; better paredit, sp-*
(require 'smartparens-config)
(sp-use-paredit-bindings)
;; Enable all the goodies.
(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(smartparens-global-mode t)

;; https://emacs.stackexchange.com/questions/7832/how-to-bind-c-for-real
;; (define-key input-decode-map (kbd "C-[") [control-bracketleft])
;; (define-key smartparens-mode-map (kbd "C-]") 'sp-forward-barf-sexp)
;; (define-key smartparens-mode-map [control-bracketleft] 'sp-backward-barf-sexp)

;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
(define-key smartparens-mode-map (kbd "C-}") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-)") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-{") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-(") 'sp-backward-barf-sexp)

;; Remove unneeded bindings: https://emacs.stackexchange.com/a/54651/11025
(define-key smartparens-mode-map (kbd "M-`") nil)
;; (define-key smartparens-mode-map [remap kill-line] 'my-homemade-kill-line)

;; (show-smartparens-global-mode +1)
;; (setq sp-override-key-bindings '(("C-<right>" . nil)))

;; ;; diminish keeps the modeline tidy
;; (require 'diminish)
;; (diminish 'my-keys)
;; (diminish 'hs)

;; saveplace remembers your location in a file when saving files
(defvar root-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar savefile-dir (expand-file-name "savefile" root-dir)
  "This folder stores all the automatically generated save/history-files.")
(setq save-place-file (expand-file-name "saveplace" savefile-dir))
(save-place-mode 1)


;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" savefile-dir))
(savehist-mode +1)

;; save recent files
(require 'recentf)
(recentf-mode t)
(setq
      ;; recentf-save-file (expand-file-name "recentf" savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 30
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      ;; recentf-auto-cleanup 'never
      )

;; Save recent files every 5m
;; https://emacs.stackexchange.com/a/16691/11025
(run-at-time (current-time) 300 'recentf-save-list)
;; (global-set-key "\C-x\ \C-r" 'recentf-open-files)


;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ;; ispell-extra-args '("--sug-mode=ultra")
      )
;; (flyspell-mode +1)

(add-hook 'text-mode-hook 'flyspell-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; Automatically run spell checker.
(add-hook 'flyspell-mode-hook #'flyspell-buffer)

;; enable change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)





;;; COMPLETION
;;
;; M-TAB -- invoke dabbrev menu (also double-TAB), aka M-/
;; TAB -- company-complete manually
;; M-/ -- dabbrev

(require 'company)

(setq company-dabbrev-downcase 0)

;; Disable company auto-completion; manually availalbe now with TAB
;; https://emacs.stackexchange.com/questions/32467/
(setq company-idle-delay nil)

(define-key company-mode-map [remap indent-for-tab-command]
  #'company-indent-or-complete-common)

;; Complete with double-TAB
;; https://emacsredux.com/blog/2016/01/31/use-tab-to-indent-or-complete/
(setq tab-always-indent 'complete)

(add-hook 'prog-mode-hook 'company-mode)

;; (with-eval-after-load 'company-quickhelp (company-quickhelp-terminal-mode 1))

;; Less powerful matching though maybe faster/simpler
;; (require 'company-fuzzy)
;; (global-company-fuzzy-mode 1)

;; Enable fuzzy matching completion, even "index" -> "map-indexed"
(with-eval-after-load 'company
  (company-flx-mode +1))

;; (require 'pos-tip) ; just a dependency package of quickhelp
;; (require 'company-quickhelp)
;; (company-quickhelp-mode 1)

;; TRIAL
(require 'popwin)
(popwin-mode 1)

;; YAY!!!
;; https://github.com/sebastiencs/company-box
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)



;;; LANGUAGES

;;; Ruby

;; (prelude-require-package 'chruby)
(require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)


;;; Markdown

(require 'flymd)
;; (require 'markdown-toc)
;; Enable syntax highlighting of code in blocks.
(setq markdown-fontify-code-blocks-natively t)


;; Gherkin/Cucumber
(require 'feature-mode)
;; Just for emacs testing
;; (prelude-require-package 'ecukes)
(require 'cucumber-goto-step)


(require 'edbi)



;;; Clojure

;; Still need to highleight and press TAB to make work.
(setq clojure-align-forms-automatically t)

;; NOTE: also installed to ~/.lein/profiles.clj: kibit, eastwood
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(require 'cider)
;; (setq cider-repl-history-file "~/.clojure_history")
(require 'cider-eval-sexp-fu)
(require 'clj-refactor)
;; (require 'clojure-snippets) ; yas for clojure
;; (prelude-require-package 'clojure-cheatsheet)
(require 'flycheck-clojure)
;; (require 'company-flx)
;; (require 'flycheck-joker)
(require 'kibit-helper)
;; (require 'sotclojure)
(require 'clojure-mode-extra-font-locking)

(setq error-tip-notify-keep-messages t)

;; For kondo: https://github.com/borkdude/flycheck-clj-kondo#multiple-linters
(require 'flycheck-clj-kondo)
(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
;; (dolist (checkers '((clj-kondo-clj . clojure-joker)
;;                     (clj-kondo-cljs . clojurescript-joker)
;;                     (clj-kondo-cljc . clojure-joker)
;;                     (clj-kondo-edn . edn-joker)))
;;   (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))

(require 'clojure-essential-ref)
(require 'clojure-essential-ref-nov)
(setq clojure-essential-ref-nov-epub-path "~/Downloads/Clojure_The_Essential_Reference_v30_MEAP.epub")

;; Trying to get rid of the prompt to save before load.
(defun my-cider-load-buffer ()
  (save-buffer)
  (cider-load-buffer))

;; https://github.com/clojure-emacs/clj-refactor.el
(defun my-clojure-mode-hook () "Foo bar."
       (message "in my-clojure-mode-hook")
       (clj-refactor-mode 1)
       (yas-minor-mode 1) ; for adding require/use/import statements
       ;; This choice of keybinding leaves cider-macroexpand-1 unbound
       (global-set-key (kbd "M-h") 'mark-paragraph)
       (global-set-key (kbd "C-c C-k") 'my-cider-load-buffer)
       (setq cider-repl-pop-to-buffer-on-connect 'display-only)
       (setq cider-repl-result-prefix ";; => ")
       (setq cider-save-file-on-load t)
       (setq cider-prompt-for-symbol nil)
       ;; (cljr-add-keybindings-with-prefix "C-c r")
       (cljr-add-keybindings-with-prefix "C-S-r")
       ;; (key-chord-define-global "'r" 'cljr-add-keybindings-with-prefix)
       ;; (key-chord-define-global "qr" 'cljr-add-keybindings-with-prefix)
       (key-chord-define-global "qr" 'cljr-ivy)
       (cljr-add-keybindings-with-prefix "C-c m")
       (define-key clojure-mode-map (kbd "C-c C-r") 'cljr-ivy)
       ;; (global-set-key (kbd "C-c R") 'cljr-helm)
       ;; (global-set-key (kbd "C-S-r") 'cljr-helm)
       ;; (global-set-key (kbd "C-c r") 'cljr-helm)
       ;; (global-set-key (kbd "C-S-T") 'cider-test-commands-map)
       ;; Disable flycheck next error in favor of Cider
       (global-set-key (kbd "C-c C-n") 'cider-ns-map)
       (global-set-key (kbd "C-c C-d C-r") 'clojure-essential-ref)
       (global-unset-key (kbd "C-c C-p"))
       (global-set-key (kbd "C-c C-p") 'cider-inspect)
       ;; (define-key (kbd "C-c r"))
       ;; (company-flx-mode +1)
       (global-set-key (kbd "M-J") 'sp-join-sexp) ; maybe already done by smartparens
       ;; Make similar to wrapping with M-(
       (global-set-key (kbd "M-[") (lambda () (interactive) (sp-wrap-with-pair "[")))
       ;; Overrides tmm-menubar
       (global-set-key (kbd "M-`") (lambda () (interactive) (sp-wrap-with-pair "`")))
       )
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)

(eval-after-load "clojure-mode"
  '(progn
     ;; (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)
     (message "MDE: in clojure eval-after-load")))

;; https://github.com/clojure-emacs/squiggly-clojure
                                        ; (eval-after-load 'flycheck '(flycheck-clojure-setup))

(add-hook 'after-init-hook #'global-flycheck-mode)
;; (require 'flycheck-pos-tip)

;; http://stackoverflow.com/questions/23766483/emacs-cider-clojure-auto-complete-how-to-get-the-docstring
;; (setq ac-delay 0.1)
;; (setq ac-quick-help-delay 0.1)

;; Refactor Menu: https://github.com/maio/discover-clj-refactor.el
;; https://github.com/maio/discover-clj-refactor.el
(require 'discover-clj-refactor) ; C-c j

(defun my-cider-find-var (arg)
  (interactive "p")
  (cider-find-var arg)
  (recenter-top-bottom))

;; Minor mode for personal overrides
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-h") 'mark-paragraph)
    map)
  "Custom my-keys-minor-mode keymap.")
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")
(my-keys-minor-mode 1)

(defun my-minibuffer-setup-hook () "Foo bar."
       (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(defun my-clj-open-above-let ()
	"Open a line above while inside a let's top line."
	(interactive)
	(beginning-of-line)
	(sp-down-sexp)
	(sp-down-sexp)
	(newline)
	(company-indent-or-complete-common t)
	(forward-line -1)
	(end-of-line))



;;; ORG MODE

;; https://emacs.stackexchange.com/questions/9709/keep-the-headlines-expanded-in-org-mode
(setq org-startup-folded nil)

;; https://stackoverflow.com/questions/4333467/override-ctrl-tab-in-emacs-org-mode
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control tab)] nil)
             (define-key org-mode-map (kbd "C-M-u") 'org-up-element)
             (define-key org-mode-map (kbd "C-M-d") 'org-down-element)
             (define-key org-mode-map (kbd "C-M-f") 'org-forward-element)
             (define-key org-mode-map (kbd "C-M-b") 'org-backward-element)
             (define-key org-mode-map (kbd "C-C C-x l") 'org-toggle-link-display)
             (define-key org-mode-map (kbd "M-}") 'beginning-of-buffer)))

(require 'org-preview-html)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Insert datestamp, useful outside of org-mode.
(global-set-key (kbd "C-c D") 'org-date-from-calendar)



;;; EXTRA stuff

(defun my-beginning-of-defun ()
  "Jump to start of name of function, since often want to search it."
  (interactive)
  (beginning-of-line)
  (beginning-of-defun)
  (forward-word 2)
  (backward-word))


;; Copy filename to clipboard
;; https://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun my-copy-filename ()
  "Copy current buffer file name to clipboard."
  (interactive)
  (let ((fname (buffer-file-name)))
    (kill-new fname)
    (message "Copied buffer file name '%s' to the clipboard." fname)))

(require 'dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(defun cider-or-dumb-jump ()
  (interactive)
  (if (cider-connected-p)
      (cider-find-var)
    (dumb-jump-go))
  (recenter-top-bottom))

(add-hook 'clojure-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'cider-or-dumb-jump)))

(defun my-forward-jump-to-line-break ()
  (interactive)
  (forward-page)
  (forward-char)
  (recenter-top-bottom 1))
(defun my-backward-jump-to-line-break ()
  (interactive)
  (backward-char 2)
  (backward-page)
  (backward-char)
  (recenter-top-bottom 1))

;; https://github.com/kai2nenobu/guide-key/blob/master/guide-key.el#L578
;; key-chord hack
(defadvice this-command-keys (after key-chord-hack disable)
  "Add key chord to the key sequence returned by `this-command-keys'.
Original `this-command-keys' returns \"[key-chord]\" when you
type any of key chords, so it is difficult to know which key
chord is pressed.  This advice enables to distinguish pressed key
chord."
  (condition-case nil
      (if (equal ad-return-value [key-chord])
          (let ((rkeys (recent-keys)))
            (setq ad-return-value
                  (vector 'key-chord (aref rkeys (- (length rkeys) 2))
                          (aref rkeys (- (length rkeys) 1))))))
    (error "")))

;; Slime-like for shell/zsh (C-u C-x M-m)
;; http://stackoverflow.com/questions/6286579/
(defun sh-send-line-or-region (&optional step)
  (interactive ())
  ;; (let ((proc (get-process "shell"))
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))
        ))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (forward-line))
    ))
(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))
(defun sh-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "vterm")) t))

;; (setq comint-scroll-to-bottom-on-output t)

;; (define-key sh-mode-map [(control ?j)] 'sh-send-line-or-region-mand-step)
;; (define-key sh-mode-map [(control ?j)] 'sh-send-line-or-region)
;; (define-key sh-mode-map [(control ?c) (control ?z)] 'sh-switch-to-process-buffer)
(define-key shell-mode-map [(control ?c) (control ?z)] 'sh-switch-to-process-buffer)
;; (define-key vterm-mode-map [(control ?c) (control ?z)] 'aw-flip-window)
;; (define-key vterm-mode-map [(control ?c) (control ?z)] 'aw-)
(require 'vterm)
(define-key vterm-mode-map (kbd "C-c C-z") (lambda () (interactive) (other-window -1)))
(require 'vterm-toggle)
(global-set-key [f2] 'vterm-toggle)

(defun tws-region-to-process (arg beg end)
  "Send the current region to a process buffer.
The first time it's called, will prompt for the buffer to
send to. Subsequent calls send to the same buffer, unless a
prefix argument is used (C-u), or the buffer no longer has an
active process."
  (interactive "P\nr")
  (if (or arg ;; user asks for selection
          (not (boundp 'tws-process-target)) ;; target not set
          ;; or target is not set to an active process:
          (not (process-live-p (get-buffer-process
                                tws-process-target))))
      (setq tws-process-target
            (completing-read
             "Process: "
             (seq-map (lambda (el) (buffer-name (process-buffer el)))
                      (process-list)))))
  (process-send-region tws-process-target beg end))

;; (define-key sh-mode-map (kbd "C-c C-c") 'tws-region-to-process)
;; (add-hook 'sh-mode (lambda () (local-set-key (kbd "C-c C-c") 'tws-region-to-process)))
;; (define-key sh-mode-map "\C-c\C-c" nil)
;; (add-hook 'shell-script-mode)
(add-hook 'shell-script-mode (lambda ()
			       (local-set-key (kbd "C-c C-c") 'tws-region-to-process)))
(eval-after-load "shell-script"
  #'(define-key (kbd "C-c C-c") 'tws-region-to-process))
;; (define-key shell-mode-map (kbd "C-c C-c") 'tws-region-to-process)

;; Fixing the mark commands in transient mark mode
;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;; Highlight numbers in groups of three, thousands, rather than using commas
;; https://emacs.stackexchange.com/questions/54505/how-to-highlight-digit-groups-of-3-in-numerals
(defun my-matcher (limit)
  (when (re-search-forward
         "\\([0-9]\\{1,3\\}\\)\\(?:[0-9]\\{6\\}\\)*\\(?:[0-9]\\{3\\}\\)\\_>" limit t)
    (goto-char (match-beginning 1))
    (re-search-forward "[0-9]+" (match-end 1))))
(font-lock-add-keywords nil '((my-matcher 0 font-lock-string-face)))

(defun my/company-show-doc-buffer ()
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (or (company-call-backend 'doc-buffer selected)
                         (error "No documentation available"))))
    (with-current-buffer doc-buffer
      (goto-char (point-min)))
    (display-buffer doc-buffer t)))
(define-key company-active-map (kbd "C-<f1>") #'my/company-show-doc-buffer)

;; https://www.reddit.com/r/emacs/comments/6ddr7p/snippet_search_cheatsh_using_ivy/di2eyaz/?utm_source=reddit&utm_medium=web2x&context=3
(defun counsel-tldr ()
  "Search https://github.com/tldr-pages/tldr."
  (interactive)
  (let* ((default-directory "~/src/tldr")
         (cands (split-string
                 (shell-command-to-string
                  "git ls-files --full-name -- pages/")
                 nil t)))
    (ivy-read "Topic: " cands
              :action #'find-file
              :caller 'counsel-tldr)))

(defun my-find-file-below (fname)
  (interactive)
  (split-window-vertically-balancedly)
  (find-file fname))

(defun counsel-refcards ()
  "Search local refcards of your own devising."
  (interactive)
  (let* ((default-directory "~/doc/refcards")
         (cands (split-string (shell-command-to-string "ls") nil t)))
    (ivy-read "Topic: " cands :action #'my-find-file-below)))
(global-set-key (kbd "C-h r") 'counsel-refcards)

(defun org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t)))
(define-key org-mode-map (kbd "C-c e") 'org-toggle-emphasis)


;; Open ivy results in new windows.
;; https://www.reddit.com/r/emacs/comments/efg362/ivy_open_selection_vertically_or_horizontally/
(defun find-file-right (filename)
  (interactive)
  (split-window-right)
  (other-window 1)
  (find-file filename))

(defun find-file-below (filename)
  (interactive)
  (split-window-below)
  (other-window 1)
  (find-file filename))

(ivy-set-actions
 'counsel-projectile-ag
 '(("|" find-file-right "open right")
   ("%" find-file-below "open below")))

(ivy-set-actions
 'counsel-find-file
 '(("|" find-file-right "open right")
   ("%" find-file-below "open below")))

(ivy-set-actions
 'counsel-recentf
 '(("|" find-file-right "open right")
   ("%" find-file-below "open below")))

(ivy-set-actions
 'counsel-buffer-or-recentf
 '(("|" find-file-right "open right")
   ("%" find-file-below "open below")))

(ivy-set-actions
 'ivy-switch-buffer
 '(("|" find-file-right "open right")
   ("%" find-file-below "open below")))


;;; END

(require 'ivy-rich)
(all-the-icons-ivy-rich-mode 1) ; FIXME: needs manual enabling
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
(setq ivy-rich-path-style 'abbrev)

;; Use direnv — supposed to be near bottom in init.el
;; https://github.com/purcell/envrc (seems better than direnv emacs package)
(require 'envrc)
(envrc-global-mode)


(provide 'init)

;;; init ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-mode t)
 '(aw-char-position 'center)
 '(beacon-blink-duration 0.4)
 '(beacon-blink-when-focused t)
 '(beacon-blink-when-point-moves-vertically 5)
 '(beacon-color "red")
 '(beacon-push-mark nil)
 '(browse-url-browser-function 'browse-url-firefox)
 '(browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox")
 '(cider-special-mode-truncate-lines nil)
 '(cljr-favor-private-functions nil)
 '(cljr-hotload-dependencies t)
 '(cljr-magic-require-namespaces
   '(("str" . "clojure.string")
     ("set" . "clojure.set")
     ("pp" . "clojure.pprint")
     ("zip" . "clojure.zip")
     ("edn" . "clojure.edn")
     ("t" . "clojure.test")
     ("as" . "clojure.core.async")
     ("logic" . "clojure.core.logic")
     ("walk" . "clojure.walk")
     ("xml" . "clojure.data.xml")
     ("csv" . "clojure.data.csv")
     ("spec" . "clojure.spec.alpha")
     ("io" . "clojure.java.io")
     ("mat" . "clojure.core.matrix")
     ("json" . "cheshire.core")
     ("time" . "java-time")
     ("spr" . "com.rpl.specter")
     ("http" . "clj-http.client")
     ("log" . "clojure.tools.logging")
     ("e" . "taoensso.encore")
     ("s3" . "amazonica.aws.s3")
     ("sql" . "hugsql.core")
     ("yaml" . "clj-yaml.core")
     ("sh" . "clojure.java.shell")
     ("w" . "clojure.walk")
     ("fs" . "me.raynes.fs")
     ("r" . "reagent.core")
     ("rf" . "re-frame.core")))
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(doom-modeline-buffer-file-name-style 'truncate-with-project)
 '(doom-modeline-continuous-word-count-modes nil)
 '(doom-modeline-display-default-persp-name t)
 '(doom-modeline-github t)
 '(doom-modeline-height 16)
 '(doom-modeline-icon t)
 '(doom-modeline-indent-info nil)
 '(doom-modeline-unicode-fallback t)
 '(doom-modeline-vcs-max-length 40)
 '(ediff-split-window-function 'split-window-horizontally)
 '(fci-rule-color "#383838")
 '(flycheck-pycheckers-checkers '(pylint pep8 pyflakes bandit))
 '(git-identity-list
   '(("mde@micahelliott.com" :name "Personal Projects" :domains
      ("github.com")
      :dirs
      ("~/.emacs.d" "~/proj" "~/dunnit"))
     ("micah.elliott@fundingcircle.com" :name "Work" :domains
      ("github.com")
      :dirs
      ("~/work"))))
 '(global-highlight-parentheses-mode t)
 '(global-hl-line-mode nil)
 '(global-superword-mode t)
 '(global-yascroll-bar-mode t)
 '(hl-paren-colors '("red" "IndianRed1"))
 '(hl-paren-delay 0.3)
 '(hl-paren-highlight-adjacent t)
 '(hs-hide-comments-when-hiding-all nil)
 '(ido-default-file-method 'selected-window)
 '(imenu-list-focus-after-activation t)
 '(imenu-list-position 'left)
 '(imenu-list-size 0.1)
 '(inhibit-startup-screen nil)
 '(ivy-initial-inputs-alist nil)
 '(magit-log-arguments '("--graph" "--color" "--decorate" "--stat" "-n10"))
 '(markdown-header-scaling t)
 '(markdown-wiki-link-search-subdirectories t)
 '(mixed-pitch-fixed-pitch-faces
   '(diff-added diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face line-number line-number-current-line line-number-major-tick line-number-minor-tick markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-block-begin-line org-block-end-line org-document-info-keyword org-code org-indent org-latex-and-related org-checkbox org-formula org-meta-line org-table org-verbatim))
 '(mixed-pitch-set-height t)
 '(neo-autorefresh t)
 '(neo-show-hidden-files t)
 '(neo-show-slash-for-folder nil)
 '(neo-smart-open t)
 '(neo-theme 'icons)
 '(neo-window-position 'left)
 '(neo-window-width 40)
 '(package-selected-packages
   '(mixed-pitch org-bullets org-preview-html clojure-essential-ref-nov cljr-ivy clojure-essential-ref github-browse-file ivy-hydra zoom envrc direnv tldr cheat-sh focus navi-mode rainbow-identifiers treemacs-persp outshine perspective helpful better-jumper switch-window eyebrowse company-box popwin company-posframe treemacs-projectile treemacs all-the-icons-ivy-rich total-lines git-link major-mode-icons popup-imenu imenu-list imenu-anywhere e2wm httprepl restclient ibuffer-vc idle-highlight-in-visible-buffers-mode highlight-thing edbi company-flx company-fuzzy symbol-overlay git-identity mic-paren csv-mode vterm-toggle vterm doom-modeline company-terraform terraform-doc terraform-mode yaml-mode diminish which-key diff-hl git-timemachine delight company-quickhelp-terminal auto-dim-other-buffers key-chord visible-mark flycheck-pos-tip company-quickhelp move-text easy-kill ample-theme beacon unfill string-inflection undo-tree typo toggle-quotes smex smartparens smart-mode-line-powerline-theme shrink-whitespace rubocop ripgrep rainbow-delimiters paren-face page-break-lines neotree mode-icons markdown-mode magit kibit-helper jump-char ido-completing-read+ highlight-parentheses git-messenger flymd flycheck-yamllint flycheck-joker flycheck-clojure flycheck-clj-kondo flx-ido fic-mode feature-mode expand-region exec-path-from-shell edit-indirect dumb-jump dot-mode discover-clj-refactor cycle-quotes cucumber-goto-step crux counsel-projectile company comment-dwim-2 clojure-mode-extra-font-locking cider-eval-sexp-fu buffer-move all-the-icons-dired ag ace-window))
 '(page-break-lines-max-width 80)
 '(popwin:popup-window-height 30)
 '(projectile-enable-caching t)
 '(projectile-file-exists-remote-cache-expire nil)
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "vendor"))
 '(projectile-indexing-method 'hybrid)
 '(projectile-mode t nil (projectile))
 '(projectile-sort-order 'recently-active)
 '(rainbow-identifiers-face-count 15)
 '(safe-local-variable-values
   '((eval with-eval-after-load 'cider
	   (setq cider-default-cljs-repl 'figwheel))
     (cider-lein-global-options . "with-profile +dev,+test")
     (scss-mode
      (css-indent-offset . 2))))
 '(scroll-bar-mode nil)
 '(search-whitespace-regexp "\"[ \\t\\r\\n]+\"")
 '(show-trailing-whitespace t)
 '(split-height-threshold 100)
 '(split-width-threshold 30)
 '(standard-indent 2)
 '(symbol-overlay-faces
   '(symbol-overlay-face-1 symbol-overlay-face-3 symbol-overlay-face-7 symbol-overlay-face-8))
 '(text-scale-mode-step 1.1)
 '(tldr-enabled-categories '("common"))
 '(tramp-default-method "ssh")
 '(treemacs-width 45)
 '(which-key-max-description-length 45))
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
