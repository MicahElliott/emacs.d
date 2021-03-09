;;; personal-init --- Micah

;;; Commentary:
;;
;; Editor Features/Subsystems/Windows:
;; - help system/browser
;; - terminal (vterm)
;; - menu nav (imenu)
;; - git (magit)
;; - buffer browser
;; - tree viewer (treemacs)
;; - popup search (ivy etc)
;; - workspace switcher (perspectives)


;;; Code:


;; https://emacs.stackexchange.com/a/4258/11025
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))


;;; PACKAGING


;;; USE-PACKAGE

;; ;; This is only needed once, near the top of the file
;; (eval-when-compile
;;   ;; Following line is not needed if use-package.el is in ~/.emacs.d
;;   (add-to-list 'load-path "<path where use-package is installed>")
;;   (require 'use-package))

;; ;; Keep packages up-to-date
;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))



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
    ag
    aggressive-indent
    alert
    ample-theme
    avy
    beacon
    buffer-move
    cider
    clojure-mode
    comment-dwim-2
    company
    company-box
    company-flx
    company-fuzzy
    company-jedi
    company-quickhelp
    company-prescient
    company-posframe
    consult
    consult-flycheck
    embark-consult
    crux
    csv
    ctrlf
    dash
    diff-hl
    direnv
    dotenv-mode
    dot-mode
    dumb-jump
    edbi
    easy-kill
    edit-indirect
    embark
    emojify
    envrc
    exec-path-from-shell
    expand-region
    fic-mode
    flycheck-clj-kondo
    flycheck-clojure
    flycheck-inline
    flycheck-pos-tip
    flymd
    focus
    flx
    flx-ido
    git-link
    git-messenger
    git-timemachine
    github-browse-file
    helpful
    highlight-numbers
    highlight-parentheses
    highlight-indentation
    ibuffer-vc
    imenu-list
    jump-char
    key-chord
    key-seq
    kibit-helper
    magit
    marginalia
    markdown-mode
    mood-line
    move-text
    linum-relative
    nlinum-relative
    nov
    orderless
    org-bullets
    org-download
    org-preview-html
    osc
    page-break-lines
    paren-face
    perspective
    popup
    projectile
    prescient
    python
    quick-peek
    rainbow-delimiters
    rg
    ripgrep
    rubocop
    selectrum
    selectrum-prescient
    shackle
    shrink-whitespace
    smartparens
    sotclojure
    super-save
    symbol-overlay
    typo
    undo-tree
    unfill
    unicode-fonts
    use-package
    visible-mark
    vterm
    vterm-toggle
    which-key
    yaml-mode
    ))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; DISABLED
;; rainbow-identifiers
;; all-the-icons-ivy-rich
;; cljr-ivy
;; ivy
;; ivy-hydra
;; ivy-posframe
;; ivy-rich
;; counsel
;; counsel-projectile
;; ivy
;; ivy-rich
;; ivy-posframe
;; ido-flx
;; ido
;; ido-completing-read+

;; Include manually installed packages
(add-to-list 'load-path "~/.emacs.d/vendor")


;; '(cursor-type (quote (bar . 2)))
;; '(blink-cursor-blinks 2)
;; '(blink-cursor-interval 0.2)

;; '(default ((t (:inherit nil :foreground "#F8F8F2" :height 60 :family "Fantasque Sans Mono"))))
;; '(default ((t (:inherit nil :stipple nil :background "gray16" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 60 :width normal :foundry "nil" :family "Fantasque Sans Mono"))))
;; '(org-block ((t (:background "#3E3D31" :foreground "#F8F8F0" :family "Fantasque Sans Mono"))))
;; '(org-code ((t (:foreground "#75715E" :family "Fantasque Sans Mono"))))
;; '(page-break-lines ((t (:slant normal :weight normal :height 180 :width condensed :family "Fantasque Sans Mono"))))

(set-face-attribute 'default nil :font "Fira Code"
                    :height (if (eq system-type 'darwin) 100 62)
		    :background (if (eq system-type 'darwin) "gray16" "gray5"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "red" :height 5.0))))
 '(clojure-keyword-face ((t (:foreground "#ab75c3"))))
 '(col-highlight ((t (:background "gray0"))))
 '(company-box-background ((t (:background "black" :inverse-video nil))) t)
 '(cursor ((t (:background "red" :foreground "#272822"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#75715E"))))
 '(font-lock-comment-face ((t (:foreground "#75715E"))))
 '(font-lock-constant-face ((t (:foreground "#dF9522"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "SlateGray3" :slant italic :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "green3" :underline t :weight ultra-bold))))
 '(font-lock-type-face ((t (:foreground "#66D9EF" :slant italic :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "green3"))))
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
 '(mood-line-status-info ((t (:foreground "purple4"))))
 '(mood-line-status-neutral ((t (:foreground "white"))))
 '(mood-line-unimportant ((t (:foreground "white"))))
 '(org-block ((t (:background "#3E3D31" :foreground "#F8F8F0" :family "Fira Code"))))
 '(org-code ((t (:foreground "#75715E" :family "Fira Code"))))
 '(org-document-title ((t (:inherit (variable-pitch font-lock-constant-face) :weight bold :height 2.0))))
 '(org-level-1 ((t (:inherit (outline-1 variable-pitch) :height 2.0))))
 '(org-level-2 ((t (:inherit (outline-2 variable-pitch) :slant italic :height 1.6))))
 '(org-level-3 ((t (:inherit (outline-3 variable-pitch)))))
 '(page-break-lines ((t (:foreground "#c4bf27" :slant normal :weight normal :height 100 :width condensed :family "Fira Code"))))
 '(quick-peek-background-face ((t (:inherit default :extend t :background "gray22"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "chartreuse" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep sky blue" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :weight bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "chartreuse" :weight bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "deep sky blue" :weight bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "yellow" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "chartreuse" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "deep sky blue" :weight bold))))
 '(region ((t (:inherit highlight :background "slate blue"))))
 '(symbol-overlay-face-3 ((t (:background "NavajoWhite3" :foreground "black"))))
 '(variable-pitch ((t (:height 1.0 :family "Fira Sans"))))
 '(visible-mark-face1 ((t (:background "DarkOrange3"))))
 '(visible-mark-face2 ((t (:background "burlywood4"))))
 '(which-key-command-description-face ((t nil)))
 '(whitespace-tab ((t (:background "purple4" :foreground "#757575")))))

;; '(default ((t (:inherit nil :stipple nil :background (if (eq system-type 'darwin) "gray16" "gray5") :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height (if (eq system-type 'darwin) 100 62) :width normal :foundry "nil" :family "Fira Code"))))
;; '(auto-dim-other-buffers-face ((t (:background "gray29"))))

;; Test rainbow parens by uncommenting:
;; ((((((((()))))))))

;; '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1" :weight bold))))
;; '(font-lock-comment-delimiter-face ((t (:foreground "#75715E"))))
;; '(font-lock-comment-face ((t (:foreground "#75715E" :height 0.9 :family "Delius"))))
;; '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "dodger blue" :height 1.1 :family "Alegreya Sans"))))
;; '(font-lock-function-name-face ((t (:foreground "#A6E22E" :underline t :weight ultra-bold))))
;; '(font-lock-type-face ((t (:foreground "#66D9EF" :slant italic :weight bold))))
;; '(swiper-background-match-face-3 ((t (:inherit swiper-match-face-3 :background "aquamarine3"))))
;; '(swiper-line-face ((t (:background "purple4"))))
;; '(swiper-match-face-2 ((t (:background "yellow" :foreground "black"))))
;; '(swiper-match-face-3 ((t (:background "aquamarine3" :foreground "black"))))

;;; TUNING / PERFORMANCE

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Improve long-line performance.
;; https://emacs.stackexchange.com/a/603/11025
;; https://www.reddit.com/r/emacs/comments/7wezb4/how_can_i_make_line_rendering_faster/du1mige/
(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering nil)

;; Avoid performance issues in files with very long lines.
;; https://emacs.stackexchange.com/a/19030/11025
(global-so-long-mode 1)

;; Follow symlinks.
(setq find-file-visit-truename t)

;; Turn off warnings from vendor packages.
;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

;; https://github.com/hlissner/doom-emacs/issues/2217#issuecomment-568037014
;; https://www.facebook.com/notes/daniel-colascione/buttery-smooth-emacs/10155313440066102/
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq savehist-autosave-interval 300)

;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 1MB
(setq large-file-warning-threshold 1000000)

;; Cursor movement lag
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag
(setq auto-window-vscroll nil)


;; Stop Emacs from processing .Xresources/.Xdefaults
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Resources.html#Resources
(setq inhibit-x-resources t)



;;; BINDINGS
;;
;; The '-leader key-chord is symmetric with q-leader. Note that some
;; combos (s, t, d) are common in the dictionary:
;;
;;  1 i
;;  1 m
;; 10 d (he'd I'd we'd)
;; 24 t
;; 95 s
;;
;; and any lisp quote will cause needed pausing/trouble
;;
;; Crazy that the combos are not ordered. So qe is the same as eq.
;; More details on freqs: https://www.johndcook.com/blog/2015/02/01/rare-bigrams/
;;
;; Ordered key chords: https://github.com/vlevit/key-seq.el

(require 'key-chord)
(key-chord-mode +1)
(setq key-chord-two-keys-delay .1 ; default is .1
      key-chord-one-key-delay  .3) ; default is .2

;; Very special!
(key-chord-define-global "q'" 'crux-smart-open-line-above)

;; Also special
(key-seq-define-global "q-" 'text-scale-decrease)
(key-seq-define-global "q+" 'text-scale-increase)
(key-seq-define-global "qq" 'aw-flip-window)
(key-seq-define-global "''" 'aw-flip-window)

;; A — All workspaces
(let ((my-spaces-keymap (make-sparse-keymap)))
  (define-key my-spaces-keymap "a" 'persp-switch)
  (define-key my-spaces-keymap "r" 'persp-rename)
  (define-key my-spaces-keymap "K" 'persp-kill)
  (define-key my-spaces-keymap "S" 'persp-state-save)
  (define-key my-spaces-keymap "L" 'persp-state-load)
  (key-seq-define-global "'a" my-spaces-keymap))

;; B — Buffer/file
(let ((my-buffer-keymap (make-sparse-keymap)))
  ;; Popups and immediate changes lower case
  ;; (define-key my-buffer-keymap "a" 'counsel-switch-buffer) ; all
  (define-key my-buffer-keymap "a" 'switch-to-buffer) ; all
  (define-key my-buffer-keymap "A" 'my-ibuffer) ; all
  (define-key my-buffer-keymap "A" 'ibuffer) ; all
  (define-key my-buffer-keymap "b" 'crux-switch-to-previous-buffer) ; default
  (define-key my-buffer-keymap "B" 'my-2back-buffers)
  ;; (define-key my-buffer-keymap "e" 'counsel-buffer-or-recentf)
  (define-key my-buffer-keymap "e" 'crux-recentf-find-file)
  ;; (define-key my-buffer-keymap "f" 'counsel-find-file) ; file
  (define-key my-buffer-keymap "f" 'projectile-find-file-dwim) ; file
  ;; (define-key my-buffer-keymap "i" 'counsel-ibuffer) ; ibuffer
  (define-key my-buffer-keymap "i" 'ibuffer) ; ibuffer
  ;; (define-key my-buffer-keymap "r" 'counsel-recentf) ; file
  ;; (define-key my-buffer-keymap "p" 'counsel-projectile-switch-to-buffer) ; project
  (define-key my-buffer-keymap "P" 'projectile-ibuffer)
  (key-seq-define-global "'b" my-buffer-keymap))

;; C — Character goto (fast)
;; (key-seq-define-global "'c" 'avy-goto-char-timer) ; character, delay
(key-seq-define-global "'c" 'avy-goto-char-2-below)
;; (key-seq-define-global "'c" 'avy-goto-word-1-below)
(key-seq-define-global "'f" 'avy-goto-char-2-above)
;; (key-seq-define-global "'f" 'avy-goto-word-1-above)
;; (key-seq-define-global "'c" 'avy-goto-word-0)
;; (key-seq-define-global "'c" 'avy-goto-word-1-above)
(key-seq-define-global "xc" 'avy-goto-word-0)
(key-seq-define-global "qc" 'avy-goto-char-timer)
(key-seq-define-global "\"C" 'beacon-blink) ; cursor

;; ;; D — Treemacs (Directory viewer)
;; (key-seq-define-global "'d" 'treemacs-select-window)
;; ;; (key-seq-define-global "\"D" 'treemacs-visit-node-in-most-recently-used-window)
;; (key-seq-define-global "\"D" 'treemacs-find-file)
;; ;; (key-seq-define-global "'d" 'treemacs)
;; ;; (key-seq-define-global "qd" 'treemacs)

;; E — Errors
(let ((my-flycheck-keymap (make-sparse-keymap)))
  (define-key my-flycheck-keymap "e" 'flycheck-next-error) ; default
  (define-key my-flycheck-keymap "c" 'flycheck-buffer) ; not working?
  (define-key my-flycheck-keymap "E" 'flycheck-explain-error-at-point) ; not useful
  (define-key my-flycheck-keymap "l" 'flycheck-list-errors)
  (define-key my-flycheck-keymap "n" 'flycheck-next-error)
  (define-key my-flycheck-keymap "p" 'flycheck-previous-error)
  (define-key my-flycheck-keymap "C" 'flycheck-compile)
  (key-seq-define-global "qe" my-flycheck-keymap)
  (key-seq-define-global "'e" my-flycheck-keymap))

;; F - Find/search
(let ((my-find-keymap (make-sparse-keymap)))
  ;; (define-key my-find-keymap "f" 'swiper-isearch)
  ;; (define-key my-find-keymap "o" 'ivy-occur)
  ;; (key-seq-define-global "'f" my-find-keymap)
  )

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
  (key-seq-define-global "'g" my-git-keymap))

;; H — Help system
;; maybe
;; - cheat-sh here
;; - clojure docs bindings
;; - emacs help system (C-h is pretty easy though)

;; I — Imenu
(let ((my-imenu-keymap (make-sparse-keymap)))
  (define-key my-imenu-keymap "i" 'consult-imenu) ; default
  ;; (define-key my-imenu-keymap "I" 'ivy-imenu-anywhere) ; across all project buffers
  (define-key my-imenu-keymap "s" 'imenu-list-smart-toggle) ; sidebar
  ;; (define-key my-imenu-keymap "p" 'popup-imenu) ; popup
  ;; (define-key my-imenu-keymap "O" 'outshine-imenu) ; outline
  ;; (define-key my-imenu-keymap "o" 'outshine-navi) ; outline
  (key-seq-define-global "qi" my-imenu-keymap))

;; J
(key-seq-define-global "'j" 'jump-to-register)
(key-seq-define-global "qj" 'jump-to-register)

;; K — Kill (and minimize)
(let ((my-kill-keymap (make-sparse-keymap)))
  (define-key my-kill-keymap "z" 'delete-window-balancedly) ; like backgrounding
  (define-key my-kill-keymap "Z" 'kill-window-balancedly)
  ;; (define-key my-kill-keymap "k" 'delete-window-balancedly) ; confusing
  (define-key my-kill-keymap "K" 'kill-current-buffer)
  ;; (define-key my-kill-keymap "x" 'kill-current-buffer)
  ;; (define-key my-kill-keymap "X" 'kill-current-buffer)
  (define-key my-kill-keymap "p" 'projectile-kill-buffers)
  (key-seq-define-global "qk" my-kill-keymap))

;; L — Line-numbering/viewing
(let ((my-lines-keymap (make-sparse-keymap)))
  (define-key my-lines-keymap "r" 'nlinum-relative-toggle)
  (define-key my-lines-keymap "l" 'nlinum-mode)
  (define-key my-lines-keymap "t" 'toggle-truncate-lines)
  ;; (define-key my-lines-keymap "c" 'crosshairs)
  (define-key my-lines-keymap "h" 'hl-line-flash)
  (define-key my-lines-keymap "b" 'beacon-blink)
  ;; (define-key my-lines-keymap "C" 'crosshairs-mode)
  ;; (key-seq-define-global "ql" my-lines-keymap)
  (key-seq-define-global "'l" my-lines-keymap))

;; M — Mark
(key-seq-define-global "qm" 'point-to-register)
;; (key-seq-define-global "QM" 'counsel-bookmark)

;; N — New windows
(key-seq-define-global "'n" 'split-window-balancedly)
(key-seq-define-global "qn" 'split-window-balancedly)
(key-seq-define-global "\"N" 'split-window-vertically-balancedly)
(key-seq-define-global "QN" 'split-window-vertically-balancedly)

;; O — Open,Other
(key-seq-define-global "qo" 'crux-smart-open-line)
(key-seq-define-global "Q\"" 'my-clj-open-above-let)

;; P — Projectile
;; [s]earch and [g]rep are the search keys for ag, ripgrep, projectile variants
(let ((my-projectile-keymap (make-sparse-keymap)))
  (define-key my-projectile-keymap "a" 'projectile-add-known-project)
  ;; (define-key my-projectile-keymap "b" 'counsel-projectile-switch-to-buffer)
  (define-key my-projectile-keymap "b" 'projectile-switch-to-buffer)
  (define-key my-projectile-keymap "d" 'projectile-find-dir)
  (define-key my-projectile-keymap "e" 'projectile-recentf)
  (define-key my-projectile-keymap "f" 'projectile-find-file)
  (define-key my-projectile-keymap "g" 'projectile-ripgrep)
  (define-key my-projectile-keymap "i" 'projectile-invalidate-cache)
  (define-key my-projectile-keymap "o" 'projectile-multi-occur)
  (define-key my-projectile-keymap "p" 'projectile-switch-project)
  ;; (define-key my-projectile-keymap "s" 'projectile-ag)
  (define-key my-projectile-keymap "s" 'consult-ripgrep)
  ;; (define-key my-projectile-keymap "s" 'consult-ripgrep)
  (define-key my-projectile-keymap "t" 'projectile-toggle-between-implementation-and-test)
  ;; ag in custom specified dir
  ;; (define-key my-projectile-keymap "S" (lambda () (interactive) (let ((current-prefix-arg 4)) (counsel-ag))))
  ;; ag with options prompt
  ;; (define-key my-projectile-keymap "G" (lambda () (interactive) (let ((current-prefix-arg 4)) (counsel-projectile-ag))))
  (define-key my-projectile-keymap "B" 'projectile-ibuffer)
  (define-key my-projectile-keymap "D" 'projectile-dired)
  (define-key my-projectile-keymap "E" 'projectile-edit-dir-locals)
  ;; (define-key my-projectile-keymap "I" 'ivy-imenu-anywhere)
  (key-seq-define-global "'p" my-projectile-keymap))

;; Q — taken by q'

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
  (key-seq-define-global "'r" my-cljr-keymap))

;; S — (BAD: possessives like Micah's)

;; T — (BAD: contractions like can't)

;; U — (BAD: qu combo)

;; V — Vterm
(let ((my-vterm-keymap (make-sparse-keymap)))
  (define-key my-vterm-keymap "v" 'vterm-toggle)
  (define-key my-vterm-keymap "c" 'vterm-toggle-cd-show)
  (define-key my-vterm-keymap "n" 'vterm)
  (define-key my-vterm-keymap "o" 'my-vterm-other)
  (key-seq-define-global "'v" my-vterm-keymap))

;; W — Windowing
(key-seq-define-global "'w" 'ace-window)
(key-chord-define-global "qw" 'ace-window)

;; X

;; Y — tYpography
;; —  ’ “ ‘ ‘ ’ “
(let ((my-typo-keymap (make-sparse-keymap)))
  (define-key my-typo-keymap "`" "‘")
  (define-key my-typo-keymap "'" "’")
  (define-key my-typo-keymap "~" "“")
  (define-key my-typo-keymap "\"" "”")
  (define-key my-typo-keymap "-"  "—")
  (define-key my-typo-keymap "="  "→")
  (define-key my-typo-keymap ">"  "»")
  (define-key my-typo-keymap "<"  "«")
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
  (key-seq-define-global "qy" my-typo-keymap))
(let ((my-typo-keymap (make-sparse-keymap)))
  (define-key my-typo-keymap "'" "‘")
  (define-key my-typo-keymap "\"" "“")
  (key-seq-define-global "QY" my-typo-keymap))

;; Z — folding/hide-show
;; Hide-Show custom prefix. This trick works for setting any key-chord prefix!
;; https://emacs.stackexchange.com/questions/33684/proper-way-to-change-prefix-key-for-minor-mode-map
(let ((my-hs-keymap (make-sparse-keymap)))
  (define-key my-hs-keymap "c" 'hs-hide-block)
  (define-key my-hs-keymap "C" 'hs-hide-all)
  (define-key my-hs-keymap "o" 'hs-show-block)
  (define-key my-hs-keymap "O" 'hs-show-all)
  (define-key my-hs-keymap "z" 'hs-toggle-hiding)
  (define-key my-hs-keymap "t" 'hs-toggle-hiding)
  (key-seq-define-global "'z" my-hs-keymap))

;; MASHINGS
(key-chord-define-global "AR" 'windmove-left)  ; top-left ring+pinky
;; (key-chord-define-global "ST" 'windmove-right)
(key-chord-define-global "ST" (lambda () (interactive)  (windmove-right)))
(key-chord-define-global "AR" (lambda () (interactive)  (windmove-left)))
(key-chord-define-global "RS" (lambda () (interactive)  (windmove-down)))
(key-chord-define-global "WF" (lambda () (interactive)  (windmove-up)))
(key-chord-define-global "RS" 'windmove-down)
(key-chord-define-global "WF" 'windmove-up)
;; (key-chord-define-global "XC" 'counsel-M-x)
(key-chord-define-global "DV" 'cider-clojuredocs)
(key-chord-define-global "CD" 'my-cider-inline-docs-toggle)
;; (key-chord-define-global "az" 'delete-window-balancedly)
;; http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(key-chord-define-global "KH" 'kill-this-buffer)
(key-chord-define-global "QW" (lambda () (interactive) (kill-this-buffer) (delete-window-balancedly)))
;; (key-chord-define-global "KH" 'kill-window-balancedly)
(key-chord-define-global "ZX" 'delete-window-balancedly)
;; (key-chord-define-global "ZR" 'kill-window-balancedly)
(key-chord-define-global "GT" 'magit-status)
(key-chord-define-global "XS" 'save-buffer)

;; TODO key seqs for opposited toggle
(key-seq-define-global "PB" 'make-frame-command)
(key-seq-define-global "BP" 'delete-frame)

(key-seq-define-global "JL" 'beacon-blink)
(key-seq-define-global "LJ" 'hl-line-flash)
;; (key-chord-define-global "H<" 'lispy-describe-inline)
(key-chord-define-global "TD" 'my-cider-eval-and-test-fn)
(key-chord-define-global "XX" 'my-cider-eval-to-comment)
(key-chord-define-global "AA" 'persp-switch-last)
(key-chord-define-global "BB" 'crux-switch-to-previous-buffer)

(key-chord-define-global "]]" 'my-forward-jump-to-line-break)
(key-chord-define-global "[[" 'my-backward-jump-to-line-break)

;; https://emacs.stackexchange.com/a/37894/11025
(defun my-2back-buffers ()
  "Round robin of 3 buffers."
  (interactive)
  (switch-to-buffer (elt (buffer-list) 2)))

(defun my-cider-eval-to-comment ()
  "Append result of evaluating expression."
  (interactive)
  (cider-eval-defun-to-comment)
  (forward-line)
  (let ((current-prefix-arg 4)) (delete-indentation)))


;; Other possible mashings
;; (key-chord-define-global "jl" '
;; (key-chord-define-global "./" '
;; (key-chord-define-global ".," '
;; (key-chord-define-global "kh" '
;; (key-chord-define-global "dv" '
;; (key-chord-define-global "dv" '




;; Other good combos
;; https://www.johndcook.com/blog/2015/02/01/rare-bigrams/
;; jj, kk, qq, vv, ww, yy, aa, xx, zz
;; z<consonant>
;; j<consonant>
;; Any pair of caps
;; Leaders: z / . , (z and / are nice since at opposite corners)


;;; OTHER BINDINGS


;; https://emacs.stackexchange.com/questions/4271/stop-at-beginning-of-a-word-on-forward-word
(defun my/forward-word-begin (arg)
  "Move forward a word and end with point being at beginning of next word.
Move point forward ARG words (backward if ARG is negative).
If ARG is omitted or nil, move point forward one word."
  (interactive "p")
  (forward-word arg)
  (forward-word 1)
  (backward-word 1))

(defun modi/forward-word-begin (arg)
  "Move forward ARG (defaults to 1) number of words.
Here 'words' are defined as characters separated by whitespace."
  (interactive "p")
  (dotimes (_ arg)
    (forward-whitespace 1)))

;; Work around accidental fast C-s C-x from freezing emacs! (maybe working)
;; (define-key ivy-minibuffer-map (kbd "C-x") 'keyboard-quit)

;; Don't want to suspend emacs!
;; http://superuser.com/questions/349943/how-to-awake-emacs-gui-after-pressing-ctrlz#349997
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'delete-window-balancedly)

(global-set-key (kbd "M-e") 'forward-word)
;; (global-set-key (kbd "M-f") 'forward-word)
;; (global-set-key (kbd "M-b") 'backward-word)
;; (global-set-key (kbd "M-f") 'forward-to-word)
;; (global-set-key (kbd "M-b") 'backward-to-word)
(global-set-key (kbd "M-f") 'my/forward-word-begin)

(global-set-key (kbd "M-y") 'consult-yank-pop)

;; TODO
(global-set-key (kbd "C-S-a") 'embark-act)

(global-set-key (kbd "M-o") 'scroll-up-stay)
(global-set-key (kbd "M-'") 'scroll-down-stay)




(global-set-key (kbd "C-p") 'previous-line)

(global-set-key (kbd "C-M-_") 'text-scale-decrease)
(global-set-key (kbd "C-M-+") 'text-scale-increase)

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

;; (global-set-key (kbd "C-s") 'swiper-isearch)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "M-y") 'counsel-yank-pop)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "<f2> j") 'counsel-set-variable)

;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
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
;; Disabling since elpy needs this for completion
;; (global-set-key (kbd "M-<tab>") 'ace-window) ; GUIONLY


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
;; (global-set-key (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
;; (global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
;; (global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle) ;; Cycle through Java styles
;; (global-set-key (kbd "C-c K") 'string-inflection-kebab-case) ;; Cycle through Java styles

;; smartparens overrides M-r, so changing default
(global-set-key "\M-R" 'move-to-window-line-top-bottom)
(global-set-key "\M-\C-R" 'jump-to-bottom)
;; Since already holding M-S-R, enable recenter (usually C-l) to also be M-S
(global-set-key (kbd "M-L") 'recenter-top-bottom)
(global-set-key (kbd "M-;") 'comment-dwim-2)

(global-set-key [(meta m)] 'jump-char-forward)
;; (global-set-key [(shift meta m)] 'jump-char-backward)
;; (global-set-key (kbd "C-=") 'er/expand-region)
;; TEST: Can't "do" this.
;; (global-set-key (kbd "C-c T") 'typo-mode)
;; ISSUE: Need to auto-enter typo-mode only while inside strings.
;; (global-set-key [remap mark-sexp] 'easy-mark) ; TRIAL
(global-set-key [remap kill-ring-save] 'easy-kill)
;; (global-set-key [remap kill-ring-save] 'easy-mark)
;; (global-set-key (kbd "M-%") 'anzu-query-replace)
;; (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(global-set-key (kbd "C-c '") 'imenu-list-smart-toggle)
;; (global-set-key (kbd "C-'") 'counsel-semantic-or-imenu)
;; (global-set-key (kbd "C-\"") 'popup-imenu)
;; (global-set-key (kbd "C-M-a") 'beginning-of-defun) ; replaced
(global-set-key (kbd "C-M-a") 'my-beginning-of-defun)
(global-set-key (kbd "C-M-e") 'my-end-of-defun)
(global-set-key (kbd "C-c c") 'my-copy-filename)
(global-set-key (kbd "C-x ]") 'my-forward-jump-to-line-break)

(global-set-key (kbd "C-x [") 'my-backward-jump-to-line-break)
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)



;;; UI / LOOK-N-FEEL

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

;; nice scrolling
;; https://stackoverflow.com/a/1128948/326516
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

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
;; (require 'doom-modeline)
;; (doom-modeline-mode 1)

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
;; (beacon-mode +1)

;; (require 'crosshairs)
;; (crosshairs-toggle-when-idle)

;; flash windows when you change to them
;; https://github.com/N-Mi/e-other-window
;; (require 'e-other-window)

;;; Shackle window focus management
;; https://depp.brause.cc/shackle/
(shackle-mode)
;; This particularly helps with compile windows (grep results, etc) that
;; pop up but don't get focus.
(setq shackle-default-rule '(:select t))
;; OR
;; https://emacs.stackexchange.com/questions/13212/how-to-make-occur-mode-select-the-window-of-buffer-occur
(add-hook 'occur-hook '(lambda () (switch-to-buffer-other-window "*Occur*")))
;; (define-key occur-mode-map (kbd "C-RET") (lambda () (occur-mode-goto-occurrence-other-window) (occq)))


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

;; Override to not insert boundaries
;; (defun ivy--insert-symbol-boundaries () (undo-boundary))

;; Make it easier to switch to swiper search from overlay
;; (with-eval-after-load 'symbol-overlay (define-key symbol-overlay-map (kbd "s") 'swiper-isearch-thing-at-point))
;; (define-key symbol-overlay-map (kbd "s") 'symbol-overlay-isearch-literally)
(define-key symbol-overlay-map (kbd "s") 'symbol-overlay-ctrlf-literally)


(defun symbol-overlay-ctrlf-literally ()
  "CTRLF symbol at point literally."
  (interactive)
  (unless (minibufferp)
    (let ((symbol (symbol-overlay-get-symbol)))
      (beginning-of-thing 'symbol)
      (ctrlf-forward-symbol-at-point))))




;; ENABLE??
;; https://github.com/magnars/expand-region.el
;; (require 'expand-region)

;;; Perpectives/Sessions
;; https://github.com/Bad-ptr/persp-mode.el
(setq wg-morph-on nil)


;; https://github.com/cyrus-and/zoom
;; (require 'zoom)
;; (zoom-mode)
;; (global-set-key (kbd "C-x +") 'zoom)


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

(require 'crux)

;; move line up/down (already enabled) -- M-S-up
;; move-text-up, move-text-down
(require 'move-text)

;; Focus — Dim the font color of text in surrounding paragraphs
;; https://github.com/larstvei/Focus
;; (require 'focus)

;; Mixed pitch fonts (variable width)
;; https://gitlab.com/jabranham/mixed-pitch
;; (require 'mixed-pitch)
;; FIXME: seems to need to be run manually
;; (add-hook 'text-mode 'mixed-pitch-mode)



;;; IVY, COUNSEL, SWIPER

;; (require 'ivy)
;; (ivy-mode 1)
;; (counsel-mode)
;; (setq ivy-height 30)
;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "(%d/%d) ")
;; (setq projectile-completion-system 'ivy)

;; Disabling since obscures search results
;; (require 'ivy-posframe)
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))

;; Enable counsel replacements for projectile.
;; https://github.com/ericdanan/counsel-projectile
;; (require 'counsel-projectile)
;; (counsel-projectile-mode)

;; https://github.com/raxod502/prescient.el
;; (require 'ivy-prescient)
;; (ivy-prescient-mode)

;; (require 'oneonone)
;; (1on1-emacs)


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
;; (require 'popup-imenu)

;;; Outshine (like "outline")
;; Like imenu, but for pages/sections nav/highlighting.
;; Has `outshine-imenu' as my main use
;; (require 'outshine)
;; Had to install navi-mode for parts to work.
;; (outshine-mode t)
;; (add-hook 'prog-mode-hook 'outshine-mode)


;; (require 'ido)
;; (require 'ido-completing-read+)
;; (require 'flx-ido)

;; (setq ido-enable-prefix nil
;;       ido-enable-flex-matching t
;;       ido-create-new-buffer 'always
;;       ido-use-filename-at-point 'guess
;;       ido-max-prospects 10
;;       ;; ido-save-directory-list-file (expand-file-name "ido.hist" prelude-savefile-dir)
;;       ido-default-file-method 'selected-window
;;       ido-auto-merge-work-directories-length -1)
;; ;; (ido-mode +1)
;; ;; (ido-ubiquitous-mode +1)

;;; smarter fuzzy matching for ido
;; (flx-ido-mode +1)
;; disable ido faces to see flx highlights
;; (setq ido-use-faces nil)



;;; LINTERS

;; (require 'flycheck-yamllint)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))


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


;; Typo fancy typography/punctuation
;; https://github.com/jorgenschaefer/typoel

;; Default to using typo mode for better/fancy typography
(typo-global-mode 1)
(add-hook 'text-mode-hook 'typo-mode)


;; special treatment of FIXME, etc
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)

;; Wow, hide comments!! Just blanks them out.
;; (require 'hide-comnt) ; in vendor/ since not in melpa


;; ;; auto-dim
;; ;; https://github.com/mina86/auto-dim-other-buffers.el
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (when (fboundp 'auto-dim-other-buffers-mode)
;;               (auto-dim-other-buffers-mode t))))

;; BUGGY
;; Line numbers
;; https://github.com/xcodebuild/nlinum-relative
;; Supposedly faster than linum
(require 'nlinum-relative) ; SLOW (for high LOC)
;; (global-nlinum-relative-mode 1) ; trying without to see if faster
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
;; (add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
;; (add-hook 'after-focus-change-function 'save-buffer)
;; Use super-save instead
;; https://github.com/bbatsov/super-save
(require 'super-save)
(super-save-mode +1)
(setq auto-save-default nil)
(add-to-list 'super-save-triggers 'ace-window)
;; FIXME should instead go to vterm window without toggle so leaving doesn't trigger a savee
;; (add-to-list 'super-save-triggers 'vterm-toggle)
(add-to-list 'super-save-hook-triggers 'find-file-hook)

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

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Spelling/grammar help
;; https://github.com/mhayashi1120/Emacs-langtool
;; ENABLE
;; (prelude-require-package 'langtool)

(setq tab-stop-list (number-sequence 2 200 2))

;; Zsh, hopefully
(setq indent-tabs-mode t)
(setq tab-width 2)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)


;; Repeat last command, like vim
;; https://www.emacswiki.org/emacs/dot-mode.el
(require 'dot-mode)
;; C-.  C-M-.  C-c.
(autoload 'dot-mode "dot-mode" nil t)
;; (dot-mode t)
(global-dot-mode t)
;; FIXME: prbly not working since flyspell rebinds

;; Make number colorful.
;; (require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; TODO: hydra for visible things
;; - toggle-truncate-lines
;; - crosshairs
;; - hl-line
;; - nlinum-relative-mode
;; - font sizing?
;; - visible whitespace

;; Line wrap is called "truncate" in emacs
(setq-default truncate-lines t)

;; Highlight across multiple buffers
;; https://stackoverflow.com/questions/15415504/highlighting-multiple-buffers-in-emacs
;; https://www.emacswiki.org/emacs/HighlightLibrary
;; ENABLE??
;; (require 'highlight)


;; https://github.com/ryuslash/mode-icons
;;(require 'mode-icons)
;;(mode-icons-mode)

;; https://github.com/domtronn/all-the-icons.el
;; (require 'all-the-icons)
;; (require 'all-the-icons-dired)
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; http://emacs.stackexchange.com/questions/13662/a-confirmation-after-c-x-c-c
;; (setq confirm-kill-emacs 'yes-or-no-p)
(global-unset-key (kbd "C-x C-c"))


;; Treemacs
;; It treemacs icons are big, try this manually
;; (treemacs-resize-icons 12)
;; https://github.com/Alexander-Miller/treemacs#faq

;; When some-var presents in SQL, treat it as a whole word (not 2 words).
;; https://emacs.stackexchange.com/a/59010/11025
(add-hook 'sql-mode-hook #'(lambda () (modify-syntax-entry ?- "w"))) ; not working

;; ;; Ignore git ignored
;; ;; (treemacs-git-mode 'extended)
;; (with-eval-after-load 'treemacs
;;   (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))


;;; HELP SYSTEM



;;; WINDOWING

;; Jump to help window when opened
;; http://stackoverflow.com/questions/36506141/emacs-dispatch-help-window-from-original-buffer
(setq help-window-select t)

;; https://github.com/Wilfred/helpful
(require 'helpful)
;; (global-set-key (kbd "C-h k") #'helpful-key)
;; (setq counsel-describe-function-function #'helpful-callable)
;; (setq counsel-describe-variable-function #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
;; (global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Winner mode: C-<left> C-<right>
(winner-mode)

;; Ace Window
;; https://github.com/abo-abo/ace-window/wiki
;; https://github.com/abo-abo/ace-window
(require 'ace-window)
(ace-window-display-mode t)

;; (require 'winum)
;; (winum-mode)
;; (global-set-key (kbd "M-1") 'winum-select-window-1)
;; (global-set-key (kbd "M-2") 'winum-select-window-2)
;; (global-set-key (kbd "M-3") 'winum-select-window-3)
;; (global-set-key (kbd "M-4") 'winum-select-window-4)
;; (global-set-key (kbd "M-5") 'winum-select-window-5)
;; (global-set-key (kbd "M-6") 'winum-select-window-6)
;; (global-set-key (kbd "M-7") 'winum-select-window-7)
;; (global-set-key (kbd "M-8") 'winum-select-window-8)
;; (global-set-key (kbd "M-9") 'winum-select-window-9)


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
;; (add-to-list 'default-frame-alist '(width . 440))
;; (add-to-list 'default-frame-alist (cons 'height (get-default-height)))
;; Set to known Dell dimensions
;; (add-to-list 'default-frame-alist '(width . 636)) ; big dell
;; (add-to-list 'default-frame-alist '(width . 338)) ; laptop
;; (add-to-list 'default-frame-alist '(height . 175))


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
;; (setq persp-state-default-file "~/.emacs.d/persp-mde")


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
;; Basis from: http://stackoverflow.com/a/10541426/326516
(defun scroll-up-stay (arg)
  (interactive "p")
  (let ((col (current-column)))
    (forward-line (* -1 4))
    (scroll-up 4)
    (move-to-column col)))
(defun scroll-down-stay (arg)
  (interactive "p")
  (let ((col (current-column)))
    (scroll-down 4)
    (forward-line 4)
    (move-to-column col)))


;; camel, kebab cases
;; https://stackoverflow.com/a/27422814/326516
;; (require 'string-inflection)


(defun jump-to-bottom ()
  (interactive)
  (move-to-window-line-top-bottom)
  (move-to-window-line-top-bottom))


;;; Commenter
(require 'comment-dwim-2)




;;; GIT

;; https://github.com/emacsorphanage/git-messenger
(require 'popup)
(require 'git-messenger)
(require 'magit)
(require 'git-timemachine)
(require 'diff-hl)
(global-diff-hl-mode)

;; (setq git-identity-default-username "Micah Elliott")
;; (require 'git-identity)
;; ;; (git-identity-magit-mode 1)
;; (define-key magit-status-mode-map (kbd "I") 'git-identity-info)

(defun my-magit-status ()
  (interactive)
  (magit-status)
  (balance-windows))


;; Pretty Magit Emoji
;; http://www.modernemacs.com/post/pretty-magit/
(require 'dash)

(defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
  "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
  `(prog1
       (add-to-list 'pretty-magit-alist
                    (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
                          ,ICON ',PROPS))
     (unless ,NO-PROMPT?
       (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

(setq pretty-magit-alist nil)
(setq pretty-magit-prompt nil)
(pretty-magit "Feature" ?✨ (:foreground "slate gray"))
(pretty-magit "feat" ?✨ (:foreground "slate gray"))
(pretty-magit ":sparkles" ?✨ (:foreground "slate gray"))
(pretty-magit "Add"     ?✨ (:foreground "#375E97"))
(pretty-magit "Fix"     ?🐛 (:foreground "#FB6542"))
(pretty-magit "fix"     ?🐛 (:foreground "#FB6542"))
(pretty-magit ":bug"    ?🐛 (:foreground "#FB6542"))
(pretty-magit "Clean"   ?👮 (:foreground "#FFBB00"))
(pretty-magit "refactor" ?👮  (:foreground "#FFBB00"))
(pretty-magit ":cop"  ?👮  (:foreground "#00BB00")) ; refactor
(pretty-magit "style"  ?👮  (:foreground "#00BB00"))
(pretty-magit ":wrench"  ?🔧 (:foreground "#FFBB00")) ; ci, tooling, config
(pretty-magit "build"  ?🔧 (:foreground "#FFBB00")) ; ci, tooling, config
(pretty-magit "test" ?✅  (:foreground "#FFBB00"))
(pretty-magit ":check"  ?✅  (:foreground "#FFBB00"))
(pretty-magit "Docs"    ?📚 (:foreground "#3F681C"))
(pretty-magit "docs"    ?📚 (:foreground "#3F681C"))
(pretty-magit ":books"    ?📚 (:foreground "#3F681C"))
(pretty-magit "security"    ?🔒 (:foreground "#3F681C"))
(pretty-magit ":lock"    ?🔒 (:foreground "#3F681C"))
(pretty-magit "master"  ? (:box t :height 1.2) t)
(pretty-magit "origin"  ?⌱ (:box t :height 1.2) t)

(defun add-magit-faces ()
  "Add face properties and compose symbols for buffer from pretty-magit."
  (interactive)
  (with-silent-modifications
    (--each pretty-magit-alist
      (-let (((rgx icon props) it))
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp rgx nil t)
            (compose-region
             (match-beginning 1) (match-end 1) icon)
            (when props
              (add-face-text-property
               (match-beginning 1) (match-end 1) props))))))))

(advice-add 'magit-status :after 'add-magit-faces)
(advice-add 'magit-refresh-buffer :after 'add-magit-faces)


(require 'unicode-fonts)
(unicode-fonts-setup)




;;; REST CLIENTS
;; (require 'restclient)
;; (require 'httprepl)


;; Select/highlight with easy-kill
;; https://github.com/leoliu/easy-kill
;; http://stackoverflow.com/a/36631886/326516
(require 'easy-kill)
(global-set-key (kbd "C-M-SPC") 'easy-mark-sexp)


;;; Hide-Show (V: visible), like folding

;; NOTE: set-selective-display is unsufficient since can only operate
;; at file level, so you can't fold just a single fn.


;; Adds the docstring detection to hs-minor-mode for clojure (not really)
;; (add-to-list 'hs-special-modes-alist
;;              '(clojure-mode "(defn [-a-z]+ \"[^\"]+\"" ")" ";; " nil nil))

;; Clojure folding
;; http://bytopia.org/2016/12/17/autocollapse-namespace-definitions-in/
(defun hs-clojure-hide-namespace-and-folds ()
  "Hide the first (ns ...) expression in the file, and also all the (^:fold ...) expressions."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (goto-char (point-min))
     (when (ignore-errors (re-search-forward "^(ns "))
       (hs-hide-block))

     (while (ignore-errors (re-search-forward "\\^:fold"))
       (hs-hide-block)
       (next-line)))))

(defun hs-clojure-mode-hook ()
  (interactive)
  (hs-minor-mode 1)
  (hs-clojure-hide-namespace-and-folds))
(add-hook 'clojure-mode-hook 'hs-clojure-hide-namespace-and-folds)


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

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))


;; AVY: https://github.com/abo-abo/avy/wiki/defcustom
;; avy allows us to effectively navigate to visible things without a mouse
(require 'avy)
(setq avy-background t)
(setq avy-style 'at-full)
;; confine avy's characters for ease of typing/finding
;; (setq avy-keys (number-sequence ?a ?f))
;; Use only easiest left and right keys
;; (setq avy-keys (string-to-list "asdfwerkluioghqtypvcxzj"))
;; (setq avy-keys (string-to-list "arstneioqwfpluyzxcdh"))
(setq avy-keys (string-to-list "qwfpluyzxcdhgmbgvjmkneioarst"))
;; (setq avy-keys (number-sequence ?a ?z))
;; (setq avy-keys (string-to-list "arstgmneiowfpluy"))
;; (setq avy-keys (string-to-list "arstneio"))
;; only search in current window
(setq avy-all-windows nil)
;; make case-sensitive
(setq avy-case-fold-search t)

;; (setq avy-orders-alist '((avy-goto-char-2-above . avy-order-closest)
;; 			 (avy-goto-char-2-below . avy-order-closest)))



(global-set-key (kbd "C-c C-u") 'browse-url)


;;; PARENS

;; Rainbow all the things

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(rainbow-delimiters-mode +1)

;; https://github.com/Fanael/rainbow-identifiers
;; (require 'rainbow-identifiers)
;; (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

;; https://www.emacswiki.org/emacs/ShowParenMode
;; (setq show-paren-delay 0)
;; (show-paren-mode 0)
;; Highlight matching parens
;; https://github.com/Fuco1/smartparens/wiki/Show-smartparens-mode
;; Can be SLOW with long lines!
;; (show-smartparens-mode t)
;; More matching parens: colors block you're in red (SLOW?)
;; https://github.com/tsdh/highlight-parentheses.el
;; TODO testing if slow
;; (require 'highlight-parentheses)
;; (highlight-parentheses-mode t)

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

;; FIXME: why are these so SLOW??
;; (add-hook 'text-mode-hook 'flyspell-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; Automatically run spell checker.
;; (add-hook 'flyspell-mode-hook #'flyspell-buffer)

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

;; TODO Decide if really wanted
;; Popup window manager for short/wide bottom C-g killable popup
;; https://github.com/emacsorphanage/popwin
;; (require 'popwin)
;; (popwin-mode 1)

;; Icons for company popup?
;; TODO Themeable
;; YAY!!!
;; https://github.com/sebastiencs/company-box
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)



;;; COLORS / THEMES

;; Special sectional comments
;; https://emacs.stackexchange.com/questions/28232/syntax-highlighting-for-comments-starting-with-specific-sequence-of-characters
(defface special-comment '((t (:foreground "#c4bf27" :weight ultra-bold))) "My Special Comment")
(defface sfdc-field'((t (:foreground "#dF9522"))) "My SFDC Field")
;; Play with setting dynamically
;; (face-spec-set 'special-comment '((t :foreground "#ff00ff" :underline t :slant normal)))
;; (face-spec-set 'sfdc-field '((t :foreground "#ff00ff" :underline t :slant normal)))
(font-lock-add-keywords 'clojure-mode '((";;;.*" 0 'special-comment t)))
(font-lock-add-keywords 'clojure-mode '(("\\w[A-z0-9_]+__c" 0 'sfdc-field t)))

;; (re-search-forward "[A-z_]+__c")
 ;; Aaa_Vvv__c


;;; Sonic PI



;;; LANGUAGES

;;; Ruby

;; (prelude-require-package 'chruby)
;; (require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)


;;; Markdown

(require 'flymd)
;; (require 'markdown-toc)
;; Enable syntax highlighting of code in blocks.
(setq markdown-fontify-code-blocks-natively t)


;; Gherkin/Cucumber
;; (require 'feature-mode)
;; Just for emacs testing
;; (prelude-require-package 'ecukes)
;; (require 'cucumber-goto-step)


;; Database UI
;; (require 'edbi)

;; (require 'popup)

;; https://emacs.stackexchange.com/a/2779/11025
(defun my-describe-function-at-point ()
  (interactive)
  (describe-function (function-called-at-point)))
;; (key-chord-define-global "CD" 'my-describe-function-at-point)
;; TODO bind to just elisp mode


;;; Clojure

;; Still need to highleight and press TAB to make work.
(setq clojure-align-forms-automatically t)

;; NOTE: also installed to ~/.lein/profiles.clj: kibit, eastwood
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(require 'cider)
;; (setq cider-repl-history-file "~/.clojure_history")
;; (require 'cider-eval-sexp-fu) ; breaks elpy
;; (require 'clj-refactor)
;; (require 'clojure-snippets) ; yas for clojure
(require 'flycheck-clojure)
;; (require 'company-flx)
;; (require 'flycheck-joker)
(require 'kibit-helper)
(require 'sotclojure)
;; (require 'clojure-mode-extra-font-locking)

;; Quick-Peek for Cider
;; https://github.com/clojure-emacs/cider/issues/2968
(require 'quick-peek)
(defvar my-cider-inline-docs-opened-p nil
  "Toggle to support an inline help being open (expanded) or not.")
(defun my-cider-inline-docs-toggle ()
  "Show a fn or ns docstring's key pieces as inline overlay."
  ;; Another way to do this is to use "popup" instead of quick-peek.
  (interactive)
  (if my-cider-inline-docs-opened-p
      (progn
	(setq my-cider-inline-docs-opened-p nil)
	(quick-peek-hide))
    ;; If not on a word, let it error
    (let* ((info    (cider-var-info (thing-at-point 'word 'no-properties)))
           ;; Built-in fns' info are indexed differently from user-defined.
	   (arglist (nrepl-dict-get info "arglists-str"))
	   (doc     (nrepl-dict-get info "doc")))
      (if doc
	  (progn
	    (quick-peek-show
	     (concat (if arglist (concat arglist "\n") "")
		     (replace-regexp-in-string "^  " "" doc)))
	    (setq my-cider-inline-docs-opened-p t))
	(message "Missing docstring or invalid thing at point")))))
;; (key-chord-define-global "CD" 'my-cider-inline-docs-toggle)


;;; Nofitications
;; https://github.com/jwiegley/alert
(setq alert-default-style 'notifier)
(alert "Tis but an alert" :severity 'high :title "some title")

;; (setq hs-special-modes-alist)

;; (defun my-projectile-ivy-ag ()
;;   "Populate ag with a search term of thing at point."
;;   (interactive)
;;   (let ((counsel-projectile-ag-initial-input
;; 	 (thing-at-point 'word 'no-properties)))
;;     (counsel-projectile-ag)))
;; ;; (key-chord-define-global "RF" 'my-projectile-ivy-ag)


(defun my-projectile-ctrlf-ag ()
  "Populate ag with a search term of thing at point."
  (interactive)
  (projectile-ag (thing-at-point 'word 'no-properties)))
(key-chord-define-global "RF" 'my-projectile-ctrlf-ag)

;; (require 'winnow)


;; For kondo: https://github.com/borkdude/flycheck-clj-kondo#multiple-linters
(require 'flycheck-clj-kondo)
(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
;; (dolist (checkers '((clj-kondo-clj . clojure-joker)
;;                     (clj-kondo-cljs . clojurescript-joker)
;;                     (clj-kondo-cljc . clojure-joker)
;;                     (clj-kondo-edn . edn-joker)))
;;   (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))

;; TODO get these docs working!
;; (require 'clojure-essential-ref)
;; (require 'clojure-essential-ref-nov)
;; (setq clojure-essential-ref-nov-epub-path "~/Downloads/Clojure_The_Essential_Reference_v30_MEAP.epub")

;; Trying to get rid of the prompt to save before load.
(defun my-cider-load-buffer ()
  "Call normal load-buffer, but save first."
  (save-buffer)
  (cider-load-buffer))

(defun my-cider-eval-and-test-fn ()
  "Quickly eval and run test."
  (interactive)
  (cider-eval-defun-at-point)
  (cider-test-run-test))

(global-set-key (kbd "M-h") 'mark-paragraph)

;; https://github.com/clojure-emacs/clj-refactor.el
(defun my-clojure-mode-hook () "Foo bar."
       (message "in my-clojure-mode-hook")
       ;; (clj-refactor-mode 1)
       ;; (yas-minor-mode 1) ; for adding require/use/import statements
       ;; This choice of keybinding leaves cider-macroexpand-1 unbound
       (define-key clojure-mode-map (kbd "C-c C-k") 'my-cider-load-buffer)
       (setq cider-repl-pop-to-buffer-on-connect 'display-only)
       (setq cider-repl-result-prefix ";; => ")
       (setq cider-save-file-on-load t)
       (setq cider-prompt-for-symbol nil)
       (setq cider-repl-wrap-history t)
       (setq cider-repl-history-size 1000)
       (setq cider-repl-history-file "~/.cider-repl-history")
       (cider-auto-test-mode 1)
       ;; (cljr-add-keybindings-with-prefix "C-c r")
       ;; (cljr-add-keybindings-with-prefix "C-S-r")
       ;; (key-chord-define-global "'r" 'cljr-add-keybindings-with-prefix)
       ;; (key-chord-define-global "qr" 'cljr-add-keybindings-with-prefix)
       ;; (key-chord-define clojure-mode-map "qr" 'cljr-ivy)
       ;; (cljr-add-keybindings-with-prefix "C-c m")
       ;; (define-key clojure-mode-map (kbd "C-c C-r") 'cljr-ivy)
       ;; (global-set-key (kbd "C-c R") 'cljr-helm)
       ;; (global-set-key (kbd "C-S-r") 'cljr-helm)
       ;; (global-set-key (kbd "C-c r") 'cljr-helm)
       ;; (global-set-key (kbd "C-S-T") 'cider-test-commands-map)
       ;; Disable flycheck next error in favor of Cider
       (define-key clojure-mode-map (kbd "C-c C-n") 'cider-ns-map)
       ;; (define-key clojure-mode-map (kbd "C-c C-d C-r") 'clojure-essential-ref)
       (define-key clojure-mode-map (kbd "C-c C-") 'cider-read-and-eval-defun-at-point)
       (global-unset-key (kbd "C-c C-p"))
       (define-key clojure-mode-map (kbd "C-c C-p") 'cider-inspect)
       ;; (define-key (kbd "C-c r"))
       ;; (company-flx-mode +1)
       (define-key clojure-mode-map (kbd "M-J") 'sp-join-sexp) ; maybe already done by smartparens
       ;; Make similar to wrapping with M-(
       (define-key clojure-mode-map (kbd "M-[") (lambda () (interactive) (sp-wrap-with-pair "[")))
       ;; Overrides tmm-menubar
       (define-key clojure-mode-map (kbd "M-`") (lambda () (interactive) (sp-wrap-with-pair "`")))
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
;; (require 'discover-clj-refactor) ; C-c j

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


;;; PYTHON

(require 'python)
;; (require 'elpy)
;; (elpy-enable)


;; importmagic
;; zimports
;; pyimport: auto-import

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;; https://emacs.stackexchange.com/questions/30082/your-python-shell-interpreter-doesn-t-seem-to-support-readline
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

(setq python-shell-completion-native-enable nil)


;;; ORG MODE

;; https://emacs.stackexchange.com/questions/9709/keep-the-headlines-expanded-in-org-mode
(setq org-startup-folded nil)
(setq org-startup-with-inline-images t)
(setq org-confirm-babel-evaluate nil)

;; (require 'org-download)


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

(defun my-end-of-defun ()
  "Jump to start of name of function, since often want to search it."
  (interactive)
  (end-of-defun)
  (end-of-defun)
  (my-beginning-of-defun))

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
  (let ((proc (get-process "vterm"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (vterm)
        (switch-to-buffer currbuff)
        (setq proc (get-process "vterm"))))
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
    (process-send-string proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (forward-line))))
(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))
;; (defun sh-switch-to-process-buffer ()
;;   (interactive)
;;   (pop-to-buffer (process-buffer (get-process "vterm")) t))

;; (setq comint-scroll-to-bottom-on-output t)

;; (define-key sh-mode-map [(control ?j)] 'sh-send-line-or-region-mand-step)
;; (define-key sh-mode-map [(control ?j)] 'sh-send-line-or-region)
;; (define-key sh-mode-map [(control ?c) (control ?z)] 'sh-switch-to-process-buffer)
(define-key shell-mode-map [(control ?c) (control ?z)] 'sh-switch-to-process-buffer)
;; (define-key vterm-mode-map [(control ?c) (control ?z)] 'aw-flip-window)
;; (define-key vterm-mode-map [(control ?c) (control ?z)] 'aw-)


;; (require 'vterm) ; TEMPORARY
;; (define-key vterm-mode-map (kbd "C-c C-z") (lambda () (interactive) (other-window -1)))
;; (require 'vterm-toggle) ; TEMPORARY
;; (global-set-key [f2] 'vterm-toggle)
;; Not working
(setq vterm-toggle-hide-method nil)

(defun my-vterm-other ()
  (interactive)
  (select-window (get-buffer-window (vterm-toggle--recent-other-buffer))))


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

;; Show Marks (contrib)
;; https://www.emacswiki.org/emacs/download/show-marks.el
;; (require 'fm)
;; (require 'show-marks)

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

;; ;; Highlight numbers in groups of three, thousands, rather than using commas
;; ;; https://emacs.stackexchange.com/questions/54505/how-to-highlight-digit-groups-of-3-in-numerals
;; (defun my-matcher (limit)
;;   (when (re-search-forward
;;          "\\([0-9]\\{1,3\\}\\)\\(?:[0-9]\\{6\\}\\)*\\(?:[0-9]\\{3\\}\\)\\_>" limit t)
;;     (goto-char (match-beginning 1))
;;     (re-search-forward "[0-9]+" (match-end 1))))
;; (font-lock-add-keywords nil '((my-matcher 0 font-lock-string-face)))

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

(defun my-find-file-below (fname)
  (interactive)
  (split-window-vertically-balancedly)
  (find-file fname))

;; https://irreal.org/blog/?p=3544
(defun ibuffer-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (next-line 3))
(defun ibuffer-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (next-line -2))
(eval-after-load 'ibuffer
  '(progn
     (define-key ibuffer-mode-map
       (vector 'remap 'end-of-buffer) 'ibuffer-jump-to-bottom)
     (define-key ibuffer-mode-map
       (vector 'remap 'beginning-of-buffer) 'ibuffer-back-to-top)))


(defun my-ibuffer ()
  (interactive)
  (split-window-balancedly)
  (ibuffer))

;; (defun counsel-refcards ()
;;   "Search local refcards of your own devising."
;;   (interactive)
;;   (let* ((default-directory "~/doc/refcards")
;;          (cands (split-string (shell-command-to-string "ls") nil t)))
;;     (ivy-read "Topic: " cands :action #'my-find-file-below)))
;; (global-set-key (kbd "C-h r") 'counsel-refcards)

(defun org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t)))
(define-key org-mode-map (kbd "C-c e") 'org-toggle-emphasis)

(defun my/ffr (filename)
  "Foo `FILENAME'."
  (interactive "P")
  (split-window-right)
  (e-other-window 1)
  (find-file filename))

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


;; https://emacs.stackexchange.com/a/13096/11025
(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

;; (add-hook 'emacs-lisp-mode-hook
;;           (defun enable-autoreload-for-dir-locals ()
;;             (when (and (buffer-file-name)
;;                        (equal dir-locals-file
;;                               (file-name-nondirectory (buffer-file-name))))
;;               (add-hook (make-variable-buffer-local 'after-save-hook)
;;                         'my-reload-dir-locals-for-all-buffer-in-this-directory))))



(defun my-vterm-send-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((current-line (thing-at-point 'line t)))
	(with-current-buffer (get-buffer "vterm")
	  (vterm-send-string current-line))
        (forward-line)))))

(defun my-vterm-send-buffer-2 ()
  (interactive)
  (let ((buffer-content (buffer-string)))
    (with-current-buffer (get-buffer "vterm")
      (vterm-send-string buffer-content))))


(require 'vterm)

(define-key vterm-mode-map (kbd "C-RET") #'my-vterm-send-buffer-2)

(define-key vterm-mode-map (kbd "C-s") #'vterm-send-C-c)

(defun ff-new-win ()
  (find-file))




(defun my-cider-find-var ()
  (interactive)
  (make-frame-command)
  (cider-find-var)
  (recenter-top-bottom))
(global-set-key (kbd "C->") 'my-cider-find-var)



;;; MACROS

(fset 'unthread1
      (kmacro-lambda-form
       [?\C-\M-d ?\C-\M-f ?\C-f ?\C-\M-  ?\C-w ?\C-\M-f ?\C-\M-b ?\C-\M-d ?\C-\M-f ?  ?\C-y return ?\C-\M-u ?\M-r ?\C-\M-  tab]
       0 "%d"))

(fset 'clj-sort-ns
      (kmacro-lambda-form [?\M-\{ ?\C-n ?\M-f return
				  ?\C-\M-u ?\C-\M-  tab ?\C-b return
				  ?\C-p ?\C-e ?\C-  ?\C-\M-u ?\C-n ?\C-a ?\C-a ?\M-x ?s ?o ?r ?t ?- ?l ?i ?n ?e ?s return
				  ?\C-\M-n ?\C-\M-e ?\C-x ?\C-e] 0 "%d"))

(fset 'clj-sort-ns-2
      (kmacro-lambda-form [?\M-\} ?\C-\M-f ?\C-b ?\C-b return ?\C-p ?\C-e ?\C-  ?\C-\M-u ?\C-n ?\C-a ?\C-a ?\M-x ?s ?o ?r ?t ?- ?l ?i ?n ?e ?d backspace ?s return ?\C-\M-n] 0 "%d"))



;;; END

;; https://github.com/Yevgnen/ivy-rich/blob/master/screenshots.org
;; I think ivy-rich is like marginalia, giving inline docs is ivy
;; (require 'ivy-rich)
;; (all-the-icons-ivy-rich-mode 1) ; FIXME: needs manual enabling
;; (ivy-rich-mode 1)
;; (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
;; (setq ivy-rich-path-style 'abbrev)

;; https://github.com/preetpalS/emacs-dotenv-mode
(require 'dotenv-mode) ; unless installed from a package
 ;; for optionally supporting additional file extensions such as `.env.test' with this major mode
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

;; https://github.com/wbolster/emacs-direnv
(require 'direnv)
(direnv-mode)

;; Use direnv — supposed to be near bottom in init.el
;; https://github.com/purcell/envrc (seems better than direnv emacs package)
(require 'envrc)
(envrc-global-mode)







;;; SELECTRUM, CTRLF, PRESCIENT/ORDERLESS, CONSULT, MARGINALIA

;; TODO

(selectrum-mode +1) ; useful even when ivy for any completing-read
(setq completion-styles '(substring partial-completion))

;; Completion styles
(require 'orderless)
(setq completion-styles '(orderless))

(selectrum-prescient-mode +1)
;; ;; save command history on disk, so the sorting gets more intelligent over time
(prescient-persist-mode +1)

(marginalia-mode)
;; ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
(advice-add #'marginalia-cycle :after
            (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
(define-key minibuffer-local-map (kbd "C-M-a") 'marginalia-cycle)

;; (ctrlf-mode +1) ; search, like normal emacs but improved, add C-s binding

;; Consult (like isearch but improved)
;; https://github.com/minad/consult

;; (global-set-key (kbd "M-s s") 'consult-isearch)
(global-set-key (kbd "C-s") 'consult-line)

;; Optionally configure a function which returns the project root directory
(autoload 'projectile-project-root "projectile")
(setq consult-project-root-function #'projectile-project-root)



;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-c b" . consult-bookmark)
	 ("C-c k" . consult-kmacro)
	 ;; C-x bindings (ctl-x-map)
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (UNRELATED!)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ("<help> a" . consult-apropos)            ;; orig. apropos-command
	 ;; M-g bindings (goto-map)
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-project-imenu) ;; Alternative: consult-imenu
	 ("M-g e" . consult-error)
	 ;; M-s bindings (search-map)
	 ("M-s g" . consult-git-grep)              ;; alt. consult-grep, consult-ripgrep
	 ("M-s f" . consult-find)                  ;; alt. consult-locate, find-fd
	 ("M-s l" . consult-line)
	 ("M-s m" . consult-multi-occur)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Replacement for isearch-edit-string
	 ("M-s e" . consult-isearch)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch))              ;; orig. isearch-edit-string

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Custom command wrappers. It is generally encouraged to write your own
  ;; commands based on the Consult commands. Some commands have arguments which
  ;; allow tweaking. Furthermore global configuration variables can be set
  ;; locally in a let-binding.
  (defun find-fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial)))

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
	register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds stripes, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Configure preview. Note that the preview-key can also be configured on a
  ;; per-command basis via `consult-config'.
  ;; The default value is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-p"))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))

  ;; Optionally configure narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; Probably not needed if you are using which-key.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optional configure a view library to be used by `consult-buffer'.
  ;; The view library must provide two functions, one to open the view by name,
  ;; and one function which must return a list of views as strings.
  ;; Example: https://github.com/minad/bookmark-view/
  ;; (setq consult-view-open-function #'bookmark-jump
  ;;       consult-view-list-function #'bookmark-view-names)

  ;; Optionally configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  ;; Recall last search
  ;; https://github.com/minad/consult/issues/214
  (setf (alist-get #'consult-line consult-config)
	(list :keymap (let ((map (make-sparse-keymap)))
			(define-key map "\C-s" #'previous-history-element)
			map)))

  )

;; Optionally add the `consult-flycheck' command.
(use-package consult-flycheck
  :bind (:map flycheck-command-map
	      ("!" . consult-flycheck)))



;; Line breaks (C-l, ^L) are shown as pretty horizontal lines
;; Maybe needs to be at end of file
;; Navigate: C-x ] and C-x [
;; https://stackoverflow.com/a/7577628/326516
(require 'page-break-lines)
(global-page-break-lines-mode)


(require 'mood-line)
(mood-line-mode)



;;; END (effectively)

(provide 'init)

;; '(safe-local-variable-values
;;    '((eval with-eval-after-load 'cider
;; 	   (setq cider-default-cljs-repl 'figwheel))
;;      (cider-lein-global-options . "with-profile +dev,+test")
;;      (scss-mode
;;       (css-indent-offset . 2))))


;;; init ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-window-display-mode t)
 '(auto-dim-other-buffers-mode nil)
 '(avy-orders-alist nil)
 '(avy-styles-alist '((avy-goto-char-2 . pre)))
 '(avy-timeout-seconds 0.2)
 '(aw-char-position 'center)
 '(aw-display-mode-overlay nil)
 '(aw-ignore-current nil)
 '(beacon-blink-duration 0.4)
 '(beacon-blink-when-focused t)
 '(beacon-blink-when-point-moves-vertically 5)
 '(beacon-color "red")
 '(beacon-push-mark nil)
 '(browse-url-browser-function 'browse-url-firefox)
 '(browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox")
 '(case-fold-search nil)
 '(cider-comment-prefix " ;=> ")
 '(cider-repl-history-file "~/.cider-repl-history")
 '(cider-repl-history-size 1000)
 '(cider-repl-use-clojure-font-lock nil)
 '(cider-repl-use-pretty-printing nil)
 '(cider-special-mode-truncate-lines nil)
 '(col-highlight-show-only 'forward-paragraph)
 '(custom-safe-themes
   '("39b0c917e910f32f43f7849d07b36a2578370a2d101988ea91292f9087f28470" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(ediff-split-window-function 'split-window-horizontally)
 '(fci-rule-color "#383838")
 '(flycheck-pycheckers-checkers '(pylint pep8 pyflakes bandit))
 '(global-hl-line-mode t)
 '(global-superword-mode t)
 '(global-yascroll-bar-mode t)
 '(highlight-nonselected-windows t)
 '(highlight-parentheses-colors '("red" "IndianRed1"))
 '(highlight-parentheses-delay 0.3)
 '(highlight-parentheses-highlight-adjacent t)
 '(hl-line-flash-show-period 2.0)
 '(hs-hide-comments-when-hiding-all nil)
 '(imenu-list-focus-after-activation t)
 '(imenu-list-position 'left)
 '(imenu-list-size 0.1)
 '(inhibit-startup-screen nil)
 '(magit-log-arguments '("--graph" "--color" "--decorate" "--stat" "-n10"))
 '(markdown-header-scaling t)
 '(markdown-wiki-link-search-subdirectories t)
 '(mood-line-show-cursor-point nil)
 '(mood-line-show-encoding-information nil)
 '(org-babel-load-languages '((emacs-lisp . t) (clojure . t) (shell . t)))
 '(package-selected-packages
   '(org-download epresent super-save unicode-fonts company-prescient orderless winum mood-line auto-package-update use-package consult-flycheck project-explorer shackle highlight-numbers alert sonic-pi quick-peek sotclojure rg consult marginalia selectrum-prescient prescient ctrlf selectrum embark company-jedi key-seq aggressive-indent dotenv-mode flycheck-inline vterm-toggle vterm org-bullets org-preview-html github-browse-file envrc direnv perspective helpful company-box popwin company-posframe git-link imenu-list ibuffer-vc company-flx company-fuzzy symbol-overlay csv-mode yaml-mode diminish which-key diff-hl git-timemachine qjakey-chord visible-mark flycheck-pos-tip company-quickhelp move-text easy-kill ample-theme beacon unfill undo-tree typo smartparens shrink-whitespace ripgrep rainbow-delimiters paren-face page-break-lines markdown-mode magit kibit-helper jump-char highlight-parentheses git-messenger flymd flycheck-clojure flycheck-clj-kondo fic-mode feature-mode expand-region exec-path-from-shell edit-indirect dumb-jump dot-mode crux company comment-dwim-2 buffer-move ag ace-window))
 '(page-break-lines-max-width 80)
 '(page-break-lines-modes
   '(emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode clojure-mode))
 '(persp-sort 'access)
 '(popwin:popup-window-height 30)
 '(projectile-enable-caching t)
 '(projectile-file-exists-remote-cache-expire nil)
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "vendor"))
 '(projectile-indexing-method 'hybrid)
 '(projectile-mode t nil (projectile))
 '(projectile-sort-order 'recently-active)
 '(quick-peek-add-spacer nil)
 '(quick-peek-position 'above)
 '(recentf-auto-cleanup 300)
 '(recentf-max-menu-items 100)
 '(recentf-max-saved-items 500)
 '(safe-local-variable-values
   '((eval with-eval-after-load 'cider
	   (setq cider-default-cljs-repl 'figwheel))))
 '(scroll-bar-mode nil)
 '(search-whitespace-regexp "\"[ \\t\\r\\n]+\"")
 '(selectrum-count-style 'current/matches)
 '(selectrum-display-style '(vertical))
 '(selectrum-num-candidates-displayed 10)
 '(selectrum-show-indices nil)
 '(shackle-mode t)
 '(show-trailing-whitespace t)
 '(split-height-threshold 100)
 '(split-width-threshold 30)
 '(standard-indent 2)
 '(symbol-overlay-faces
   '(symbol-overlay-face-1 symbol-overlay-face-3 symbol-overlay-face-7 symbol-overlay-face-8))
 '(text-scale-mode-step 1.1)
 '(tldr-enabled-categories '("common"))
 '(tramp-default-method "ssh")
 '(which-key-max-description-length 45))

;; '(mixed-pitch-fixed-pitch-faces
;;   '(diff-added diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face line-number line-number-current-line line-number-major-tick line-number-minor-tick markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-block-begin-line org-block-end-line org-document-info-keyword org-code org-indent org-latex-and-related org-checkbox org-formula org-meta-line org-table org-verbatim))
;; '(mixed-pitch-set-height t)

;; '(persp-auto-resume-time 0.0)
;; '(persp-auto-save-fname "my-autosave")
;; '(persp-auto-save-num-of-backups 0)
;; '(persp-auto-save-opt 0)
;; '(persp-auto-save-persps-to-their-file nil)
;; '(persp-autokill-persp-when-removed-last-buffer 'kill)
;; '(persp-nil-name "main")

;; '(elpy-rpc-python-command "poetry run python")
;; '(elpy-rpc-virtualenv-path "")

;; '(doom-modeline-buffer-file-name-style 'truncate-with-project)
;; '(doom-modeline-buffer-modification-icon nil)
;; '(doom-modeline-buffer-state-icon nil)
;; '(doom-modeline-continuous-word-count-modes nil)
;; '(doom-modeline-display-default-persp-name t)
;; '(doom-modeline-github t)
;; '(doom-modeline-height 16)
;; '(doom-modeline-icon nil)
;; '(doom-modeline-indent-info nil)
;; '(doom-modeline-major-mode-icon nil)
;; '(doom-modeline-modal-icon nil)
;; '(doom-modeline-persp-icon nil)
;; '(doom-modeline-unicode-fallback t)
;; '(doom-modeline-vcs-max-length 40)

;; '(cljr-favor-private-functions nil)
;; '(cljr-hotload-dependencies t)
;; '(cljr-magic-require-namespaces
;;   '(("str" . "clojure.string")
;;     ("set" . "clojure.set")
;;     ("pp" . "clojure.pprint")
;;     ("zip" . "clojure.zip")
;;     ("edn" . "clojure.edn")
;;     ("t" . "clojure.test")
;;     ("as" . "clojure.core.async")
;;     ("logic" . "clojure.core.logic")
;;     ("walk" . "clojure.walk")
;;     ("xml" . "clojure.data.xml")
;;     ("csv" . "clojure.data.csv")
;;     ("spec" . "clojure.spec.alpha")
;;     ("io" . "clojure.java.io")
;;     ("mat" . "clojure.core.matrix")
;;     ("json" . "cheshire.core")
;;     ("time" . "java-time")
;;     ("spr" . "com.rpl.specter")
;;     ("http" . "clj-http.client")
;;     ("log" . "clojure.tools.logging")
;;     ("e" . "taoensso.encore")
;;     ("s3" . "amazonica.aws.s3")
;;     ("sql" . "hugsql.core")
;;     ("yaml" . "clj-yaml.core")
;;     ("sh" . "clojure.java.shell")
;;     ("w" . "clojure.walk")
;;     ("fs" . "me.raynes.fs")
;;     ("r" . "reagent.core")
;;     ("rf" . "re-frame.core")))

;; '(treemacs-collapse-dirs 6)
;; '(treemacs-follow-mode nil)
;; '(treemacs-no-png-images t)
;; '(treemacs-width 45)

;; '(rainbow-identifiers-face-count 15)

;; '(ivy-initial-inputs-alist nil)
;; '(ivy-posframe-border-width 3)
;; '(ivy-posframe-min-width 120)
;; '(ivy-posframe-width 160)
;; '(ido-default-file-method 'selected-window)
;; '(neo-autorefresh t)
;; '(neo-hidden-regexp-list
;;   '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" "\\.xml$"))
;; '(neo-show-hidden-files t)
;; '(neo-show-slash-for-folder nil)
;; '(neo-smart-open t)
;; '(neo-theme 'icons)
;; '(neo-window-position 'left)
;; '(neo-window-width 40)


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
