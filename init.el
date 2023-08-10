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

;; https://stackoverflow.com/questions/5570451/how-to-start-emacs-server-only-if-it-is-not-started
;; upsets magit
(load "server")
(unless (server-running-p) (server-start))


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
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("cselpa" . "https://elpa.thecybershadow.net/packages/")))

(setq package-enable-at-startup nil)
;; (package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages
  '(ace-window
    ag
    aggressive-indent
    alert
    ample-theme
    auto-compile
    avy
    beacon
    buffer-move
    cider
    clojure-mode
    comment-dwim-2
    consult
    consult-eglot
    conventional-changelog
    corfu
    crux
    csv
    dash
    diff-hl
    dired-rainbow
    dired-sidebar
    direnv
    dotenv-mode
    dot-mode
    dumb-jump
    edbi
    edebug-inline-result
    edit-indirect
    eglot
    embark
    embark-consult
    envrc
    exec-path-from-shell
    flymd
    flymake-kondor
    git-gutter
    git-link
    git-timemachine
    github-browse-file
    goggles
    gpt
    helpful
    highlight
    highlight-escape-sequences
    highlight-indentation
    highlight-numbers
    highlight-parentheses
    hl-todo
    ibuffer-vc
    imenu-list
    jet
    jump-char
    just-mode
    justl
    key-chord
    key-seq
    keycast
    magit
    marginalia
    mark-thing-at
    markdown-mode
    monokai-theme
    multi-vterm
    move-text
    orderless
    org-bullets
    org-download
    org-preview-html
    page-break-lines
    paren-face
    perspective
    popper
    puni
    python
    quick-peek
    rainbow-delimiters
    restclient
    restclient-jq
    rich-minority
    ripgrep
    rg
    shrink-whitespace
    sml-modeline
    super-save
    symbol-overlay
    term-keys
    toggle-test
    typo
    unfill
    unicode-fonts
    use-package
    vertico
    visible-mark
    vterm
    vterm-toggle
    which-key
    xclip
    zop-to-char))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

    ;; consult-flycheck
    ;; flycheck-clj-kondo
    ;; flycheck-clojure
    ;; flycheck-indicator
    ;; flycheck-inline

(setq load-prefer-newer t)
;; (package-initialize)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; '(consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number . --glob !*.xml")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-window-display-mode t)
 '(auto-dim-other-buffers-mode nil)
 '(auto-revert-interval 2)
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
 '(bqn-key-prefix 92)
 '(bqn-mode-map-prefix "C-M-")
 '(browse-url-browser-function 'browse-url-firefox)
 '(case-fold-search nil)
 '(cider-annotate-completion-function 'my-cider-annotate-completion-function)
 '(cider-comment-prefix " ;=> ")
 '(cider-inspector-auto-select-buffer t)
 '(cider-inspector-page-size 50)
 '(cider-repl-history-file "~/.cider-repl-history")
 '(cider-repl-history-size 1000)
 '(cider-repl-use-clojure-font-lock t)
 '(cider-repl-use-pretty-printing t)
 '(cider-special-mode-truncate-lines nil)
 '(cider-test-defining-forms '("deftest" "defspec" "defcsvtest"))
 '(clojure-defun-indents '(fn-traced))
 '(col-highlight-show-only 'forward-paragraph)
 '(column-number-mode t)
 '(completion-show-help nil)
 '(consult-mode-histories
   '((eshell-mode . eshell-history-ring)
     (comint-mode . comint-input-ring)
     (term-mode . term-input-ring)
     (clojure-mode . clojure-input-ring)
     (elisp-mode . elisp-input-ring)))
 '(consult-preview-max-count 5)
 '(consult-preview-max-size 104857)
 '(consult-preview-raw-size 1024)
 '(corfu-doc-max-height 30)
 '(corfu-doc-max-width 120)
 '(ctrlf-auto-recenter nil)
 '(ctrlf-show-match-count-at-eol t)
 '(custom-safe-themes
   '("207431eaa6aa655a252d3807578bac8187fc0d87ccd2ace5538b1a3c048a5bf8" "a5944520983c5467f163f68d256ff869a338c88acbd3610ffde4ba259c51dc61" "3d38054b096d264c5e8bcf46820b31853989404dc41d3a43b6a071c54b32f7ad" "7d5007882734456bc3df171bb8b0a83962841e2a8a5e9b861fbe2f24b1265ffd" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "39b0c917e910f32f43f7849d07b36a2578370a2d101988ea91292f9087f28470" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(dired-sidebar-icon-scale 1)
 '(dired-sidebar-mode-line-format
   '("%e" mode-line-front-space mode-line-buffer-identification " " mode-line-end-spaces))
 '(dired-sidebar-should-follow-file t)
 '(dired-sidebar-subtree-line-prefix "——")
 '(dired-sidebar-theme 'nerd)
 '(dired-sidebar-use-custom-font t)
 '(dired-sidebar-width 50)
 '(display-line-numbers-major-tick 0)
 '(display-line-numbers-type 'visual)
 '(dynamic-completion-mode nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(eglot-connect-timeout 120)
 '(epresent-hide-properties nil)
 '(epresent-hide-tags nil)
 '(epresent-mode-line '(:eval (int-to-string epresent-page-number)))
 '(epresent-text-scale 300)
 '(fci-rule-color "#383838")
 '(fill-column 78)
 '(flycheck-indicator-icon-error 9632)
 '(flycheck-indicator-icon-info 9679)
 '(flycheck-indicator-icon-warning 9650)
 '(flycheck-indicator-status-icons '((finished . "✓YAY✓") (errored . "XXX ERROR XXX")))
 '(flycheck-markdown-markdownlint-cli-executable "markdownlint")
 '(flycheck-markdown-mdl-executable "mdl")
 '(flycheck-pycheckers-checkers '(pylint pep8 pyflakes bandit))
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag t)
 '(global-prettify-symbols-mode nil)
 '(global-superword-mode t)
 '(global-whitespace-mode t)
 '(highlight-nonselected-windows t)
 '(highlight-parentheses-colors '("red" "IndianRed1"))
 '(highlight-parentheses-delay 0.3)
 '(highlight-parentheses-highlight-adjacent t)
 '(hl-line-flash-show-period 2.0)
 '(hl-todo-keyword-faces
   '(("HOLD" . "#d0bf8f")
     ("TODO" . "#cc9393")
     ("NEXT" . "#dca3a3")
     ("THEM" . "#dc8cc3")
     ("PROG" . "#7cb8bb")
     ("OKAY" . "#7cb8bb")
     ("TESTING" . "#5f7f5f")
     ("DONT" . "#5f7f5f")
     ("FAIL" . "#8c5353")
     ("DONE" . "#afd8af")
     ("NOTE" . "#d0bf8f")
     ("KLUDGE" . "#d0bf8f")
     ("HACK" . "#d0bf8f")
     ("REVIEW" . "#d0bf8f")
     ("REFACTOR" . "#d0bf8f")
     ("FUTURE" . "#d0bf8f")
     ("TEMP" . "#d0bf8f")
     ("FIXME" . "#cc9393")
     ("XXXX*" . "#cc9393")
     ("WARNING" . "orange")
     ("UGLY" . "orange")
     ("SLOW" . "red")
     ("OPTIMIZE" . "red")
     ("BUG" . "red")
     ("HELP" . "red")))
 '(hs-hide-comments-when-hiding-all nil)
 '(icomplete-prospects-height 70)
 '(imenu-list-auto-resize t)
 '(imenu-list-focus-after-activation t)
 '(imenu-list-position 'left)
 '(imenu-list-size 0.1)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen nil)
 '(initial-buffer-choice t)
 '(justl-recipe-width 50)
 '(key-chord-safety-interval-backward 0.0)
 '(key-chord-safety-interval-forward 0.0)
 '(live-completions-columns 'single)
 '(live-completions-mode t)
 '(live-completions-sort-order 'cycle)
 '(magit-log-arguments '("--graph" "--color" "--decorate" "--stat" "-n10"))
 '(magit-revision-insert-related-refs nil)
 '(marginalia-margin-threshold 120 t)
 '(mark-thing-at-mode t)
 '(markdown-header-scaling t)
 '(markdown-wiki-link-search-subdirectories t)
 '(mlscroll-border 20)
 '(mlscroll-minimum-current-width 10)
 '(mlscroll-right-align nil)
 '(mode-line-percent-position nil)
 '(mood-line-show-cursor-point nil)
 '(mood-line-show-encoding-information nil)
 '(moody-mode-line-height 25)
 '(mouse-avoidance-mode 'exile nil (avoid))
 '(multi-vterm-dedicated-window-height 70)
 '(nrepl-sync-request-timeout 30)
 '(nyan-mode t)
 '(org-babel-clojure-backend 'cider)
 '(org-babel-load-languages '((emacs-lisp . t) (clojure . t) (shell . t)))
 '(org-confirm-babel-evaluate nil)
 '(org-return-follows-link t)
 '(package-selected-packages
   '(sml-modeline flymake-kondor puni cider mark-thing-at gpt jet justl just-mode hy-mode consult-eglot eglot rust-mode transient-dwim conventional-changelog term-keys restclient-jq jq-mode xclip windresize bicycle dired-rainbow highlight edebug-inline-result monokai-theme rich-minority keycast org-tree-slide zop-to-char restclient corfu vertico dired-sidebar dirtree highlight-escape-sequences hl-todo org-download epresent super-save unicode-fonts orderless winum auto-package-update use-package project-explorer highlight-numbers alert sonic-pi quick-peek rg consult marginalia embark aggressive-indent dotenv-mode org-bullets org-preview-html github-browse-file envrc direnv perspective helpful popwin git-link imenu-list ibuffer-vc symbol-overlay csv-mode diminish which-key diff-hl git-timemachine qjakey-chord visible-mark move-text ample-theme beacon unfill typo shrink-whitespace ripgrep rainbow-delimiters paren-face page-break-lines markdown-mode magit jump-char highlight-parentheses flymd feature-mode exec-path-from-shell edit-indirect dumb-jump dot-mode crux comment-dwim-2 buffer-move ag ace-window))
 '(page-break-lines-max-width 79)
 '(page-break-lines-modes
   '(emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode clojure-mode))
 '(persp-mode-prefix-key nil)
 '(persp-sort 'access)
 '(persp-suppress-no-prefix-key-warning t)
 '(popper-window-height 20)
 '(quick-peek-add-spacer nil)
 '(quick-peek-position 'above)
 '(recentf-auto-cleanup 300)
 '(recentf-max-menu-items 100)
 '(recentf-max-saved-items 500)
 '(ripgrep-arguments '("--smart-case" "--glob=!*.xml"))
 '(save-completions-retention-time 700)
 '(scroll-bar-mode nil)
 '(search-whitespace-regexp "\"[ \\t\\r\\n]+\"")
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(split-height-threshold 100)
 '(split-width-threshold 30)
 '(standard-indent 2)
 '(symbol-overlay-displayed-window nil)
 '(symbol-overlay-faces
   '(symbol-overlay-face-1 symbol-overlay-face-2 symbol-overlay-face-3 symbol-overlay-face-4 symbol-overlay-face-5 symbol-overlay-face-6 symbol-overlay-face-7 symbol-overlay-face-8))
 '(tldr-enabled-categories '("common"))
 '(tramp-default-method "ssh")
 '(uniquify-min-dir-content 1)
 '(uniquify-trailing-separator-p t)
 '(vertico-count 50)
 '(vertico-cycle nil)
 '(which-key-frame-max-width 100)
 '(which-key-idle-delay 0.4)
 '(which-key-idle-secondary-delay 0.0)
 '(which-key-max-description-length 45)
 '(which-key-popup-type 'side-window)
 '(which-key-show-docstrings t)
 '(which-key-side-window-location 'left)
 '(which-key-side-window-max-height 0.9)
 '(which-key-side-window-max-width 0.9)
 '(which-key-unicode-correction 30)
 '(whitespace-style
   '(face trailing tabs spaces lines-tail empty indentation::tab indentation))
 '(yascroll:delay-to-hide nil)
 '(yascroll:disabled-modes '(image-mode cider-repl-mode vterm-mode))
 '(yascroll:scroll-bar '(text-area right-fringe left-fringe)))

;; DISABLED
;; rainbow-identifiers
;; all-the-icons-ivy-rich
;; cljr-ivy
;; ivy
;; ivy-hydra
;; ivy-posframe
;; ivy-rich
;; counsel
;; ivy
;; ivy-rich
;; ivy-posframe
;; ido-flx
;; ido
;; ido-completing-read+
;; company
;; company-box
;; company-flx
;; company-fuzzy
;; company-jedi
;; company-quickhelp
;; company-prescient
;; company-posframe
;; rubocop

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

;; (when (display-graphic-p)
;;   (set-face-attribute 'default nil :font "Fira Code"
;;                       :height (if (eq system-type 'darwin) 100 )
;;                       :background (if (eq system-type 'darwin) "gray16" "gray5")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "gray2" :foreground "#bdbdb3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 75 :width normal :foundry "UKWN" :family "Iosevka Term"))))
 '(auto-dim-other-buffers-face ((t (:background "gray12"))))
 '(avy-lead-face-0 ((t (:background "RoyalBlue4" :foreground "white"))))
 '(aw-leading-char-face ((t (:foreground "red" :height 5.0))))
 '(bqn-function-face ((t (:foreground "#66D9EF"))))
 '(bqn-list-face ((t (:foreground "#AE81FF"))))
 '(bqn-one-modifier-face ((t (:foreground "dark red"))))
 '(bqn-separator-face ((t (:foreground "#AE81FF"))))
 '(bqn-two-modifier-face ((t (:foreground "hot pink" :weight extra-bold))))
 '(cider-instrumented-face ((t (:foreground "#AE81FF" :background "brightmagenta"))))
 '(cider-repl-result-face ((t (:foreground "cyan"))))
 '(cider-traced-face ((t (:slant italic :background "darkslategray"))))
 '(clojure-keyword-face ((t (:foreground "#ab75c3"))))
 '(col-highlight ((t (:background "RoyalBlue4"))))
 '(ctrlf-highlight-active ((t (:background "yellow" :foreground "black"))))
 '(ctrlf-highlight-line ((t (:background "red4"))))
 '(ctrlf-highlight-passive ((t (:background "orange red" :foreground "black"))))
 '(cursor ((t (:background "red" :foreground "#272822"))))
 '(flycheck-indicator-success ((t (:inherit custom-state))))
 '(flymake-error ((t (:foreground "red" :underline (:color foreground-color :style wave)))))
 '(flymake-warning ((t (:foreground "brightyellow" :underline (:color foreground-color :style wave)))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#75715E"))))
 '(font-lock-comment-face ((t (:foreground "gray40" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#dF9522"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "white" :slant italic :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "green3" :underline t :weight ultra-bold))))
 '(font-lock-type-face ((t (:foreground "#66D9EF" :slant italic :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "green3"))))
 '(highlight ((t (:background "RoyalBlue4"))))
 '(highlight-numbers-number ((t (:foreground "orangered" :weight extra-bold))))
 '(hl-line ((t (:extend t :background "#011a40" :box nil))))
 '(markdown-code-face ((t (:inherit code-face))))
 '(markdown-header-delimiter-face ((t (:inherit markdown-markup-face))))
 '(markdown-header-face ((t (:family "Fira Sans"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :foreground "pale turquoise" :weight bold :height 1.5))))
 '(markdown-header-face-2 ((t (:foreground "#ab75c3" :slant normal :weight bold :height 1.3))))
 '(markdown-header-face-3 ((t (:foreground "#dF9522" :slant italic :weight bold :height 1.05))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :slant italic :height 1.0))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))))
 '(markdown-italic-face ((t (:inherit italic :slant italic))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face))))
 '(minibuffer-prompt ((t (:foreground "#fffe0a" :underline t :slant italic :weight bold :height 1.0))))
 '(mode-line ((t (:background "RoyalBlue3" :foreground "gray75" :inverse-video nil :height 1.0))))
 '(mode-line-inactive ((t (:background "gray22" :foreground "cornsilk4" :inverse-video nil :height 1.0))))
 '(mood-line-status-info ((t (:foreground "purple4"))))
 '(mood-line-status-neutral ((t (:foreground "white"))))
 '(mood-line-unimportant ((t (:foreground "white"))))
 '(org-block ((t (:extend t :background "#3E3D31" :foreground "#F8F8F0" :family "Iosevka Term"))))
 '(org-code ((t (:foreground "#75715E" :family "Fira Code"))))
 '(org-document-title ((t (:inherit (variable-pitch font-lock-constant-face) :weight bold :height 2.0))))
 '(org-level-1 ((t (:inherit (outline-1 variable-pitch) :extend t :foreground "#FD971F" :height 3.0))))
 '(org-level-2 ((t (:inherit (outline-2 variable-pitch) :slant italic :height 1.6))))
 '(org-level-3 ((t (:inherit (outline-3 variable-pitch)))))
 '(org-list-dt ((t (:foreground "DeepSkyBlue" :weight bold))))
 '(outline-1 ((t (:inherit org-level-1 :weight bold))))
 '(page-break-lines ((t (:foreground "#c4bf27"))))
 '(persp-selected-face ((t (:inherit custom-state :weight bold))))
 '(quick-peek-background-face ((t (:inherit default :extend t :background "gray10"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "chartreuse" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep sky blue" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :weight bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "chartreuse" :weight bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "deep sky blue" :weight bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "yellow" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "chartreuse" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "deep sky blue" :weight bold))))
 '(region ((t (:inherit highlight :extend t :background "purple4"))))
 '(show-paren-match ((t (:background "#272822" :foreground "green" :inverse-video t :weight normal))))
 '(sp-show-pair-match-face ((t (:background "#272822" :foreground "green" :inverse-video t :weight normal))))
 '(symbol-overlay-default-face ((t (:inherit nil :background "MediumBlue"))))
 '(symbol-overlay-face-3 ((t (:background "sienna" :foreground "black"))))
 '(symbol-overlay-face-4 ((t (:background "dark orchid" :foreground "black"))))
 '(symbol-overlay-face-6 ((t (:background "orange" :foreground "black"))))
 '(tooltip ((t (:background "red" :foreground "green"))))
 '(variable-pitch ((t (:height 1.0 :family "Fira Sans"))))
 '(vertico-group-title ((t (:background "brightwhite" :foreground "gray40" :slant italic :height 1.6 :family "Fira Sans"))))
 '(visible-mark-face1 ((t (:background "DarkOrange3" :foreground "black"))))
 '(visible-mark-face2 ((t (:background "burlywood4" :foreground "black"))))
 '(which-key-command-description-face ((t nil)))
 '(whitespace-tab ((t (:background "gainsboro" :foreground "#757575" :weight bold))))
 '(yascroll:thumb-fringe ((t (:background "#75715E" :foreground "orange red")))))

;; '(hl-line ((t (:extend t :background "gray12" :box (:line-width 1 :color "black")))))

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

;; https://www.reddit.com/r/emacs/comments/4j828f/comment/d34gbsp (the default is on every 0.76MB)
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

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
(setq key-chord-two-keys-delay .2 ; default is .1
      key-chord-one-key-delay  .3) ; default is .2

;; Very special!
(key-chord-define-global "q'" 'crux-smart-open-line-above)
;; (key-chord-define-global "q'" (lambda () (interactive) (crux-smart-open-line-above) (crux-smart-open-line-above)))

;; Also special (zoom font size)
;; (key-seq-define-global "q-" 'default-text-scale-decrease)
;; (key-seq-define-global "q+" 'default-text-scale-increase)
(key-seq-define-global "Q+" 'text-scale-increase)
(key-seq-define-global "Q_" 'text-scale-decrease)
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
  ;; (define-key my-buffer-keymap "a" 'switch-to-buffer) ; all
  (define-key my-buffer-keymap "a" 'consult-buffer) ; all
  (define-key my-buffer-keymap "A" 'my-ibuffer) ; all
  ;; (define-key my-buffer-keymap "A" 'ibuffer) ; all
  (define-key my-buffer-keymap "b" 'crux-switch-to-previous-buffer) ; default
  (define-key my-buffer-keymap "B" 'my-2back-buffers)
  ;; (define-key my-buffer-keymap "e" 'counsel-buffer-or-recentf)
  (define-key my-buffer-keymap "e" 'consult-recent-file)
  (define-key my-buffer-keymap "f" 'my-copy-filename)
  (define-key my-buffer-keymap "F" 'my-copy-buffername)
  (define-key my-buffer-keymap "N" 'my-copy-namespace-function-lineno)
  (define-key my-buffer-keymap "n" 'my-copy-namespace-name)
  (define-key my-buffer-keymap "p" (lambda () (interactive)
				     (my-ibuffer)
				     (persp-ibuffer nil)))
  ;; (define-key my-buffer-keymap "f" 'counsel-find-file) ; file
  ;; (define-key my-buffer-keymap "f" 'projectile-find-file-dwim) ; file
  ;; (define-key my-buffer-keymap "i" 'counsel-ibuffer) ; ibuffer
  ;; (define-key my-buffer-keymap "i" 'ibuffer) ; ibuffer
  (define-key my-buffer-keymap "i" 'my-ibuffer) ; ibuffer
  ;; (define-key my-buffer-keymap "r" 'counsel-recentf) ; file
  ;; (define-key my-buffer-keymap "p" 'counsel-projectile-switch-to-buffer) ; project
  ;; (define-key my-buffer-keymap "P" 'projectile-ibuffer)
  (key-seq-define-global "'b" my-buffer-keymap))

;; C — Character goto (fast)
;; (key-seq-define-global "'c" 'avy-goto-char-timer) ; character, delay
;; (key-seq-define-global "'c" 'avy-goto-char-2-below)
(key-seq-define-global "'c" 'avy-goto-symbol-1-below)
;; (key-seq-define-global "'f" 'avy-goto-char-2-above)
;; (key-seq-define-global "'f" 'avy-goto-symbol-1-above)
;; (key-seq-define-global "'f" (lambda () (interactive) (avy-goto-symbol-1-above 0 t)))
(key-seq-define-global "'f" 'avy-goto-symbol-1-above)
;; (key-seq-define-global "'c" 'avy-goto-word-0)
;; (key-seq-define-global "'c" 'avy-goto-word-1-above)
;; (key-seq-define-global "xc" 'avy-goto-word-0)
(key-seq-define-global "qc" 'avy-goto-char-timer)
(key-seq-define-global "\"C" 'beacon-blink) ; cursor

;; ;; D — Treemacs (Directory viewer)
;; (key-seq-define-global "'d" 'treemacs-select-window)
;; ;; (key-seq-define-global "\"D" 'treemacs-visit-node-in-most-recently-used-window)
;; (key-seq-define-global "\"D" 'treemacs-find-file)
;; ;; (key-seq-define-global "'d" 'treemacs)
;; ;; (key-seq-define-global "qd" 'treemacs)

;; E — Errors
(let ((my-flymake-keymap (make-sparse-keymap)))
  (define-key my-flymake-keymap "e" ''flymake-goto-next-error) ; default
  (define-key my-flymake-keymap "c" 'flymake-show-diagnostics-buffer)
  (define-key my-flymake-keymap "l" 'consult-flymake)
  (define-key my-flymake-keymap "n" 'flymake-goto-next-error)
  (define-key my-flymake-keymap "p" 'flymake-goto-prev-error)
  (define-key my-flymake-keymap "C" 'flymake-compile)
  (key-seq-define-global "qe" my-flymake-keymap))

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
  (define-key my-git-keymap "B" 'my-copy-branch-name)
  (define-key my-git-keymap "f" 'magit-diff-buffer-file)
  (define-key my-git-keymap "h" 'github-browse-file)
  ;; (define-key my-git-keymap "i" 'git-messenger:popup-message)
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
  ;; (define-key my-kill-keymap "p" 'projectile-kill-buffers)
  (key-seq-define-global "qk" my-kill-keymap))

;; L — Line-numbering/viewing/UI
(let ((my-lines-keymap (make-sparse-keymap)))
  (define-key my-lines-keymap "r" 'display-line-numbers-relative)
  (define-key my-lines-keymap "a" 'display-line-numbers-absolute)
  (define-key my-lines-keymap "s" 'sml-modeline-mode)
  (define-key my-lines-keymap "l" 'display-line-numbers-mode)
  (define-key my-lines-keymap "t" 'toggle-truncate-lines)
  (define-key my-lines-keymap "c" 'crosshairs)
  (define-key my-lines-keymap "H" 'hl-line-flash)
  (define-key my-lines-keymap "b" 'beacon-blink)
  (define-key my-lines-keymap "x" 'crosshairs-mode)
  (define-key my-lines-keymap "m" 'menu-bar-open)
  (define-key my-lines-keymap "h" 'global-hl-line-mode)
  ;; (key-seq-define-global "'l" my-lines-keymap)
  (key-seq-define-global "ql" my-lines-keymap)
  )

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
;; (key-seq-define-global "qo" (lambda () (interactive) (crux-smart-open-line nil) (crux-smart-open-line nil) (forward-line -1) (indent-for-tab-command)))
(key-seq-define-global "Q\"" 'my-clj-open-above-let)

;; P — Project
;; [s]earch and [g]rep are the search keys for ag, ripgrep, projectile variants
(let ((my-project-keymap (make-sparse-keymap)))
  ;; (define-key my-project-keymap "a" 'projectile-add-known-project)
  ;; (define-key my-project-keymap "b" 'counsel-projectile-switch-to-buffer)
  ;; (define-key my-project-keymap "b" 'projectile-switch-to-buffer)
  ;; (define-key my-project-keymap "b" 'project-display-buffer)
  (define-key my-project-keymap "b" 'consult-project-buffer)
  (define-key my-project-keymap "c" 'project-compile)
  (define-key my-project-keymap "d" 'project-find-dir)
  ;; Can't figure out how to bypass orderless with this
  (define-key my-project-keymap "e" 'consult-recent-file)
  ;; (define-key my-project-keymap "e" 'consult-recent-file)
  (define-key my-project-keymap "f" 'project-find-file)
  (define-key my-project-keymap "F" 'project-find-regexp)
  (define-key my-project-keymap "k" 'project-kill-buffers)
  (define-key my-project-keymap "i" (lambda () (interactive) (split-window-balancedly) (persp-ibuffer nil) (message "kill window to quit")))
  ;; (define-key my-project-keymap "i" 'projectile-invalidate-cache)
  ;; (define-key my-project-keymap "o" 'projectile-multi-occur)
  (define-key my-project-keymap "r" 'project-query-replace-regexp)
  (define-key my-project-keymap "R" 'persp-remove-buffer)
  ;; (define-key my-project-keymap "s" 'projectile-ag)
  (define-key my-project-keymap "s" 'consult-ripgrep)
  (define-key my-project-keymap "g" 'consult-grep)
  ;; (define-key my-project-keymap "t" 'projectile-toggle-between-implementation-and-test)
  (define-key my-project-keymap "t" 'tgt-toggle)
  ;; ag in custom specified dir
  ;; (define-key my-project-keymap "S" (lambda () (interactive) (let ((current-prefix-arg 4)) (counsel-ag))))
  ;; ag with options prompt
  ;; (define-key my-project-keymap "G" (lambda () (interactive) (let ((current-prefix-arg 4)) (counsel-projectile-ag))))
  ;; (define-key my-project-keymap "B" 'projectile-ibuffer)
  (define-key my-project-keymap "D" 'project-dired)
  ;; (define-key my-project-keymap "E" 'projectile-edit-dir-locals)
  ;; (define-key my-project-keymap "I" 'ivy-imenu-anywhere)
  (key-seq-define-global "'p" my-project-keymap)
  (key-seq-define-global "qp" my-project-keymap)
  )

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
  ;; (key-seq-define-global "'r" my-cljr-keymap)
  )

;; S — (BAD: possessives like Micah's)

;; T — (BAD: contractions like can't) BUT, qt is ok
(let ((my-todo-keymap (make-sparse-keymap)))
  (define-key my-todo-keymap "n" 'hl-todo-next)
  (define-key my-todo-keymap "p" 'hl-todo-previous)
  (define-key my-todo-keymap "o" 'hl-todo-occur)
  (define-key my-todo-keymap "i" 'hl-todo-insert)
  (key-seq-define-global "qt" my-todo-keymap))

;; U — (BAD: qu combo, but 'u is kinda ok)
(let ((my-url-keymap (make-sparse-keymap)))
  (define-key my-url-keymap "e" 'org-toggle-emphasis)
  (define-key my-url-keymap "i" 'org-insert-link)
  (define-key my-url-keymap "I" 'mardown-insert-link)
  (define-key my-url-keymap "t" 'org-toggle-link-display)
  (define-key my-url-keymap "u" 'browse-url)
  (key-seq-define-global "'u" my-url-keymap))

;; V — Vterm
(let ((my-vterm-keymap (make-sparse-keymap)))
  (define-key my-vterm-keymap "v" 'multi-vterm-dedicated-toggle)
  ;; (define-key my-vterm-keymap "v" 'vterm-toggle)
  ;; (define-key my-vterm-keymap "v" 'vterm-toggle-show)
  (define-key my-vterm-keymap "c" 'vterm-toggle-cd-show)
  (define-key my-vterm-keymap "n" 'my-vterm-new)
  (define-key my-vterm-keymap "o" 'my-vterm-other)
  ;; (key-seq-define-global "'v" my-vterm-keymap)
  )

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
  ;; (define-key my-hs-keymap "c" 'hs-hide-block)
  (define-key my-hs-keymap "c" 'hs-hide-all) ; hide for very Compact view
  ;; (define-key my-hs-keymap "o" 'hs-show-block)
  ;; (define-key my-hs-keymap "O" 'hs-show-all)
  ;; (define-key my-hs-keymap "z" 'hs-toggle-hiding)
;  (define-key my-hs-keymap "f" ')
;  (define-key my-hs-keymap "F" ')
  (define-key my-hs-keymap "g" 'my-hs-hide-functions-all)
  (define-key my-hs-keymap "G" 'hs-show-all)
  ;; (define-key my-hs-keymap "c" 'my-hs-toggle-function)
  ;; (define-key my-hs-keymap "l" 'hs-hide-level)
  ;; (define-key my-hs-keymap "t" 'outline-show-all)
  (define-key my-hs-keymap "s" 'outline-toggle-children)
  (define-key my-hs-keymap "S" 'outline-toggle-children)
  (define-key my-hs-keymap "t" 'outline-hide-body) ; hide all
  (define-key my-hs-keymap "T" 'outline-show-all)
  ;; Show docstrings only; file-level setting :(
  ;; (define-key my-hs-keymap "d" (lambda () (interactive) (set-selective-display 3)))
  ;; (define-key my-hs-keymap "D" (lambda () (interactive) (set-selective-display 0)))
  (key-seq-define-global "'z" my-hs-keymap))

;;; MASHINGS
(key-chord-define-global "AR" 'windmove-left)  ; top-left ring+pinky
;; (key-chord-define-global "ST" 'windmove-right)
(key-chord-define-global "ST" (lambda () (interactive)  (windmove-right)))
(key-chord-define-global "AR" (lambda () (interactive)  (windmove-left)))
(key-chord-define-global "RS" (lambda () (interactive)  (windmove-down)))
(key-chord-define-global "WF" (lambda () (interactive)  (windmove-up)))
(key-chord-define-global "GG" 'my-buf-pivot-right)
(key-chord-define-global "TT" 'my-buf-move-right)
(key-chord-define-global "FF" 'buf-move-up)
(key-chord-define-global "SS" 'buf-move-down)

(key-chord-define-global "RR" 'my-buf-move-left)
(key-chord-define-global "RS" 'windmove-down)
(key-chord-define-global "WF" 'windmove-up)
;; (key-chord-define-global "XC" 'counsel-M-x)
(key-chord-define-global "DV" 'cider-clojuredocs)
(key-chord-define-global "CD" 'clojure-docs-peek-toggle)
;; (key-chord-define-global "XC" 'cider-xref-fn-refs-select)
(key-chord-define-global "XC" 'my-rg-xref)
;; (key-chord-define-global "BG" (lambda () (interactive) (cider-browse-ns (cider-current-ns))))
(key-seq-define-global   "IO" 'delete-other-windows) ; Only One
(key-seq-define-global   "OI" 'winner-undo)
;; (key-chord-define-global "MN" 'winner-undo) ; MaNy

(key-seq-define-global   "H<" 'outline-show-entry)
;; (key-seq-define-global   "<H" (lambda () (interactive) (outline-hide-subtree) (recenter-top-bottom)))
(key-seq-define-global   "HN" 'outline-show-all)
(key-seq-define-global   "NH" 'outline-hide-sublevels)

(key-seq-define-global   "FP" 'hs-show-block)
(key-seq-define-global   "PF" 'my-hs-hide-block)
(key-seq-define-global   "SF" 'my-hs-hide-functions-all)
;; (key-seq-define-global   "SF" 'hs-show-all)
(key-seq-define-global   "FS" (lambda () "Show all" (interactive) (hs-show-all) (recenter-top-bottom)))
(key-seq-define-global   "BF" 'hs-hide-all)
(key-seq-define-global   "FT" 'hs-hide-block) ; fully hide single block; for ns and vars




;; (key-chord-define-global "az" 'delete-window-balancedly)
;; http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(key-chord-define-global "KH" 'kill-this-buffer)
(key-chord-define-global "QW" (lambda () (interactive) (kill-this-buffer) (delete-window-balancedly)))
;; (key-chord-define-global "KH" 'kill-window-balancedly)
(key-chord-define-global "ZX" 'delete-window-balancedly)
;; (key-chord-define-global "ZR" 'kill-window-balancedly)
(key-chord-define-global "GT" 'magit-status)
(key-chord-define-global "XS" 'save-buffer)

(key-chord-define-global "WA" 'cycle-pairs)
(key-chord-define-global "{<" 'cycle-pairs)

;; TODO key seqs for opposited toggle
(key-seq-define-global "PB" 'make-frame-command)
;; (key-seq-define-global "PB" (lambda () (interactive) (make-frame-command) (crux-switch-to-previous-buffer)))
(key-seq-define-global "BP" 'delete-frame)

(key-seq-define-global "JL" 'beacon-blink)
(key-seq-define-global "LJ" 'hl-line-flash)
;; (key-chord-define-global "H<" 'lispy-describe-inline)
(key-chord-define-global "TD" 'my-cider-eval-and-test-fn)
;; (key-chord-define-global "XX" 'my-cider-eval-to-comment)
(key-chord-define-global "AA" 'persp-switch-last)
(key-chord-define-global "BB" 'crux-switch-to-previous-buffer)
;; (key-chord-define-global "VV" 'multi-vterm-dedicated-toggle)
(key-chord-define-global "VV" 'vterm-toggle)

;; (key-chord-define-global "TP" 'dired-sidebar-toggle-sidebar)
(key-seq-define-global "PT" 'dired-sidebar-hide-sidebar)
(key-seq-define-global "TP" 'dired-sidebar-jump-to-sidebar)

(key-chord-define-global "GV" 'set-variable)

;; (key-chord-define-global "]]" 'my-forward-jump-to-line-break)
;; (key-chord-define-global "[[" 'my-backward-jump-to-line-break)


(defun display-line-numbers-absolute ()
  "Absolute line numbers."
  (interactive)
  (set-variable 'display-line-numbers-type 'absolute)
  (display-line-numbers-mode 0)
  (display-line-numbers-mode t))

(defun display-line-numbers-relative ()
  "Visual/relative line numbers."
  (interactive)
  (set-variable 'display-line-numbers-type 'visual)
  (display-line-numbers-mode 0)
  (display-line-numbers-mode t))


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
;; (global-unset-key (kbd "C-z"))
;; (global-set-key (kbd "C-z") 'delete-window-balancedly)

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

;; Zoom font size
;; (global-set-key (kbd "C-M-_") 'default-text-scale-decrease)
;; (global-set-key (kbd "C-M-+") 'default-text-scale-increase)

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

(global-set-key (kbd "C-'") 'toggle-quotes)
;; TEST: Can't "do" this.
;; (global-set-key (kbd "C-c C") 'hide/show-comments-toggle)

(global-set-key (kbd "C-'") 'toggle-quotes)

;; https://stackoverflow.com/a/76869891/326516
(defun cycle-pairs ()
  "Toggle parens, braces, brackets."
  (interactive)
  (save-excursion
    (when (not (string-match-p (regexp-quote (char-to-string (char-after))) "([{<"))
      (backward-up-list)
      (when (eq ?\" (char-after)) ; up again if inside string
        (backward-up-list)))
    (progn
      (cl-case (char-after)
        (?\( (puni-wrap-square))
        (?\[ (puni-wrap-curly))
        (?\{ (puni-wrap-round))
        ;; (?\< "(")
        )
      (forward-char)
      (puni-splice))))

;; https://stackoverflow.com/a/21780995/326516
(global-set-key (kbd "C-S-k") (lambda () (interactive) (delete-region (point) (line-end-position))))


;; Planck-friendly
(global-set-key (kbd "M-{") 'backward-paragraph)
(global-set-key (kbd "M-<") 'forward-paragraph)
(global-set-key (kbd "M->") 'end-of-buffer)
(global-set-key (kbd "M-}") 'beginning-of-buffer)
;; (global-set-key (kbd "C-?") 'undo-tree-redo) ; GUIONLY
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
(global-set-key (kbd "C-S-t") (lambda () (interactive)
				(push-mark (point)) ; save place
                                (let ((current-prefix-arg 0))
				  (call-interactively 'move-to-window-line-top-bottom))))
(global-set-key (kbd "C-S-b") (lambda () (interactive)
				(push-mark (point)) ; save place
				(let ((current-prefix-arg '(-1))) (call-interactively 'move-to-window-line-top-bottom))))

(global-set-key (kbd "M-S-v") 'jump-to-top)
;; Since already holding M-S-R, enable recenter (usually C-l) to also be M-S
(global-set-key (kbd "M-L") 'recenter-top-bottom)
(global-set-key (kbd "M-;") 'comment-dwim-2)

(global-set-key [(meta m)] 'jump-char-forward)
;; (global-set-key [(shift meta m)] 'jump-char-backward)
;; TEST: Can't "do" this.
;; (global-set-key (kbd "C-c T") 'typo-mode)
;; ISSUE: Need to auto-enter typo-mode only while inside strings.
;; (global-set-key (kbd "M-%") 'anzu-query-replace)
;; (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(global-set-key (kbd "C-c '") 'imenu-list-smart-toggle)
;; (global-set-key (kbd "C-'") 'counsel-semantic-or-imenu)
;; (global-set-key (kbd "C-\"") 'popup-imenu)
;; (global-set-key (kbd "C-M-a") 'beginning-of-defun) ; replaced
(global-set-key (kbd "C-c c") 'my-copy-filename)
(global-set-key (kbd "C-x ]") 'my-forward-jump-to-line-break)

(global-set-key (kbd "C-x [") 'my-backward-jump-to-line-break)
(global-set-key (kbd "C-`") 'push-mark-no-activate)
;; (global-set-key (kbd "M-`") 'jump-to-mark)

(global-set-key (kbd "C-n") 'my-next-line)
(global-set-key (kbd "C-p") 'my-previous-line)


(global-set-key (kbd "C-c j") 'justl)
(global-set-key (kbd "C-c J") 'justl-exec-recipe-in-dir)



;;; UI / LOOK-N-FEEL

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
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
;; (size-indication-mode t) ; show size of file in mode-line

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Make mouse invisible (hide)
(mouse-avoidance-mode 1)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " MDE - " (:eval (if (buffer-file-name)
                                                (abbreviate-file-name (buffer-file-name))
                                              "%b"))))


;;; THEME

(require 'monokai-theme)
(enable-theme 'monokai)

;; (require 'ample-theme)
;; (load-theme 'ample t t)
;; (enable-theme 'ample)
;; ;; (load-theme 'ample-flat t t)
;; ;; (load-theme 'ample-light t t)

;; (load-theme 'dracula t)

;; https://seagle0128.github.io/doom-modeline/
;; (require 'doom-modeline)
;; (doom-modeline-mode 1)

;; (set-face-attribute 'mode-line nil :family "Alegreya Sans" :height 75)
;; (set-face-attribute 'mode-line-inactive nil :family "Alegreya Sans" :height 100)
;; (set-face-attribute 'mode-line-inactive nil :background "white")
;; (set-face-attribute 'mode-line  nil :background "orange")
;;
;; (setq flycheck-set-indication-mode 'left-margin)
;; (flycheck-set-indication-mode 'left-fringe)

;; show available keybindings after you start typing
;; Need to decide between discover-my-major, guide-key, plain-old describe-bindings (C-h b)
;; https://emacs.stackexchange.com/questions/
(require 'which-key)
(which-key-mode +1)
;; (which-key-setup-side-window-right-bottom)

;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(scroll-bar-mode -1)
(setq scroll-bar-width 2)

;; SLOW in terminal!! Probably should just keep this uninstalled, though it's nice to have a scrollbar!
;; (require 'yascroll)
;; (global-yascroll-bar-mode 1)

;; https://melpa.org/#/sml-modeline
(require 'sml-modeline)

;; No splash screen
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen nil)
(setq inhibit-splash-screen nil)

;; Newline at end of file
(setq require-final-newline t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;; https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; Goggles: https://github.com/minad/goggles
;; Pulse modified region
;; Goggles highlights the modified region using pulse. Currently the commands undo, yank, kill and delete are supported.
;; deps: pulse
(use-package goggles
  :hook ((prog-mode text-modee) . goggles-mode)
  :config (setq-default goggles-pulse t))

;; show the cursor when moving after big movements in the window
(require 'beacon)
;; (beacon-mode +1)

;; Enable hl-line-flash
(require 'hl-line+) ; local vendor package

(require 'crosshairs)
;; (crosshairs-toggle-when-idle)

;; flash windows when you change to them
;; https://github.com/N-Mi/e-other-window
;; (require 'e-other-window)

;;; Shackle window focus management
;; https://depp.brause.cc/shackle/
;; (shackle-mode)
;; This particularly helps with compile windows (grep results, etc) that
;; pop up but don't get focus.
;; (setq shackle-default-rule '(:select t))
;; OR
;; https://emacs.stackexchange.com/questions/13212/how-to-make-occur-mode-select-the-window-of-buffer-occur
;; (add-hook 'occur-hook (lambda () (switch-to-buffer-other-window "*Occur*")))
;; (define-key occur-mode-map (kbd "C-RET") (lambda () (occur-mode-goto-occurrence-other-window) (occq)))

;; Highlight symbols with keymap-enabled overlays (search, find)
;; https://github.com/wolray/symbol-overlay/
(require 'symbol-overlay)
;; FIXME not taking effect
(symbol-overlay-mode +1)

;; Highlight word matching point without doing anything
;; https://github.com/nonsequitur/idle-highlight-mode/blob/master/idle-highlight-mode.el
;; Disabling since might play badly with org-mode
;; Also screws with visible-mark.
;; Now part of symbol-overlay

;; ;; Highlight at point
;; (require 'highlight-thing)
;; (global-highlight-thing-mode)
;; (setq highlight-thing-delay-seconds 0.9)
;; (setq highlight-thing-exclude-thing-under-point t)

;; Override to not insert boundaries
;; (defun ivy--insert-symbol-boundaries () (undo-boundary))

;; Make it easier to switch to swiper search from overlay
;; (with-eval-after-load 'symbol-overlay (define-key symbol-overlay-map (kbd "s") 'swiper-isearch-thing-at-point))
;; (define-key symbol-overlay-map (kbd "s") 'symbol-overlay-isearch-literally)
;; (define-key symbol-overlay-map (kbd "s") 'ctrlf-forward-symbol-at-point)
;; Try to hack to reset the boundaries
(define-key symbol-overlay-map (kbd "s") (lambda () (interactive) (ctrlf-forward-symbol-at-point) (message "hit RET to remove boundaries")))

;; Nice transient example
;; https://github.com/wolray/symbol-overlay/issues/59

;; (define-transient-command symbol-overlay-transient ()
;;  "Symbol Overlay transient"
;;  ["Symbol Overlay"
;;   ["Overlays"
;;    ("." "Add/Remove at point" symbol-overlay-put)
;;    ("k" "Remove All" symbol-overlay-remove-all)]
;;   ["Move to Symbol"
;;    ("n" "Next" symbol-overlay-switch-forward)
;;    ("p" "Previous" symbol-overlay-switch-backward)]
;;   ["Other"
;;    ("m" "Hightlight symbol-at-point" symbol-overlay-mode)]])
;; (global-set-key (kbd "C->") 'symbol-overlay-transient)


(require 'transient)
(require 'cider-transient)
;; Moved cider-transient to its own repo:
;;

;; (key-chord-define-global "CC" 'cider-transient)
(key-seq-define-global "xc" 'cider-transient)



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


;; https://github.com/purcell/default-text-scale
;; (require 'default-text-scale)
;; (default-text-scale-mode)


;;; IVY, COUNSEL, SWIPER

;; Disabling since obscures search results
;; (require 'ivy-posframe)
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))

;; https://github.com/raxod502/prescient.el
;; (require 'ivy-prescient)
;; (ivy-prescient-mode)

;; (require 'oneonone)
;; (1on1-emacs)


(require 'ripgrep)


;;; Try out Emacs' new project mode
(require 'project)

;; Replace projectile-toggle-between-implementation-and-test
(require 'toggle-test)
(add-to-list 'tgt-projects
	     '((:root-dir "~/work/cc")
               (:src-dirs "src")
               (:test-dirs "test")
               ;; (:test-prefixes <optional list of prefix strings that are added on source file names to get test file names>)
               (:test-suffixes "_test")
	       ))



;;; imenu
;; (require 'imenu-anywhere)
(require 'imenu-list)
;; (require 'popup-imenu)

;; navi-mode (needed by outshine)

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

;; (require 'flycheck)

(require 'flymake)
(add-hook 'prog-mode #'flymake-mode)
(add-hook 'markdown-mode #'flymake-mode) ; FIXME should this be flymd?
;; https://github.com/turbo-cafe/flymake-kondor
(remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
(use-package flymake-kondor :ensure t :hook (clojure-mode . flymake-kondor-setup))

;; (require 'flymake-kondor)

;; (require 'flycheck-yamllint)

;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;; FIXME This might be breaking my own clojure quickpeek for docs
;; Make inline message pretty with quick-peek
;; https://github.com/flycheck/flycheck-inline#configuration
;; (setq flycheck-inline-display-function
;;       (lambda (msg pos err)
;;         (let* ((ov (quick-peek-overlay-ensure-at pos))
;;                (contents (quick-peek-overlay-contents ov)))
;;           (setf (quick-peek-overlay-contents ov)
;;                 (concat contents (when contents "\n") msg))
;;           (quick-peek-update ov)))
;;       flycheck-inline-clear-function #'quick-peek-hide)


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
;; Turn off for markdown since ` is annoyingly changed
;; (add-hook 'text-mode-hook 'typo-mode)


;; In vendor now since tiny?
(require 'toggle-quotes)

;; special treatment of FIXME, TODO, etc

(require 'hl-todo)
(add-hook 'prog-mode-hook 'hl-todo-mode)
(global-hl-todo-mode)
(define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
(define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
(define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)
(define-key hl-todo-mode-map (kbd "C-c i") 'hl-todo-insert)


;; Wow, hide comments!! Just blanks them out.
;; (require 'hide-comnt) ; in vendor/ since not in melpa


;; auto-dim
;; SLOW probably: https://ahmadnazir.github.io/posts/2016-03-02-debugging-slow-emacs/post.html
;; NOTE use this instead of hiwin mode
;; https://github.com/mina86/auto-dim-other-buffers.el
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (when (fboundp 'auto-dim-other-buffers-mode)
;;               (auto-dim-other-buffers-mode t))))

;; BUGGY
;; Line numbers
;; https://github.com/xcodebuild/nlinum-relative
;; Supposedly faster than linum
;; (require 'nlinum-relative) ; SLOW (for high LOC)
;; (global-nlinum-relative-mode 1) ; trying without to see if faster
;; (setq nlinum-relative-redisplay-delay 1)
;; (setq nlinum-relative-offset 0)

(require 'git-gutter)
(git-gutter-mode +1)

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
;; (global-undo-tree-mode)

;; (setq undo-tree-auto-save-history t)
;; autosave the undo-tree history (from prelude)
;; (setq undo-tree-history-directory-alist `((".*" . "~/.emacs.d/undo")))


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

;; Seems the only way to have multiple vterms
;; (when (eq system-type 'gnu/linux) (require 'multi-vterm))

;; One-time setup
;; https://github.com/CyberShadow/term-keys#kitty
;; After generation, this file needs to be manually copied to ~/.config/kitty.conf
;; (with-temp-buffer
;;   (insert (term-keys/kitty-conf))
;;   (write-region (point-min) (point-max) "~/kitty-for-term-keys.conf"))

(when (not (eq system-type 'darwin))
  ;; FIXME Might need to only do this when window-system
  (require 'term-keys-kitty)
  (term-keys-mode t))

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

;; https://stackoverflow.com/a/46219455/326516
(xclip-mode)

;; Make Emacs use the $PATH set up by the user's shell
;; Affects other envvars too.
(require 'exec-path-from-shell)

;; Whitespace removal DWIM key for emacs.
;; maybe use in future, not bound to anything
;; https://github.com/jcpetkovich/shrink-whitespace.el
;; (require 'shrink-whitespace)

;; whitespace-mode config
(require 'whitespace)
(whitespace-mode)
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


;;; EShell






;; (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'clojure-mode-hook #'aggressive-indent-mode)


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

;; ;; Ignore git ignored
;; ;; (treemacs-git-mode 'extended)
;; (with-eval-after-load 'treemacs
;;   (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))




;;; DIRED

;; (load "dired-x")

(use-package dired-rainbow
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    ))


;; https://github.com/jojojames/dired-sidebar
(require 'dired-sidebar)

;; Neotree bindings
;; n next line, p previous line。
;; SPC or RET or TAB Open current item if it is a file. Fold/Unfold current item if it is a directory.
;; U Go up a directory
;; g Refresh
;; A Maximize/Minimize the NeoTree Window
;; H Toggle display hidden files
;; O Recursively open a directory
;; C-c C-n Create a file or create a directory if filename ends with a ‘/’
;; C-c C-d Delete a file or a directory.
;; C-c C-r Rename a file or a directory.
;; C-c C-c Change the root directory.
;; C-c C-p Copy a file or a directory.

(define-key dired-sidebar-mode-map (kbd "<backtab>") 'dired-subtree-cycle)
;; (define-key dired-sidebar-mode-map (kbd "TAB") 'dired-sidebar-subtree-toggle)
;; (define-key dired-sidebar-mode-map (kbd "N") 'dired-subtree-narrow)
;; (define-key dired-sidebar-mode-map (kbd "U") 'dired-subtree-up)
(define-key dired-sidebar-mode-map (kbd "M-u") 'dired-subtree-up)
(define-key dired-sidebar-mode-map (kbd "M-d") 'dired-subtree-down)
(define-key dired-sidebar-mode-map (kbd "M-e") 'dired-subtree-end)
(define-key dired-sidebar-mode-map (kbd "M-b") 'dired-subtree-beginning)
(define-key dired-sidebar-mode-map (kbd "M-n") 'dired-subtree-next-sibling)
(define-key dired-sidebar-mode-map (kbd "M-p") 'dired-subtree-previous-sibling)
(define-key dired-sidebar-mode-map (kbd "M-o") 'dired-subtree-only-this-directory)
(define-key dired-sidebar-mode-map (kbd "M-M") 'dired-subtree-mark-subtree)
(define-key dired-sidebar-mode-map (kbd "M-U") 'dired-subtree-unmark-subtree)





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


(defun my-vterm-push-goto ()
  "Mark current window as recent and jump to vterm window.
Enables jumping back to prior."
  (interactive)
  (if (s-starts-with? "*vterm" (buffer-name))
      (aw-flip-window)
    (aw--push-window (buffer-name))
    (multi-vterm-dedicated-select)))
;; (define-key cider-mode-map (kbd "C-c C-x") nil)
(global-set-key (kbd "C-c C-x") 'my-vterm-push-goto)



(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
    (?m aw-swap-window "Swap Window")
    (?M aw-move-window "Move Window")
    (?s aw-switch-buffer-in-window "Select Buffer")
    (?n aw-flip-window)
    (?\t aw-flip-window)
    (?c aw-split-window-fair "Split Fair Window")
    (?v aw-split-window-vert "Split Vert Window")
    (?h aw-split-window-horz "Split Horz Window")
    (?i delete-other-windows "Delete Other Windows")
    (?o delete-other-windows)
    (?? aw-show-dispatch-help))
  "List of actions for `aw-dispatch-default'.")

;; Colemak
;; (setq aw-keys '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?o))
(setq aw-keys '(?q ?w ?f ?p ?b ?j ?l ?u ?y ?n ?e ?i ?o))
(setq aw-dispatch-always t)
(setq aw-scope 'frame) ; or 'global


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

;; TODO replace balance-windows with this, maybe
;; With imenu open, built-in balance-windows makes imenu take up half of screen
;; https://github.com/emacs-lsp/lsp-ui/issues/543
(defun my-balance-windows ()
  (interactive)
  (imenu-list)
  (window-preserve-size (get-buffer-window imenu-list-buffer-name) t t)
  (balance-windows))

(add-hook 'imenu-list-minor-mode-hook (lambda () (message "in minor hook") (window-preserve-size (get-buffer-window imenu-list-buffer-name) t t)))

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
;;   (add-hook 'after-init-hook (lambda () (persp-mode 1))))

;; This is actually perspective, not persp.
;; https://github.com/nex3/perspective-el
;; Don't want to be saving since not sure how much bad/stale state gets saved/loaded.
;; So easiest to just create new perpectives every time emacs starts.
(persp-mode)
;; Maybe this causes problems??
;; (add-hook 'kill-emacs-hook #'persp-state-save)
;; (setq persp-state-default-file "~/.emacs.d/persp-mde")


;; (ace-window-display-mode t)


(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Shuffle/swap windows around
(require 'buffer-move)
;; https://github.com/bbatsov/prelude/issues/106


(defun my-buf-pivot-right ()
  "Slide the buffer below to a full-height to the right."
  (interactive)
  (let ((next-move (if (window-in-direction 'up)
                       (progn (windmove-up)
                              'windmove-down)
                     (windmove-down)
                     'windmove-up)))
    (split-window-balancedly)
    (crux-switch-to-previous-buffer)
    (funcall next-move)
    (delete-window-balancedly)))

(defun my-buf-move-right ()
  "Slide the buffer below to the right."
  (interactive)
  (windmove-right)
  (split-window-vertically-balancedly)
  (buf-move-left)
  (delete-window-balancedly)
  (windmove-left))

(defun my-buf-move-left ()
  "Slide the buffer below to the left."
  (interactive)
  (windmove-left)
  (split-window-vertically-balancedly)
  (buf-move-right)
  (delete-window-balancedly)
  (windmove-right))

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

(defun my-next-line (arg)
  "Wrap `next-line' to save point mark."
  (interactive "P")
  ;; (message arg)
  (when (and arg (< 4 arg)) (push-mark (point)))
  (next-line arg))

(defun my-previous-line (arg)
  "Wrap `previous-line' to save point mark."
  (interactive "P")
  (when (and arg (< 4 arg)) (push-mark (point)))
  (previous-line arg))

;;; Commenter
(require 'comment-dwim-2)




;;; GIT

;; https://github.com/emacsorphanage/git-messenger
;; (require 'popup)
;; (require 'git-messenger)
(require 'magit)
(require 'git-timemachine)
(require 'diff-hl)
(global-diff-hl-mode)

;; (setq git-identity-default-username "Micah Elliott")
;; (require 'git-identity)
;; ;; (git-identity-magit-mode 1)
;; (define-key magit-status-mode-map (kbd "I") 'git-identity-info)

;; Auto-show magit-process-buffer on fetch
;; https://github.com/magit/magit/issues/4401
;; (add-hook 'magit-credential-hook 'magit-process-buffer)
;; Or always show process buffer
;; https://github.com/magit/magit/wiki/Tips-and-Tricks#automatically-displaying-the-process-buffer
(defun auto-display-magit-process-buffer (&rest args)
  "Automatically display the process buffer when it is updated."
  (let ((magit-display-buffer-noselect t))
    (magit-process-buffer)))
(advice-add 'magit-process-insert-section :before
	    #'auto-display-magit-process-buffer)

;; https://github.com/magit/magit/issues/1878#issuecomment-418763526
;; Enable magit-process to interpret ansi colors
(defun color-buffer (proc &rest args)
  (interactive)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))
(advice-add 'magit-process-filter :after #'color-buffer)

;; https://www.reddit.com/r/emacs/comments/fmd2qo/disabling_globalwhitespacemode_in_magit_buffers/
(defun turn-off-whitespace-mode ()
  "Unconditionally turn off Whitespace mode."
  (whitespace-mode -1))
(add-hook 'magit-section-mode-hook #'turn-off-whitespace-mode)

(defun my-play-long-game ()    (play-sound '(sound :file "~/Music/game-sounds/624878__sonically_sound__old-video-game-4.wav" :volume 80)))
(defun my-play-suntemple ()    (play-sound '(sound :file "~/Music/game-sounds/253177__suntemple__retro-accomplished-sfx.wav" :volume 80)))
(defun my-play-uhoh ()         (play-sound '(sound :file "~/Music/game-sounds/361255__japanyoshithegamer__8-bit-uh-oh-sound.wav" :volume 80)))
(defun my-play-accomplished () (play-sound '(sound :file "~/Music/game-sounds/253177__suntemple__retro-accomplished-sfx.wav" :volume 80)))
(defun my-play-coins ()        (play-sound '(sound :file "~/Music/game-sounds/341695__projectsu012__coins-1.wav" :volume 80)))
(defun my-play-heal ()         (play-sound '(sound :file "~/Music/game-sounds/346116__lulyc__retro-game-heal-sound.wav" :volume 80)))
(defun my-play-laser ()        (play-sound '(sound :file "~/Music/game-sounds/413057__matrixxx__retro_laser_shot_01.wav" :volume 80)))
(defun my-play-coin1 ()        (play-sound '(sound :file "~/Music/game-sounds/415083__harrietniamh__video-game-coin.wav" :volume 80)))
(defun my-play-fx5 ()          (play-sound '(sound :file "~/Music/game-sounds/517756__danlucaz__game-fx-5.wav" :volume 80)))
(defun my-play-jump ()         (play-sound '(sound :file "~/Music/game-sounds/580309__colorscrimsontears__jump-platformer.wav" :volume 80)))
;; (defun my-play- () (play-sound '(sound :file "~/Music/game-sounds/" :volume 80)))

;; Plaing these may interfere with other apps, and can cause emacs to crash, I think
;; (add-hook 'magit-log-mode-hook     #'my-play-laser)
;; (add-hook 'magit-post-stage-hook   #'my-play-coins)
;; (add-hook 'magit-post-unstage-hook #'my-play-heal)
;; (add-hook 'magit-post-commit-hook  #'my-play-long-game)

;; (add-hook 'cider-test-report-mode-hook #'my-play-uhoh)
;; (add-hook 'cider-stacktrace-mode-hook  #'my-play-fx5)
;; (add-hook 'cider-connected-hook        #'my-play-accomplished)
;; (add-hook 'cider-repl-mode-hook        #'my-play-long-game)
;; (add-hook 'cider-disconnected-hook     #'my-play-suntemple)

;; (add-hook 'flycheck-syntax-check-failed-hook #'my-play-uhoh)

;; https://stackoverflow.com/questions/20126575/how-to-make-emacs-not-highlight-trailing-whitespace-in-term
(add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))
;; (add-hook 'magit-process-mode-hook (lambda () (setq show-trailing-whitespace nil)))
;; (add-hook 'magit-process-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))
;; (add-hook 'magit-process-mode-hook (lambda () (whitespace-mode 0)))
;; https://emacs.stackexchange.com/a/38778/11025
(defun prevent-whitespace-mode-for-magit ()
  (not (derived-mode-p 'magit-mode)))
(add-function :before-while whitespace-enable-predicate 'prevent-whitespace-mode-for-magit)


(defun my-magit-status ()
  (interactive)
  (magit-status)
  (balance-windows))

;; https://emacs.stackexchange.com/a/28541/11025
(defun my-git-commit-setup ()
  (message "Setting up magit for canned messaging")
  (let* ((orig (magit-get-current-branch))
	 ;; (orig "mde/SCRUM-12345_this-is-temp")
	 ;; (orig "mde/SCRUM-12345_FEAT-this-is-temp")
	 (bonly (s-replace "mde/" "" orig))
	 ;; FIXME enable this line instead
         (parts (s-split "_" bonly))
	 ;; (parts (s-split "[0-9]+-" bonly))
         (title (s-replace "-" " " (nth 1 parts)))
         (t2 (string-join (subseq (s-split " " title) 1) " "))
         (ctype (downcase (car (s-split " " title))))
         (scrum (car parts))
         )
    ;; (message bonly)
    ;; (message t2)
    ;; (message (car parts))
    ;; (message scrum)
    ;; (message ctype)
    ;; (message title)
    (insert (concat ctype ": (" scrum ") " t2))))
;; Run this inside proj buffer: (my-git-commit-setup)

(add-hook 'git-commit-setup-hook 'my-git-commit-setup)




(require 'conventional-changelog)
(with-eval-after-load 'magit-tag    ;; Integrate to `magit-tag'
  (transient-append-suffix 'magit-tag
    '(1 0 -1)
    '("c" "changelog" conventional-changelog-menu)))

;; Pretty Magit Emoji
;; http://www.modernemacs.com/post/pretty-magit/
(require 'dash)


;; (when window-system
;;   (cl-do

;;       ;; (add-hook 'magit-mode-hook #'emojify-mode) ; needs emojify installed

;;       (defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
;; 	"Replace sanitized WORD with ICON, PROPS and by default add to prompts."
;; 	`(prog1
;; 	     (add-to-list 'pretty-magit-alist
;; 			  (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
;; 				,ICON ',PROPS))
;; 	   (unless ,NO-PROMPT?
;; 	     (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

;;     (setq pretty-magit-alist nil)
;;     (setq pretty-magit-prompt nil)
;;     (pretty-magit "Feature" ?✨ (:foreground "slate gray"))
;;     (pretty-magit "feat" ?✨ (:foreground "slate gray"))
;;     (pretty-magit ":sparkles" ?✨ (:foreground "slate gray"))
;;     (pretty-magit "Add"     ?✨ (:foreground "#375E97"))
;;     ;; (pretty-magit "Merge"     ?🔀 (:foreground "#375E97")) ; no colon
;;     (pretty-magit "Fix"     ?🐛 (:foreground "#FB6542"))
;;     (pretty-magit "fix"     ?🐛 (:foreground "#FB6542"))
;;     (pretty-magit ":bug"    ?🐛 (:foreground "#FB6542"))
;;     (pretty-magit "Clean"   ?👮 (:foreground "#FFBB00"))
;;     (pretty-magit "refactor" ?👮  (:foreground "#FFBB00"))
;;     (pretty-magit ":cop"  ?👮  (:foreground "#00BB00")) ; refactor
;;     (pretty-magit "style"  ?👮  (:foreground "#00BB00"))
;;     (pretty-magit ":wrench"  ?🔧 (:foreground "#FFBB00")) ; ci, tooling, config
;;     (pretty-magit "build"  ?🔧 (:foreground "#FFBB00")) ; ci, tooling, config
;;     (pretty-magit "test" ?✅  (:foreground "#FFBB00"))
;;     (pretty-magit ":check"  ?✅  (:foreground "#FFBB00"))
;;     (pretty-magit "Docs"    ?📚 (:foreground "#3F681C"))
;;     (pretty-magit "docs"    ?📚 (:foreground "#3F681C"))
;;     (pretty-magit ":books"    ?📚 (:foreground "#3F681C"))
;;     (pretty-magit "security"    ?🔒 (:foreground "#3F681C"))
;;     (pretty-magit ":lock"    ?🔒 (:foreground "#3F681C"))
;;     (pretty-magit "master"  ?👑 (:height 1.2) t)
;;     (pretty-magit "origin"  ?💦  (:height 1.2) t)

;;     (defun add-magit-faces ()
;;       "Add face properties and compose symbols for buffer from pretty-magit."
;;       (interactive)
;;       (with-silent-modifications
;; 	(--each pretty-magit-alist
;; 	  (-let (((rgx icon props) it))
;; 	    (save-excursion
;; 	      (goto-char (point-min))
;; 	      (while (search-forward-regexp rgx nil t)
;; 		(compose-region
;; 		 (match-beginning 1) (match-end 1) icon)
;; 		(when props
;; 		  (add-face-text-property
;; 		   (match-beginning 1) (match-end 1) props))))))))

;;     (advice-add 'magit-status :after 'add-magit-faces)
;;     (advice-add 'magit-refresh-buffer :after 'add-magit-faces)
;;     ))


(require 'unicode-fonts)
(unicode-fonts-setup)




;;; REST CLIENTS
(require 'restclient)
(require 'restclient-jq)
;; (require 'httprepl)



;; https://github.com/thierryvolpiatto/zop-to-char
(require 'zop-to-char)
(global-set-key [remap zap-to-char] 'zop-to-char)


;; DISABLED Too annoying that easy-mark loses prior mark
;; Select/highlight with easy-kill
;; https://github.com/leoliu/easy-kill
;; http://stackoverflow.com/a/36631886/326516
;; (require 'easy-kill)
;; (global-set-key (kbd "M-w") 'kill-ring-save)
;; (global-set-key (kbd "C-M-SPC") 'easy-mark)
;; (global-set-key [remap kill-ring-save] 'easy-mark)
;; (global-set-key [remap kill-ring-save] 'easy-mark)

;; (global-set-key [remap mark-sexp] 'easy-mark)
;; (global-set-key [remap kill-ring-save] 'easy-kill)




;;; Hide-Show (V: visible), like folding

;; NOTE: set-selective-display is unsufficient since can only operate
;; at file level, so you can't fold just a single fn.


;; Adds the docstring detection to hs-minor-mode for clojure (not really)
;; (add-to-list 'hs-special-modes-alist '(clojure-mode "(defn [-a-z]+ \"[^\"]+\"" ")" ";; " nil nil))

;; (add-to-list 'hs-special-modes-alist '(clojure-mode "^;;; " "\f" ";;" nil nil))

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
;; (add-hook 'clojure-mode-hook 'hs-clojure-hide-namespace-and-folds)



(add-hook 'clojure-mode-hook (lambda () (local-set-key (kbd "<backtab>") 'outline-toggle-children)))

(setq outline-minor-mode 1)
(outline-minor-mode)




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

;; https://stackoverflow.com/a/20303342/326516
(with-eval-after-load 'dired (define-key dired-mode-map "c" 'find-file))

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
;; (setq avy-keys (string-to-list "rstneioqwfpluyzxcdhbjgmva"))
(setq avy-keys (string-to-list "qwfpluyarstneiozxcdhbjgmvk"))
;; (setq avy-keys (number-sequence ?a ?z))
;; (setq avy-keys (string-to-list "arstgmneiowfpluy"))
;; (setq avy-keys (string-to-list "arstneio"))
;; only search in current window
(setq avy-all-windows nil)
;; make case-sensitive
(setq avy-case-fold-search t)

;; (setq avy-orders-alist '((avy-goto-char-2-above . avy-order-closest)
;; 			 (avy-goto-char-2-below . avy-order-closest)))


(global-set-key (kbd "C-S-n") 'my-goto-line-and-char)
(defun my-goto-line-and-char (arg char)
  (interactive "P\ncwhatchar: ")
  ;; (interactive (list (read-char "char: " t)))
  ;; (avy-goto-char-in-line "a")
  ;; (message (string char))
  ;; (message (string arg))
  (if (and arg (< 4 arg))
      (progn
	(push-mark (point))
	(next-line arg)
	(avy-with avy-goto-char
	  (avy-jump
	   (regexp-quote (string char))
	   :beg (line-beginning-position) :end (line-end-position)
	   )))
    (next-line arg)))



(global-set-key (kbd "C-c C-u") 'browse-url)

(setq browse-url-firefox-program (if (eq system-type 'darwin)
				     "/Applications/Firefox.app/Contents/MacOS/firefox"
				   "/usr/bin/firefox"))




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
;; (show-smartparens-global-mode t)
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


;;; Paredit family
(electric-pair-mode 1)
(require 'puni)
(puni-global-mode)

;; puni defines a few of its own and I don't like them
;; https://stackoverflow.com/a/46868395
(eval-after-load "puni"
    '(progn
       (define-key puni-mode-map (kbd "C-M-a") 'my-beginning-of-defun)
       (define-key puni-mode-map (kbd "C-M-e") 'my-end-of-defun)))
(global-set-key (kbd "C-M-a") 'my-beginning-of-defun)
(global-set-key (kbd "C-M-e") 'my-end-of-defun)
;; (add-hook 'term-mode-hook #'puni-disable-puni-mode)
;; (define-key puni-mode-map (kbd "M-(") 'puni-wrap-round)
(define-key puni-mode-map (kbd "M-s") 'puni-splice)
(define-key puni-mode-map (kbd "M-r") 'puni-raise)
(global-unset-key (kbd "M-X"))
(define-key puni-mode-map (kbd "M-X") 'puni-squeeze)
(define-key puni-mode-map (kbd "C-S-K") 'puni-squeeze)
(define-key puni-mode-map (kbd "M-S") 'puni-split)
;; https://github.com/AmaiKinono/puni/issues/52
;; (define-key puni-mode-map (kbd "M""-S") 'puni-join)
(define-key puni-mode-map (kbd "C-S-}") 'puni-slurp-forward)
(define-key puni-mode-map (kbd "C-S-)") 'puni-barf-forward)
(define-key puni-mode-map (kbd "C-S-{") 'puni-slurp-backward)
(define-key puni-mode-map (kbd "C-S-(") 'puni-barf-backward)

(define-key puni-mode-map (kbd "C-M-n") 'puni-end-of-sexp)
(define-key puni-mode-map (kbd "C-M-p") 'puni-beginning-of-sexp)
;; (global-set-key (kbd "M-f") 'forward-word)

(define-key puni-mode-map (kbd "C-M-SPC") 'puni-mark-sexp-at-point)
;; (define-key puni-mode-map (kbd "M-(") 'my-puni-wrap-round)
(define-key puni-mode-map (kbd "M-(") 'puni-wrap-round)
;; (define-key my-keys-minor-mode-map (kbd "C-M-SPC") 'puni-mark-sexp-at-point)

;; Minor mode for personal overrides
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-M-@"))
(global-unset-key (kbd "M-@"))
;; https://github.com/plandes/mark-thing-at
(require 'mark-thing-at)

(defun my-puni-wrap-round ()
  "Go to beginning of word, then wrap-round."
  (interactive)
  (if current-prefix-arg
      (progn
        (thing-at-point--beginning-of-symbol)
        (let ((current-prefix-arg 4))
          (call-interactively 'puni-wrap-round)))
    (thing-at-point--beginning-of-symbol)
    ;; (puni-backward-sexp)
    (puni-wrap-round))
  )

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-h") 'mark-paragraph)
    (define-key map (kbd "C-M-@") 'mark-sexp-thing)
    ;; (define-key my-keys-minor-mode-map (kbd "C-M-@") 'mark-sexp-thing)
    (define-key map (kbd "M-@") 'mark-word-thing)
    (define-key map (kbd "C-M-S-SPC") 'mark-line)
    ;; (define-key my-keys-minor-mode-map (kbd "C-M-S-SPC") 'mark-line)
    map)
  "Custom my-keys-minor-mode keymap.")
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")
(my-keys-minor-mode 1)



;; TODO: why are these 3 set?
;; (setq sp-base-key-bindings 'paredit)
;; (setq sp-autoskip-closing-pair 'always)
;; (setq sp-hybrid-kill-entire-symbol nil)
;; (require 'smartparens)  ; better paredit, sp-*
;; (require 'smartparens-config)
;; (sp-use-paredit-bindings)
;; Enable all the goodies.
;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;; (smartparens-global-mode t)

;; https://emacs.stackexchange.com/questions/7832/how-to-bind-c-for-real
;; (define-key input-decode-map (kbd "C-[") [control-bracketleft])
;; (define-key smartparens-mode-map (kbd "C-]") 'sp-forward-barf-sexp)
;; (define-key smartparens-mode-map [control-bracketleft] 'sp-backward-barf-sexp)

;; ;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
;; (define-key smartparens-mode-map (kbd "C-S-SPC") 'sp-forward-barf-sexp)
;; (define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
;; (define-key smartparens-mode-map (kbd "C-S-_") 'sp-backward-barf-sexp)
;; (define-key smartparens-mode-map (kbd "C-(") 'sp-backward-slurp-sexp)

;; Remove unneeded bindings: https://emacs.stackexchange.com/a/54651/11025
(define-key smartparens-mode-map (kbd "M-`") nil)
;; (define-key smartparens-mode-map [remap kill-line] 'my-homemade-kill-line)

;; (show-smartparens-global-mode +1)
;; (setq sp-override-key-bindings '(("C-<right>" . nil)))

;; saveplace remembers your location in a file when saving files
(defvar root-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar savefile-dir (expand-file-name "savefile" root-dir)
  "This folder stores all the automatically generated save/history-files.")
(setq save-place-file (expand-file-name "saveplace" savefile-dir))
(save-place-mode 1)


;; save recent files
(require 'recentf)
(recentf-mode t)
(setq
 ;; recentf-save-file (expand-file-name "recentf" savefile-dir)
 recentf-max-saved-items 500
 recentf-max-menu-items 70
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



;;; COLORS / THEMES

;;https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(require 're-builder)
(setq reb-re-syntax 'read)

;; Special sectional comments
;; https://emacs.stackexchange.com/questions/28232/syntax-highlighting-for-comments-starting-with-specific-sequence-of-characters
(defface special-comment '((t (:foreground "#c4bf27" :weight ultra-bold))) "My Special Comment" :group 'clojure-mode)
(defface boolean-true '((t (:foreground "green" :weight bold))) "Boolean true" :group 'clojure-mode)
(defface boolean-false '((t (:foreground "red" :weight bold))) "Boolean true" :group 'clojure-mode)
(defface sfdc-field      '((t (:foreground "#dF9522" :weight ultra-bold))) "My SFDC Field" :group 'clojure-mode)
(defface sfdc-record '((t (:foreground "#dF9522" :weight normal))) "SFDC Record" :group 'clojure-mode)
(defface route-path '((t (:foreground "#4287f5" :weight ultra-bold))) "URL Route" :group 'clojure-mode)
(defface sql-field       '((t (:foreground "#528fd1" :weight ultra-bold))) "My SQL Field" :group 'clojure-mode)
;; Play with setting dynamically
;; (face-spec-set 'special-comment '((t :foreground "#ff00ff" :underline t :slant normal)))
;; (face-spec-set 'sfdc-field '((t :foreground "#ff00ff" :underline t :slant normal)))
;; (face-spec-set 'sql-field '((t :foreground "#ff00ff" :underline t :slant normal)))
(font-lock-add-keywords 'clojure-mode '((";;;.*" 0 'special-comment t)))
(font-lock-add-keywords 'clojure-mode '(("\\btrue\\b" 0 'boolean-true t)))
(font-lock-add-keywords 'clojure-mode '(("\\bfalse\\b" 0 'boolean-false t)))
(font-lock-add-keywords 'clojure-mode '(("\\w[A-z0-9_]+__c" 0 'sfdc-field t)))
(font-lock-add-keywords 'clojure-mode '(("\\w[A-z0-9_]+__r" 0 'sfdc-record t)))
(font-lock-add-keywords 'clojure-mode '(("\\\"/[-:a-z0-9/]+\\\"" 0 'route-path t)))
(font-lock-add-keywords 'clojure-mode '(("\\(SELECT\\|FROM\\|WHERE\\|NULL\\|FALSE\\|AND\\|LIKE\\|TRUE\\|ASC\\|DESC\\|DELETE\\|GROUP\\|ORDER\\|NOT\\|NOT IN\\|JOIN\\|BY\\|ON\\|TYPEOF\\|END\\|USING\\|WITH\\|SCOPE\\|DATA\\|CATEGORY\\|HAVING\\|LIMIT\\|OFFSET\\|FOR\\|VIEW\\|REFERENCE\\|UPDATE\\|SET\\|NULLS\\|FIRST\\|LAST\\)" 0 'sql-field t)))

(font-lock-add-keywords 'sql-mode '(("-- :doc .*" 0 'sfdc-record t)))
(font-lock-add-keywords 'sql-mode '(("-- :name [^:]+" 0 'special-comment t)))
(font-lock-add-keywords 'sql-mode '((" \\(:\\*\\|:!\\|:n\\|:\\?\\|:1\\)" 0 'boolean-true t)))
(font-lock-add-keywords 'sql-mode '((" :\\(v\\*:\\)?[-a-z0-9?]+"  0 'sql-field t)))

;; https://emacs.stackexchange.com/questions/2508/highlight-n-and-s-inside-strings
(defface my-backslash-escape-backslash-face
  '((t :inherit font-lock-regexp-grouping-backslash))
  "Face for the back-slash component of a back-slash escape."
  :group 'font-lock-faces)
(defface my-backslash-escape-char-face
  '((t :inherit font-lock-regexp-grouping-construct))
  "Face for the charcter component of a back-slash escape."
  :group 'font-lock-faces)
(defface my-format-code-format-face
  '((t :inherit font-lock-regexp-grouping-backslash))
  "Face for the % component of a printf format code."
  :group 'font-lock-faces)
(defface my-format-code-directive-face
  '((t :inherit font-lock-regexp-grouping-construct))
  "Face for the directive component of a printf format code."
  :group 'font-lock-faces)
(font-lock-add-keywords 'clojure-mode
			'(("\\(\\\\\\)." 1 'my-backslash-escape-backslash-face prepend)
			  ("\\\\\\(.\\)" 1 'my-backslash-escape-char-face      prepend)
			  ("\\(%\\)."    1 'my-format-code-format-face         prepend)
			  ("%\\(.\\)"    1 'my-format-code-directive-face      prepend)))

(require 'highlight-escape-sequences) ; already has clojure support: customize `hes-mode'

;; (defface my-identifier-x '((t :foreground "red" :weight bold)) "face for user defined variables." :group 'my-mode )
;; (face-spec-set 'my-identifier-x '((t :foreground "blue" :weight bold)) 'face-defface-spec)
;; (face-spec-set 'sql-field `((t :foreground "blue")) 'face-defface-spec)

;; (re-search-forward "[A-z_]+__c")
;; Aaa_Vvv__c


;;; Sonic PI



;;; LANGUAGES

;;; Ruby

;; (prelude-require-package 'chruby)
;; (require 'rubocop)
;; (add-hook 'ruby-mode-hook 'rubocop-mode)


;;; Markdown

(require 'flymd)
;; (require 'markdown-toc)
;; Enable syntax highlighting of code in blocks.
(setq markdown-fontify-code-blocks-natively t)

;; Seems to be best version markdownlint; see also mdl
;; https://github.com/igorshubovych/markdownlint-cli


;; (require 'markdown-mode)
;; (eval-after-load 'markdown-mode (define-key markdown-mode-map (kbd "C-RET") 'markdown-follow-thing-at-point))
;; FIXME not working
(add-hook 'markdown-mode-hook (lambda () (define-key markdown-mode-map (kbd "C-RET") 'markdown-follow-thing-at-point)))


;; Gherkin/Cucumber
;; (require 'feature-mode)
;; Just for emacs testing
;; (prelude-require-package 'ecukes)
;; (require 'cucumber-goto-step)

;; Have "pickle" installed but maybe not using it.
;; Comment in package: If you just want syntax highlighting in a lightweight mode, use this.


;; Database UI
;; (require 'edbi)

;; (require 'popup)

;; https://emacs.stackexchange.com/a/2779/11025
(defun my-describe-function-at-point ()
  (interactive)
  (describe-function (function-called-at-point)))
;; (key-chord-define-global "CD" 'my-describe-function-at-point)
;; TODO bind to just elisp mode



;;; ELISP

;; (add-hook 'inferior-emacs-lisp-mode-hook (lambda () ))
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'helpful-at-point)
(add-hook 'ielm-mode-hook (lambda () (define-key ielm-map (kbd "C-c C-d") 'helpful-at-point)))

(define-key emacs-lisp-mode-map (kbd "C-c C-k") (lambda () (interactive)
						  (message (concat "Loading " (buffer-name)))
						  (eval-buffer)))

(use-package edebug-inline-result
  :ensure t
  :defer t
  :custom (edebug-inline-result-backend 'quick-peek)
  :hook (edebug-mode . edebug-inline-result-mode))


;;; Clojure

;; My first elisp package!
(when (not (eq system-type 'darwin))
  (require 'clojure-docs-peek))

;; For controlling clj tests from CSV/TSV
;; TODO Turn into proper fns/lib
;; (cider-test-execute "crawlingchaos.domain.installer.installer-api.solarsystemvalidations.max-solar-cost-per-watt-test" nil nil nil)
;; (cider-load-buffer (get-buffer "solarsystemvalidations/max_solar_cost_per_watt_test.clj"))

;; Still need to highleight and press TAB to make work.
(setq clojure-align-forms-automatically t)

;; NOTE: also installed to ~/.lein/profiles.clj: kibit, eastwood
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(require 'cider)
;; (setq cider-repl-history-file "~/.clojure_history")
;; (require 'cider-eval-sexp-fu) ; breaks elpy
;; (require 'clj-refactor)
;; (require 'clojure-snippets) ; yas for clojure
;; (require 'flycheck-clojure)
;; (require 'kibit-helper)
;; (require 'sotclojure)
;; (speed-of-thought-mode)
;; (require 'clojure-mode-extra-font-locking)

;; Disable syntax highlighting and line-numbering in repl
(add-hook 'cider-repl-mode-hook (lambda ()
                                  (sml-modeline-mode 0)
				  (font-lock-mode 0)))

;;; Nofitications
;; https://github.com/jwiegley/alert
;; (setq alert-default-style 'notifier)
(if (eq system-type 'darwin)
    (setq alert-default-style 'mode-line)
  (setq alert-default-style 'libnotify))
;; (alert "Start mlscroll manually" :severity 'low :title "NOTICE:")

;; (setq hs-special-modes-alist)


;; (require 'winnow)


;; For kondo: https://github.com/borkdude/flycheck-clj-kondo#multiple-linters
;; (require 'flycheck-clj-kondo)
;; (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
;;   (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
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


(defun my-cider-eval-db-hugs ()
  "Re-evaluate crawlingchaos.db.core file."
  (interactive)
  (save-buffer)
  (let* ((proot (consult--project-root)) ; "/home/mde/work/cc/"
	 (dbpath (concat proot "/cc-base/src/main/clojure/crawlingchaos/db/core.clj")))
    (cider-map-repls :auto
      (lambda (repl)
	(cider-request:load-file (cider--file-string dbpath) dbpath "core.db" repl nil)))))

;; Add - for sql mode words
;; (add-hook 'sql-mode-hook     (lambda () (modify-syntax-entry ?- "w" sql-mode-syntax-table)))  ; not working
(add-hook 'sql-mode-hook (lambda ()
                           (modify-syntax-entry ?- "w" sql-mode-syntax-table) ; add - for sql mode words
                           (local-set-key (kbd "C-c C-k") 'my-cider-eval-db-hugs)))
;; https://stackoverflow.com/questions/9818307/emacs-mode-specific-custom-key-bindings-local-set-key-vs-define-key
;; (eval-after-load "sql-mode" (lambda () (local-set-key (kbd "C-c C-k") 'my-cider-eval-db-hugs)))


(defun my-cider-load-route-handler ()
  "Reload handler NS, and send restart-limited to REPL."
  (interactive)
  (save-buffer)
  (let* ((fname (file-name-base)) ; some_route
	 (proot (consult--project-root)) ; "/home/mde/work/cc/"
	 (handlerpath (concat proot "/cc-base/src/main/clojure/crawlingchaos/handler.clj")))
    ;; Check current file name for "route"
    (when (string-match-p "route" fname)
      ;; Re-eval handler NS
      (cider-map-repls :auto
	(lambda (repl)
	  (cider-request:load-file (cider--file-string handlerpath) handlerpath "ignored??" repl nil)))
      ;; Send to REPL a restart-limited so new routes are loaded
      (cider-interactive-eval "(user/restart-limited)")))
  (cider-load-buffer))  ; C-c C-k
(add-hook 'clojure-mode-hook (lambda () (local-set-key (kbd "C-c K") 'my-cider-load-route-handler)))



;; https://gist.github.com/plexus/5418819323afb892b481816745be15e0
;; Rename clj/cljs/cljc buffers to their namespace name, so you see
;; `foo.bar.core' in the modeline, rather than `core.clj'
;; (advice-add 'rename-buffer :around #'plexus/clj-ns--rename-buffer-advice)
;; Killed since not working.


(defun my-jump-to-hugsql-defn ()
  "Jump to HugSQL db symbol at point, through grep result buffer.
Relies on consult (for project-root), cider."
  (interactive)
  ;; https://emacs.stackexchange.com/questions/28367/get-word-at-point
  (let ((sym (thing-at-point 'symbol 'no-properties))) ; ex: "db/get-project-root-doc-pkgs-by-id-2"
    (if (s-starts-with? "db/" sym)
	;; Match the pattern like: ":name get-project-root\b" (use word boundary)
	(let* ((symstr (concat ":name " (string-trim sym "db/") "\\b "))
	       (sqldir (concat (consult--project-root) "resources/sql/")))
	  ;; (rgrep "get-project-root-doc-pkgs-by-id-2" "*.sql" "~/work/cc/resources/sql/")
	  ;; FIXME Seems this has to be run once interactively to work in function
          (rgrep symstr "*.sql" sqldir nil) ; ex: "~/work/cc/resources/sql/"
	  ;; rgrep seems to need a little time to prep/open buffer
	  (sit-for 0.2)
	  ;; Go to new grep seearch result buffer and visit first (only) result
	  (select-window (get-buffer-window "*grep*"))
	  (compilation-next-error 1)
	  (compile-goto-error)
	  (recenter-top-bottom)
	  ;; Close the grep buffer
	  (select-window (get-buffer-window "*grep*"))
	  ;; (kill-this-buffer)
	  (delete-window)
	  (balance-windows))
      ;; Not a db/ symbol so fall back to normal
      (cider-find-var))))
(define-key cider-mode-map (kbd "M-.") 'my-jump-to-hugsql-defn)
;; (define-key cider-mode-map (kbd "M-.") 'cider-find-var) ; orig

(defun my-cider-eval-and-test-fn ()
  "Quickly eval and run test."
  (interactive)
  (cider-eval-defun-at-point)
  (cider-test-run-test))

(global-set-key (kbd "M-h") 'mark-paragraph)

;; (s-replace-regexp "^::*" "" ":::foo-bar")
(defun my-rg-xref ()
  "Like xref but remove leading colons and NS."
  (interactive)
  (let* ((word (thing-at-point 'word))
	 (word (s-replace-regexp "^::*" "" word))
	 (word (s-replace-regexp ".*/" "" word)))
    (consult-ripgrep nil (concat word "#src"))))

;; Convert EDN, JSON, Transit, YAML
;; Good transient examples in this code.
;; https://github.com/ericdallo/jet.el
(require 'jet)

;; https://github.com/clojure-emacs/clj-refactor.el
(defun my-clojure-mode-hook () "Foo bar."
       (message "in my-clojure-mode-hook")
       ;; (clj-refactor-mode 1)
       ;; (yas-minor-mode 1) ; for adding require/use/import statements
       ;; This choice of keybinding leaves cider-macroexpand-1 unbound
       (define-key clojure-mode-map (kbd "C-c C-k") 'my-cider-load-buffer)
       (define-key clojure-mode-map (kbd "C-c C-S-p") 'cider-inspect-last-result)
       (define-key clojure-mode-map (kbd "C-c M-b") 'cider-browse-instrumented-defs)
       (setq-local cider-repl-pop-to-buffer-on-connect 'display-only)
       (setq-local cider-repl-result-prefix ";; => ")
       (setq-local cider-save-file-on-load t)
       (setq-local cider-prompt-for-symbol nil)
       (setq-local cider-repl-wrap-history t)
       (setq-local cider-repl-history-size 1000)
       (setq-local cider-repl-history-file "~/.cider-repl-history")

       (key-chord-define clojure-mode-map "XC" 'my-rg-xref)
       ;; (key-chord-define sql-mode-map "XC" 'my-rg-xref)

       ;; (cider-auto-test-mode 1)
       ;; (cljr-add-keybindings-with-prefix "C-c r")
       ;; (cljr-add-keybindings-with-prefix "C-S-r")
       ;; (key-chord-define-global "'r" 'cljr-add-keybindings-with-prefix)
       ;; (key-chord-define-global "qr" 'cljr-add-keybindings-with-prefix)
       ;; (key-chord-define clojure-mode-map "qr" 'cljr-ivy)
       ;; (cljr-add-keybindings-with-prefix "C-c m")
       ;; (define-key clojure-mode-map (kbd "C-c C-r") 'cljr-ivy)
       ;; (global-set-key (kbd "C-S-T") 'cider-test-commands-map)
       ;; Disable flycheck next error in favor of Cider
       (define-key clojure-mode-map (kbd "C-c C-n") 'cider-ns-map)
       ;; (define-key clojure-mode-map (kbd "C-c C-d C-r") 'clojure-essential-ref)
       ;; (define-key clojure-mode-map (kbd "C-c C-") 'cider-read-and-eval-defun-at-point)
       (global-unset-key (kbd "C-c C-p"))
       (define-key clojure-mode-map (kbd "C-c C-p") 'cider-inspect)
       ;; (define-key (kbd "C-c r"))
       ;; (define-key clojure-mode-map (kbd "M-J") 'sp-join-sexp) ; maybe already done by smartparens
       ;; Make similar to wrapping with M-(
       ;; COMPAT: no go in termenal
       ;; (define-key clojure-mode-map (kbd "M-[") (lambda () (interactive) (sp-wrap-with-pair "[")))
       ;; Overrides tmm-menubar
       ;; (define-key clojure-mode-map (kbd "M-`") (lambda () (interactive) (sp-wrap-with-pair "`")))
       ;; FIXME
       ;; (setq-local completion-styles '(orderless cider))
       (setq-local completion-styles '(orderless))
       (symbol-overlay-mode +1)
       (setq completion-styles '(orderless basic))
       (git-gutter-mode +1)

       ;; Magic order needed to enable kondo
       (flymake-kondor-setup)
       (flymake-mode-on)

       )

;; (add-hook 'justl-mode-hook (lambda () (local-set-key (kbd "C-c C-j") 'justl)))


(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)

(eval-after-load "clojure-mode"
  '(progn
     ;; (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)
     (message "MDE: in clojure eval-after-load")))

;; Remove : from word, no longer a constituent; for symbol-overlay
(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?: ".")))
;; (add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?/ "w")))
;; (modify-syntax-entry ?: "." clojure-mode-syntax-table)

;; https://github.com/clojure-emacs/squiggly-clojure
;; (eval-after-load 'flycheck '(flycheck-clojure-setup))

;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; http://stackoverflow.com/questions/23766483/emacs-cider-clojure-auto-complete-how-to-get-the-docstring
;; (setq ac-delay 0.1)
;; (setq ac-quick-help-delay 0.1)

;; Refactor Menu: https://github.com/maio/discover-clj-refactor.el
;; https://github.com/maio/discover-clj-refactor.el
;; (require 'discover-clj-refactor) ; C-c j

(defun my-minibuffer-setup-hook () "Foo bar."
       (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(defun my-clj-open-above-let ()
  "Open a line above while inside a let's top line."
  (interactive)
  (beginning-of-line)
  (down-list)
  (down-list)
  (newline)
  (indent-for-tab-command)
  ;; (company-indent-or-complete-common t)
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
  ;; (add-to-list 'company-backends 'company-jedi)
  )

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
          (lambda ()
            (define-key org-mode-map [(control tab)] nil)
            (define-key org-mode-map (kbd "C-M-u") 'org-up-element)
            (define-key org-mode-map (kbd "C-M-d") 'org-down-element)
            (define-key org-mode-map (kbd "C-M-f") 'org-forward-element)
            (define-key org-mode-map (kbd "C-M-b") 'org-backward-element)
            (define-key org-mode-map (kbd "C-C C-x l") 'org-toggle-link-display)
            (define-key org-mode-map (kbd "M-}") 'beginning-of-buffer)

	    ;; https://www.emacswiki.org/emacs/FacesPerBuffer
            (face-remap-add-relative 'default :family "Fira Sans")
	    (buffer-face-mode)
	    ))

(add-hook 'epresent-mode-hook (lambda ()
				(face-remap-add-relative 'default :family "Fira Sans")
				(buffer-face-mode)))

(require 'org-preview-html)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Insert datestamp, useful outside of org-mode.
(global-set-key (kbd "C-c D") 'org-date-from-calendar)



;;; Folding, hide-show, outline

(add-hook 'prog-mode-hook 'hs-minor-mode)

(defun my-beginning-of-defun ()
  "Jump to start of name of function, since often want to search it."
  (interactive)
  (beginning-of-line)
  (when (beginning-of-defun) ; check that not at first fn in file
    (forward-word 1)
    (forward-char 1)
    (if (looking-at "[-<>a-z0-9?!]")
        t
      (if (looking-at "[\\^:{]")
	  (my/forward-word-begin 1)
	(backward-word)))))

(defun my-end-of-defun ()
  "Jump to start of name of function, since often want to search it."
  (interactive)
  (end-of-defun)
  (end-of-defun)
  (my-beginning-of-defun))


(defun my-hs-hide-block ()
  "Hide just this block, keeping docstring."
  (interactive)
  (beginning-of-defun)
  (forward-word)
  (hs-hide-level 1))

;; Not working
(defun my-hs-toggle-function ()
  "Hide/show one fn."
  (interactive)
  (save-excursion
    (end-of-defun)
    (backward-word 3)
    (if (hs-already-hidden-p)
	(hs-show-block)
      (beginning-of-defun)
      (hs-hide-level 1))))

;; (defun my-hs-hide-functions-all ()
;;   "Hide all fn levels."
;;   (interactive)
;;   (save-excursion
;;     (end-of-buffer)
;;     (backward-char)
;;     (while (progn (forward-char)
;; 		  (if (thing-at-point-looking-at "def-* ")
;; 		      (hs-hide-block)
;; 		    (hs-hide-level 1))
;; 		  (backward-char)
;; 		  (beginning-of-defun)))
;;     (recenter-top-bottom)))

(defun my-hs-hide-functions-all ()
  "Hide all fn levels, but fully hide vars and deprecations."
  (interactive)
  (save-excursion
    (end-of-buffer)
    (backward-char)
    (while (progn (forward-char)
		  (if (or (thing-at-point-looking-at "def-* ")
			  (s-match "deprecated" (thing-at-point 'line)))
		      (hs-hide-block)
		    (hs-hide-level 1))
		  (backward-char)
		  (beginning-of-defun)))
    (recenter-top-bottom)))

;; (defun my-hs-hide-functions-all ()
;;   "Hide all fn levels."
;;   (interactive)
;;   (save-excursion
;;     (end-of-buffer)
;;     (while (progn (hs-hide-level 1)
;; 		  (beginning-of-defun)))))



;; Copy filename to clipboard
;; https://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun my-copy-filename ()
  "Copy current buffer file name to clipboard."
  (interactive)
  (let ((fname (buffer-file-name)))
    (kill-new fname)
    (message "Copied buffer file name '%s' to the clipboard." fname)))

(defun my-copy-buffername ()
  (interactive)
  (let ((bname (buffer-name)))
    (kill-new bname)
    (message "Copied buffer name '%s' to the clipboard." bname)))

;; Copy git-branch to clipboard
(defun my-copy-branch-name ()
  "Copy current branch name to clipboard."
  (interactive)
  (kill-new (substring vc-mode 5))
  (message "Copied buffer branch name '%s' to the clipboard." vc-mode))


(require 'dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(defun cider-or-dumb-jump ()
  (interactive)
  (if (cider-connected-p)
      (cider-find-var)
    (dumb-jump-go))
  (recenter-top-bottom))

(add-hook 'clojure-mode-hook (lambda () (local-set-key (kbd "M-.") 'cider-or-dumb-jump)))

(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
	   (replace-regexp-in-string
	    "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
	    (magit-get "remote"
                       (magit-get-current-remote)
                       "url"))
	   (magit-get-current-branch))))


;; Re-frame jumping through keywords
;; https://github.com/oliyh/re-jump.el
;; Has really annoying remapping of M-> to user.clj
;; (require 're-jump)
;; But it's broken, so:
(require 'my-re-jump)
;; (add-hook 'clojure-mode-hook (lambda () (local-set-key (kbd "C->") 're-frame-jump-to-reg)))



;;; Cider Customizations


;; Taken from cider-completion.el

(defun my-cider-annotate-completion-function (type ns )
  "Get completion function based on TYPE and NS and DS."
  (concat (when type (format "\t<%s>" type))
          (when ns (format " (%s)" ns))
          ;; ds
          ))

;; Used by customize
;; Turned out this was just too slow to be usable.
;; Locked up when scrolling down through completion list, and wasn't able to edit candidate
(defun cider-annotate-symbol (symbol)
  "Do cider usual stuff, plus add the docstring SYMBOL annotation."
  (when cider-annotate-completion-candidates
    ;; (message (type symbol))
    (let* ((type (cider-completion--get-candidate-type symbol))
           (ns (cider-completion--get-candidate-ns symbol))
           ;; Get full newline-joined doc

           ;; (docstring (lax-plist-get (cider-eldoc-info symbol) "docstring"))
           ;; ;; Cut it at the first period
           ;; (sentence1 (concat (car (split-string docstring "\\.")) "."))
           ;; (maxlen (min (length sentence1) 100))
           ;; (shorter (substring sentence1 0 maxlen))
           ;; (docstring-final (s-replace-all '( ("\n" . "") ("  " . " ")) shorter))

           ;; (tp (get-text-property 2 'type symbol))
           ;; Ex: <f> (clojure.core)
           (tiny-annots (funcall cider-annotate-completion-function type ns)))
      ;; My addition to add docstring
      ;; (funcall cider-annotate-completion-function type ns)
      ;; (concat tiny-annots "\t" docstring-final) ; simple for just cider
      tiny-annots
      ;; (concat  ; or fancy for marginalia
      ;;  (marginalia--fields
      ;;   ;; (type :face 'marginalia-type)
      ;;   ;; (ns :face 'marginalia-value :truncate 0.5)
      ;;   (tiny-annots :face 'marginalia-value :truncate 0.8) ; 'my-cider-annotate-completion-function
      ;;   ;; (or (cadr (assoc (get-text-property 0 'type symbol) cider-completion-annotations-alist))
      ;;   ;;     (get-text-property 0 'type symbol))

      ;;   (docstring-final :truncate 1.0 :face 'marginalia-documentation)
        )))

;; (defun marginalia-annotate-symbol (cand)
;;   "Annotate function CAND with its documentation string."
;;   (when-let (sym (intern-soft cand))
;;     (when (fboundp sym)
;;       (concat
;;        ;; "Some stuff"
;;        (marginalia-annotate-binding cand)
;;        (marginalia--fields
;;         ((marginalia--symbol-class sym) :face 'marginalia-type)
;;         ((marginalia--function-args sym) :face 'marginalia-value
;;          :truncate 0.5)
;;         ((marginalia--function-doc sym) :truncate 1.0
;;          :face 'marginalia-documentation))))))

;; This is an "annotation-function". You're supposed to provide your own per package
;; (defun marginalia-annotate-function (cand)
(defun cider-complete-at-point ()
  "Complete the symbol at point."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol)))
    (when (and (cider-connected-p)
               (not (or (cider-in-string-p) (cider-in-comment-p))))
      (list (car bounds) (cdr bounds)
            (completion-table-dynamic #'cider-complete)
            ;; This is what does all the work we care about
            ;; :annotation-function #'say-foo
            :annotation-function #'cider-annotate-symbol
            ;; :annotation-function #'marginalia-annotate-symbol
            :company-kind #'cider-company-symbol-kind
            :company-doc-buffer #'cider-create-doc-buffer
            :company-location #'cider-company-location
            :company-docsig #'cider-company-docsig))))

;; SLOW maybe, test with disbursement-create
(defun cider-company-docsig (thing)
  "Return signature for THING."
  (let* ((eldoc-info (cider-eldoc-info thing))
         (ns (lax-plist-get eldoc-info "ns"))
         (symbol (lax-plist-get eldoc-info "symbol"))
         (arglists (lax-plist-get eldoc-info "arglists"))
	 (docstring (lax-plist-get eldoc-info "docstring")))
    (when eldoc-info
      ;; (message "inn cider-company-docsig")
      (format "%s: %s\n%s"
              (cider-eldoc-format-thing ns symbol thing
                                        (cider-eldoc-thing-type eldoc-info))
              (cider-eldoc-format-arglist arglists 0)

	      ;; This is the big addition, to show docstrings as part of completion in echo area
	      docstring))))

(defun cider-eldoc-format-function (thing pos eldoc-info)
  "Return the formatted eldoc string for a function.
THING is the function name.  POS is the argument-index of the functions
arglists.  ELDOC-INFO is a p-list containing the eldoc information."
  (let ((ns (lax-plist-get eldoc-info "ns"))
        (symbol (lax-plist-get eldoc-info "symbol"))
        (arglists (lax-plist-get eldoc-info "arglists")))
    (format "%s: %s\n%s"
            (cider-eldoc-format-thing ns symbol thing 'fn)
            (cider-eldoc-format-arglist arglists pos)

	    ;; Show docstring always in echo area
	    (lax-plist-get eldoc-info "docstring"))))


(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(global-set-key  (kbd "C-M->") 'dumb-jump-go)

(defun my-forward-jump-to-line-break ()
  (interactive)
  (forward-page)
  (forward-char)
  (recenter-top-bottom 50))
(defun my-backward-jump-to-line-break ()
  (interactive)
  (backward-char 2)
  (backward-page)
  (backward-char)
  (recenter-top-bottom 50))

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

;; (autoload 'bash-completion-dynamic-complete "bash-completion" "BASH completion hook")
;; (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)



;;; SLIME MISC

(defun sh-send-line-or-region ()
  (interactive ())
  (process-send-string (get-process "vterm") "ls -l\n"))
;; (define-key shell-script-mode (kbd "C-c C-c") 'sh-send-line-or-region)
(global-set-key (kbd "C-c Q") 'sh-send-line-or-region)
(add-hook 'shell-script-mode (lambda () (local-set-key (kbd "C-c C-c") 'sh-send-line-or-region)))


;; Slime-like for shell/zsh (C-u C-x M-m)
;; http://stackoverflow.com/questions/6286579/
(defun sh-send-line-or-region (&optional step)
  (interactive ())
  ;; (let ((proc (get-process "shell"))
  (let ((proc (get-process "vterm"))
        pbuf min max command)
    ;; (unless proc
    ;;   (let ((currbuff (current-buffer)))
    ;;     (vterm)
    ;;     (switch-to-buffer currbuff)
    ;;     (setq proc (get-process "vterm"))))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    ;; (with-current-buffer pbuff
    ;;   (goto-char (process-mark proc))
    ;;   (insert command)
    ;;   (move-marker (process-mark proc) (point))
    ;;   ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string proc command)
    ;; (display-buffer (process-buffer proc) t)
    ;; (when step
    ;;   (goto-char max)
    ;;   (forward-line))
    ))
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

(defun my-vterm-new ()
  (interactive)
  (split-window-balancedly)
  (multi-vterm))

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
;; (add-hook 'shell-script-mode (lambda () (local-set-key (kbd "C-c C-c") 'tws-region-to-process)))

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

(defun jump-to-penultimate-mark ()
  "Jump to second-to-last mark, rotate last 3 marks."
  (interactive)
  ;; (set-mark-command 1)
  (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
  (set-marker (mark-marker) (point))
  (goto-char (marker-position (cadr mark-ring))))
;; (global-set-key (kbd "C-x C-x") 'push-mark-no-activate)
(global-set-key (kbd "C-S-x C-S-x") 'jump-to-penultimate-mark)

;; Stolen, remove
;; https://stackoverflow.com/a/14539202/326516
(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        ;; (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))

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


;; Can't get vterm to work on mac
(when (eq system-type 'gnu/linux)

  (require 'vterm)

  ;; (define-key vterm-mode-map [(control ?c) (control ?z)] 'aw-flip-window)
  ;; (define-key vterm-mode-map [(control ?c) (control ?z)] 'aw-)

  ;; vterm meta/alt not recognized, so adding manually
  ;; https://github.com/akermu/emacs-libvterm/issues/632
  (define-key vterm-mode-map (kbd "M-f") 'vterm-send-M-f)
  (define-key vterm-mode-map (kbd "M-b") 'vterm-send-M-b)
  (define-key vterm-mode-map (kbd "M-p") 'vterm-send-M-p)
  (define-key vterm-mode-map (kbd "M-n") 'vterm-send-M-n)
  (define-key vterm-mode-map (kbd "M-d") 'vterm-send-M-d)
  (define-key vterm-mode-map (kbd "M-h") 'vterm-send-M-h)
  (define-key vterm-mode-map (kbd "M-v") 'vterm-send-M-v)


  ;; (require 'vterm) ; TEMPORARY
  ;; (define-key vterm-mode-map (kbd "C-c C-z") (lambda () (interactive) (other-window -1)))
  (require 'vterm-toggle)
  ;; (global-set-key [f2] 'vterm-toggle)
  ;; Not working
  (setq vterm-toggle-hide-method nil)

  (define-key vterm-mode-map (kbd "C-RET") #'my-vterm-send-buffer-2)

  (define-key vterm-mode-map (kbd "C-s") #'vterm-send-C-c)


  (add-hook 'vterm-mode-hook (lambda ()
                               (setq show-trailing-whitespace nil)
                               (setf truncate-lines nil)
                               (setq-local show-paren-mode nil)
                               ;; (flycheck-mode -1)
                               ))
  )

(defun ff-new-win ()
  (find-file))




(defun my-cider-find-var ()
  (interactive)
  (make-frame-command)
  (cider-find-var)
  (recenter-top-bottom))
;; (global-set-key (kbd "C->") 'my-cider-find-var)

;; Enable resizing term
;; https://emacs.stackexchange.com/questions/39312/output-reflow-in-ansi-term?noredirect=1&lq=1
(setq term-suppress-hard-newline t)


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






;;; CTRLF, PRESCIENT/ORDERLESS, CONSULT, MARGINALIA, VERTICO, CORFU, EMBARK

;; (selectrum-mode +1) ; useful even when ivy for any completing-read
;; (setq completion-styles '(substring partial-completion))

;; (setq icomplete-prospects-height 30)

;; Complete with double-TAB
;; https://emacsredux.com/blog/2016/01/31/use-tab-to-indent-or-complete/
;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 1)

;; minibuffer-force-complete-and-exit. You might want to bind the
;; latter to RET in minibuffer-local-completion-map (and maybe bind
;; minibuffer-force-complete to something too, like C-M-i or
;; <backtab>).

;; https://github.com/oantolin/live-completions/issues/7
;; (define-key minibuffer-local-completion-map "RET" #'minibuffer-force-complete-and-exit)
;; (define-key minibuffer-local-completion-map "C-l" #'minibuffer-force-complete)

;;; Vertico
;; Really nicely uses marginalia
;; https://github.com/minad/vertico
(use-package vertico
  :init
  (vertico-mode)
  ;; Optionally enable cycling for `vertico-next', `vertico-previous', `vertico-next-group' and `vertico-previous-group'.
  ;; (setq vertico-cycle t)
  )
(define-key vertico-map "?" #'minibuffer-completion-help)
(define-key vertico-map (kbd "M-RET") #'minibuffer-force-complete-and-exit)
(define-key vertico-map (kbd "M-TAB") #'minibuffer-complete)
(define-key vertico-map (kbd "C-n") #'next-line)
(define-key vertico-map (kbd "C-p") #'previous-line)

;; vertico-repeat extentsion; this works! added to savehist-additional-variables
;; https://github.com/minad/vertico/blob/main/extensions/vertico-repeat.el
(define-key vertico-map (kbd "M-r") 'vertico-repeat)
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

;; Enable C-SPC to select multiple candidates
;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
(advice-remove #'completing-read-multiple  #'consult-completing-read-multiple)
(defun crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; completion-category-overrides '((file (styles . (partial-completion))))
	orderless-component-separator " +"
	))



;; https://github.com/minad/vertico/wiki#customize-sorting-based-on-completion-category
;; https://github.com/minad/vertico/issues/160 ; my issue
;; try the `completion-category-sort-function' first
;; (advice-add #'vertico--sort-function :before-until #'completion-category-sort-function)
;; ;; (advice-remove #'vertico--sort-function #'completion-category-sort-function)




;; (setq completion-styles '(orderless flex basic))
;; Cider completion bug!! Must have "basic" included for namespaces to work.
;; https://github.com/clojure-emacs/cider/issues/3019
(setq completion-styles '(orderless basic))

;; (setq completion-styles '(initials basic))

;; ;; https://github.com/karthink/consult-dir
;; (use-package consult-dir
;;   :ensure t
;;   :bind (("C-x C-d" . consult-dir)
;;          :map vertico-map
;;          ("C-x C-d" . consult-dir)
;;          ("C-x C-j" . consult-dir-jump-file)))


(use-package embark
  :ensure t
  :bind
  (("C-," . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; Configure corfu (Has a prob with cider under some package combo)
(use-package corfu
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first t)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 20)        ;; Use scroll margin
  :init
  (global-corfu-mode))
;; (corfu-mode -1)

(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))
(define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)


;; Corfu-doc
;; Display a documentation popup for completion candidate when using
;; Corfu. It can be regarded as company-quickhelp for Corfu.
;; https://github.com/galeo/corfu-doc
;; (require 'corfu-doc)
;; (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
;; (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
(define-key corfu-map (kbd "M-n") #'corfu-next)
(define-key corfu-map (kbd "M-p") #'corfu-previous)
;; (add-hook 'corfu-mode-hook #'corfu-doc-mode)

(require 'corfu-popupinfo)
(require 'corfu-terminal)
;; (require 'corfu-doc-terminal)
;; (corfu-doc-terminal-mode +1)
;; (corfu-doc-mode +1)





(defun my-cider-try-completion (string table pred point &optional metadata)
  ;; call cider-complete and MASSAGE it into a pair???
  ;; (message "\n")
  ;; (message string)
  ;; (print table)
  ;; (print (type-of pred))
  ;; (message "%d" point)
  (cons string point))

;; Replace cider's bad try-completion fn
;; https://github.com/minad/corfu/issues/30
;; FIXME
;; (add-to-list 'completion-styles-alist '(cider my-cider-try-completion cider-company-unfiltered-candidates "CIDER backend-driven completion style."))

;; Example of calling nrepl command directly, like cljr does!
;; (cider-nrepl-send-sync-request '("op" "clean-ns" "prefix-rewriting" "false" "debug" "false" "path" "/home/mde/work/cc/src/clj/crawlingchaos/process/changeorder/changes.clj" "relative-path" "src/clj/crawlingchaos/process/changeorder/changes.clj" "prune-ns-form" "true"))
;; => prints new ns string that you can substute in

;; (cider-nrepl-send-sync-request '("op" "artifact-versions" "artifact" "org.clojure/clojure"))
;; => (dict "status" ("done") "id" "213" "session" "171b8d0e-64ee-40e6-95f7-c48fd0e2e318"
;;     "versions" ("1.11.0-alpha1" "1.10.3" "1.10.3-rc1" "1.10.2" "1.10.2-rc3" ...  "1.10.2-alpha1" "1.10.1" ...))

;; (cider-nrepl-send-sync-request '("op" "resolve-missing" "symbol" "postwalk"))
;; => (dict "status" ("done") "candidates" "(
;; {:name refactor-nrepl.inlined-deps.rewrite-clj.v0v6v1.rewrite-clj.zip.walk, :type :ns}
;; {:name clojure.walk, :type :ns}
;; {:name refactor-nrepl.inlined-deps.rewrite-clj.v0v6v1.rewrite-clj.zip, :type :ns}
;; {:name rewrite-clj.zip.walk, :type :ns}
;; {:name from.potemkin.walk, :type :ns}
;; {:name schema-tools.walk, :type :ns}
;; {:name potemkin.walk, :type :ns}
;; {:name rewrite-clj.zip, :type :ns}
;; {:name clojure.tools.analyzer.ast, :type :ns})"
;;   "id" "210" "session" "171b8d0e-64ee-40e6-95f7-c48fd0e2e318")


;; (cider-nrepl-send-sync-request '("op" "find-used-publics"  "file" "/home/mde/work/cc/src/clj/crawlingchaos/process/changeorder/changes.clj" "used-ns" "crawlingchaos.process.changeorder.changes" ))

;; Dabbrev works with Corfu
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))
;; Expand file names too!
(global-set-key (kbd "C-M-?") 'hippie-expand) ; C-M-S-/

;; Persist history over Emacs restarts. Vertico sorts by history position.
;; savehist keeps track of some history
(use-package savehist
  :init
  (setq savehist-additional-variables
	;; search entries
	'(search-ring kill-ring regexp-search-ring vertico-repeat-history)

	;; save every minute
	savehist-autosave-interval 60
	;; keep the home clean
	savehist-file (expand-file-name "savehist" savefile-dir))
  ;; Had a bug when this was before the setq
  (savehist-mode)
  )

;; (defun crm-indicator (args)
;;   (cons (concat "[CRM] " (car args)) (cdr args)))
;; (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Grow and shrink minibuffer
(setq resize-mini-windows t)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)


;; Improve isearch: https://github.com/raxod502/ctrlf#why-not-isearch
;; In vendor now since C-L hack: https://github.com/raxod502/ctrlf/pull/92/files
(use-package ctrlf)
(ctrlf-mode +1)

(defun my-ctrlf-tap-simplified ()
  "Remove leading colon(s) since often want to match both symbols and keywords.
So when point is on a keyword like ':foo', you'll still find 'foo'.
Press C-s/C-r repeatedly afterward to bounce results.
This is way faster than 'C-s M-n C-a C-d'."
  (interactive)
  (let* ((tap (thing-at-point 'word))
	 ;; TODO Use `string-replace' instead when v28 is out (https://stackoverflow.com/a/66039099/326516)
	 (word (when tap (s-replace ":" "" tap))))
    (ctrlf-forward 'literal nil word)))
(global-set-key (kbd "C-S-f") 'my-ctrlf-tap-simplified)
;; (global-set-key (kbd "C-s") 'ctrlf-forward-default)



;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))


;; Consult: Consulting completing-read
;; quickly select an item from a list of candidates with completion; live preview and narrowing support
;; https://github.com/minad/consult

;; (global-set-key (kbd "C-s") 'consult-isearch)
(global-set-key (kbd "C-S-s") 'consult-line)


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
	 ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ;; ("M-g i" . consult-project-imenu) ;; Alternative: consult-imenu
	 ("M-g e" . consult-error)
	 ;; M-s bindings (search-map)
	 ;; ("M-s g" . consult-git-grep)              ;; alt. consult-grep, consult-ripgrep
	 ;; ("M-s f" . consult-find)                  ;; alt. consult-locate, find-fd
	 ;; ("M-s l" . consult-line)
	 ;; ("M-s m" . consult-multi-occur)
	 ;; ("M-s k" . consult-keep-lines)
	 ;; ("M-s u" . consult-focus-lines)
	 ;; Replacement for isearch-edit-string
	 ;; ("M-s e" . consult-isearch)
	 :map isearch-mode-map
	 ;; ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
	 ;; ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
	 )

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
  ;; (advice-add #'register-preview :override #'consult-register-window)



  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq-default completion-in-region-function
		(lambda (&rest args)
		  (apply (if vertico-mode
			     #'consult-completion-in-region
			   #'completion--in-region)
			 args)))

  ;; (setq-default completion-in-region-function 'consult-completion-in-region)
  ;; (setq completion-in-region-function 'consult-completion-in-region)
  ;; (setq-default completion-in-region-function 'corfu--completion-in-region)


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

  ;; Configure a function which returns the project.el root directory
  (setq consult-project-root-function
        (lambda () (when-let (project (project-current))
		(car (project-roots project)))))



  ;; ;; Recall last search
  ;; ;; https://github.com/minad/consult/issues/214
  ;; (setf (alist-get #'consult-line consult-config)
  ;; 	(list :keymap (let ((map (make-sparse-keymap)))
  ;; 			(define-key map "\C-s" #'previous-history-element)
  ;; 			map)))

  )





;; Enable consult/vertico to do completion-at-point
;; https://github.com/minad/consult#miscellaneous
;; https://github.com/minad/consult/issues/338
;; (setq completion-in-region-function 'consult-completion-in-region)




;; (require 'consult)

;; Optionally add the `consult-flycheck' command.
;; (use-package consult-flycheck
;;   :bind (:map flycheck-command-map
;; 	      ("!" . consult-flycheck)))



(defun my/popper-toggle-latest ()
  (interactive)
  (aw--push-window (get-buffer-window))
  (popper-toggle-latest))

;; https://github.com/karthink/popper
(use-package popper
  :ensure t ; or :straight t
  :bind (;("C-`"   . popper-toggle-latest)
	 ("C-`"   . (lambda () (interactive) (aw--push-window (get-buffer-window)) (popper-toggle-latest)))
	 ;; ("C-`"   . my/popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
	  cider-browse-ns-mode
	  "\\*cider-ns-browser\\*"
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))


(defun my-copy-namespace-function-lineno ()
  "Copy the NS, function, and line number to kill ring.

This is a replacement for `github-browse-file' and ilk. It's
often nicer to just see a location of a fn/line that you can plug
into Emacs, rather than jump to a browser and see it on GH."
  (interactive)
  (save-excursion
    (my-beginning-of-defun)
    (let* ((fn    (word-at-point))
	   (ln    (number-to-string (line-number-at-pos)))
	   (ns    (cider-current-ns))
	   (combo (concat ns "/" fn ":" ln)))
      (kill-new combo)
      (message "Copied location '%s' to the clipboard." combo)
      ;; (alert "Copied to kill ring" :severity 'low :title combo)
      )))

(defun my-copy-namespace-name ()
  "Copy the namespace name to kill ring."
  (interactive)
  (kill-new (cider-current-ns))
  ;; (message-box "Copied to kill ring")
  ;; (alert "Copied to kill ring" :severity 'low :title (cider-current-ns))
  )
(define-key cider-mode-map (kbd "C-c C-n c") 'my-copy-namespace-name)


;; Line breaks (C-l, ^L) are shown as pretty horizontal lines
;; Maybe needs to be at end of file
;; Navigate: C-x ] and C-x [
;; https://stackoverflow.com/a/7577628/326516
(require 'page-break-lines)
(global-page-break-lines-mode)



;;; MODE-LINE

;; Mode-line, modeline
;; (require 'mood-line)
;; (mood-line-mode)

;; https://github.com/Malabarba/smart-mode-line
;; (setq sml/theme 'respectful)
;; (sml/setup)

;; Show buffer position in mode-line, turn 70+ cols red
;; https://raw.githubusercontent.com/emacsmirror/modeline-posn/master/modeline-posn.el
;; (require 'modeline-posn)


;; https://github.com/gexplorer/flycheck-indicator
;; (use-package flycheck-indicator
;;   :hook (flycheck-mode . flycheck-indicator-mode))


;; (require 'mlscroll)
;; mlscroll: tiny purple scrollbar in mode-line
;; Might have to run manually: mlscroll-mode after starting
;; Ignore the :box warnings; seems to work fine/better with mode-line box
;; (use-package mlscroll
;;   :ensure t
;;   :hook (server-after-make-frame . mlscroll-mode))
;; (use-package mlscroll
;;   :ensure t
;;   :config
;;   (setq mlscroll-shortfun-min-width 11) ;truncate which-func, for default mode-line-format's
;;   (mlscroll-mode 1))


;; ;; diminish keeps the modeline tidy
;; (require 'diminish)
;; (diminish 'my-keys)
;; (diminish 'hs)

;; (require 'delight)
;; (delight '((abbrev-mode " Abv" abbrev)
;;            (smart-tab-mode " \\t" smart-tab)
;;            (eldoc-mode nil "eldoc")
;;            (rainbow-mode)
;;            (overwrite-mode " Ov" t)
;;            (emacs-lisp-mode "Elisp" :major)))

;;  hiding and/or highlighting the list of minor-modes in the mode-line.
;; cf. delight, diminish
;; https://github.com/Malabarba/rich-minority
(setq rm-blacklist
      (format "^ \\(%s\\)$"
              (mapconcat #'identity
                         '(
			   "Test"
			   "ElDoc"
			   "cider.*"
			   "Abbrev"
                           "PgLn"
			   "ARev"
			   "Dot"
			   "Abv"
			   "WK"
			   "Undo-Tree"
			   "super-save"
			   "SP/s"
			   "my-keys"
			   "envrc.*"
			   "Goggles"
			   "SoT"
			   "hs"
			   "²"
			   ;; "light" cider-enlighten
			   "superword-mode")
                         "\\|")))
;; No need to activate if using smart-mode-line
(rich-minority-mode 1)


;; ;; Tabs and ribbons for the mode-line
;; ;; https://github.com/tarsius/moody
;; (use-package moody
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode)
;;   (moody-replace-eldoc-minibuffer-message-function))

;; Truncate branch name in mode-line
;; https://emacs.stackexchange.com/questions/10955/customize-vc-mode-appearance-in-mode-line
(defadvice vc-mode-line (after strip-backend () activate)
  (when (stringp vc-mode)
    ;; (replace-regexp-in-string "^Git" " " "Git-mde/SCRUM-12345-foo-bar")
    ;; (replace-regexp-in-string "SCRUM-\\([0-9]+\\)-.*" "\\1" "Git-mde/SCRUM-12345-foo-bar")
    (let ((noback (replace-regexp-in-string
                   ;; (format "^ %s" (vc-backend buffer-file-name)) ; orig from malabarba
		   ;; Git-mde/SCRUM-12345-foo-bar
                   "mde/SCRUM-\\([0-9]+\\)_.*"
                   "\\1" vc-mode)))
      (setq vc-mode noback))))

;; https://emacs.stackexchange.com/questions/10222/make-the-mode-line-display-percentage-and-not-top-bottom-all
;; (setcar mode-line-position
;;         ;; '(:eval (format "%3d%%" (/ (window-start) 0.01 (point-max)))))
;; 	'(:eval (format "%3d%%" (/ (window-start) 0.01 (point-max)))))

;; Not using since hides some important modes, like flycheck
;; (use-package minions
;;   :config (minions-mode 1))

;; https://github.com/tarsius/keycast
;; Enable keycast for demos to log all keys to separate buffer
;; (keycast-log-mode 1)



;; Could remove the -'s
;; http://hideki.hclippr.com/2014/02/02/on-generating-uuid/
(defun insert-random-uuid ()
  (interactive)
  (shell-command "uuidgen" t))

;; (require 'symbolic-clojure)

;; https://emacs.stackexchange.com/questions/47706/how-to-prettify-symbols-inside-comments
;; For some reason this needs to be set in the clojure buffer, then turn on/off prettify-symbols-mode
(setq prettify-symbols-compose-predicate
      (defun my-prettify-symbols-default-compose-p (start end _match)
        "Same as `prettify-symbols-default-compose-p', except compose symbols in comments as well."
        (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
                                 '(?w ?_) '(?. ?\\)))
               (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
                                 '(?w ?_) '(?. ?\\))))
          (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
                   (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end)
		   ;; This is the bit that says to only operate on comments but not strings
                   (nth 3 (syntax-ppss))
		   )))))


;; ;; Default elisp imenu-generic-expression:
;; ((nil "^\\s-*(\\(transient-define-\\(?:argument\\|\\(?:in\\|pre\\|suf\\)fix\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
;;  (nil "^\\s-*(\\(cl-def\\(?:generic\\|ine-compiler-macro\\|m\\(?:acro\\|ethod\\)\\|subst\\|un\\)\\|def\\(?:advice\\|generic\\|ine-\\(?:advice\\|compil\\(?:ation-mode\\|er-macro\\)\\|derived-mode\\|g\\(?:\\(?:eneric\\|lobal\\(?:\\(?:ized\\)?-minor\\)\\)-mode\\)\\|inline\\|m\\(?:ethod-combination\\|inor-mode\\|odify-macro\\)\\|s\\(?:etf-expander\\|keleton\\)\\)\\|m\\(?:acro\\|ethod\\)\\|s\\(?:etf\\|ubst\\)\\|un\\*?\\)\\|ert-deftest\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
;;  ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
;;  ("Variables" "^\\s-*(defvar\\(?:-local\\)?\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]+[^)]" 1)
;;  ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2))

(setq clj-imenu-generic-expression
      ;; Ordering is reversed from this in imenu-list, so put most important at bottom.
      '(("Deprecated" "^(defn-? \\^:deprecated \\(?1:[a-z0-9\\?<>!-]+\\)" 1)
	("Fixme" ";; +\\(?:FIXME\\|TODO\\|XXX\\) \\(.*\\)" 1)
	("Fixme" "; +\\(?:FIXME\\|TODO\\|XXX\\) \\(.*\\)" 1)
	("Comment Blocks" "^(comment ; \\(.*\\)" 1)
        ;; Intentionally ignore deprecated fns
	;; ("Functions" "^(defn-? \\(?3:\\^:deprecated \\)?\\(?1:[a-z\\?<>!-]+\\)" 1)
	("Functions :: Public"  "^(defn \\(?1:[a-z0-9\\?<>!-]+\\)" 1)
	("Functions :: Private" "^(defn- \\(?1:[a-z0-9\\?<>!-]+\\)" 1)
        ("Vars"      "^(def \\(?3:\\^:\\(?:private\\|impure\\) \\)?\\(?1:[a-z0-9\\?<>!-]+\\)" 1)
        ;; ("Sections" "^;;; \\(.+\\)" 1)
        ("Sections" "^;;;;? \\(.+\\)" 1)
	("NS" "^(ns \\([a-z0-9.]+\\)" 1)))
(add-hook 'clojure-mode-hook (lambda ()  (setq imenu-generic-expression clj-imenu-generic-expression)))

;; What is 42?
(require 'gpt)




;;; cljr piecemeal replacement

(defun cljr--goto-ns ()
  "Go to the first namespace defining form in the buffer."
  (goto-char (point-min))
  (if (re-search-forward clojure-namespace-regexp nil t) ; defined in clojure-mode
      (beginning-of-buffer) ; simplistic but usually ok to assume first line is (ns ...)
    (error "No namespace declaration found")))

(defun cljr--insert-in-ns (type)
  "Insert another clause into the TYPE clause of the ns statement."
  (cljr--goto-ns)
  (if (search-forward (concat "(" type))
      (if (looking-at " *)")
          (progn
            (search-backward "(")
            (forward-list 1)
            (forward-char -1)
            (insert " "))
        (search-backward "(")
        (forward-list 1)
        (forward-char -1)
        (newline-and-indent))
    (forward-list 1)
    (forward-char -1)
    (newline-and-indent)
    (insert "(" type " )")
    (forward-char -1)))


;;; CLJNS

(setq cljns--pairs (butlast (s-split "\n" (f-read-text "~/.emacs.d/cljns-canon.tsv"))))
(setq cljns--mapping1 (--map (s-split "\t" it) cljns--pairs))
(setq cljns--mapping2 (--map (reverse (s-split "\t" it)) cljns--pairs))
(setq cljns--fqnss (--map (last it) cljns--mapping1))
(setq cljns--aliases (--map (car it) cljns--mapping1))

(defvar demo-var-with-docstrinng 24 "some info on the demo")

(defun cljns-add-require ()
  "Add a require ns, sort, re-eval ns.
Can be called conveniently via `cider-transient'."
  (interactive)
  (save-excursion
    (let* ((alias (car (s-split "/" (thing-at-point 'symbol 'no-properties))))
           ;; (choice (completing-read "namespace:" cljns--fqnss))
           (fqns   (first (last (assoc alias cljns--mapping1)))))
      (cljr--insert-in-ns ":require")
      (insert (format "[%s :as %s]" fqns alias))
      (clojure-sort-ns)
      (cider-eval-ns-form))))

(defun cljns-completing-ns ()
  "Complete a partially entered namespace alias."
  (interactive)
  (let ((ans (completing-read "fqns:" cljns--fqnss)))
    (insert (first (last (assoc ans cljns--mapping2))))))

;; https://with-emacs.com/posts/tutorials/customize-completion-at-point/
;; https://emacs.stackexchange.com/a/37446/11025
(defun cljns-complete-ns ()
  "Tab-completeable ns completer."
  (interactive)
  ;; Get word-at-point
  ;; Read all NSs into list
  (let* ((tap (bounds-of-thing-at-point 'word))
	 (beg (car tap))
	 (end (cdr tap))
	 ;; (alist '(("str"  "clojure.string") ("csv"  "clojure.data.csv")))
	 (annotf (lambda (str)
		   (format " (%s)" (first (cdr (assoc str cljns--mapping2))))))
	 (ans (completing-read "Candidates: "
			       (lambda (str pred action)
				 (if (eq action 'metadata)
				     `(metadata
				       (annotation-function . ,annotf)
				       (cycle-sort-function . identity)
				       (display-sort-function . identity))
				   (complete-with-action action cljns--mapping2 str pred)))
			       nil nil (word-at-point) )))
    (message ans)
    (when beg (delete-region beg end))
    ;; (insert (concat ans "/"))
    ;; (first (cdr (assoc "clojure.string" cljns--mapping2)))
    (insert (concat (first (cdr (assoc ans cljns--mapping2))) "/"))))

(define-key clojure-mode-map (kbd "<backtab>") 'cljns-complete-ns)
;; (add-hook 'clojure-mode-hook (lambda () (local-set-key (kbd "<backtab>") 'cljns-complete-ns)))



;; (defun cljr--call-middleware-for-namespace-aliases ()
;;   (thread-first "namespace-aliases"
;;     cljr--ensure-op-supported
;;     cljr--create-msg
;;     (cljr--call-middleware-sync "namespace-aliases")
;;     parseedn-read-str))

;; (defun cljr--maybe-rethrow-error (response)
;;   response)
;; ;; (apply #'list "op" "namespace-aliases")

;; (defun cljr--call-middleware-sync (request &optional key)
;;   (let* ((response (thread-first request cider-nrepl-send-sync-request (lambda (request) request))))
;;     (if key
;;         (nrepl-dict-get response key)
;;       response)))

;; Don't add any spaces to align-regexp (1 is default?)
;; (setq align-default-spacing 0)
(defun cljns-align ()
  "Align ns requires."
  (interactive)
  (end-of-buffer)
  (when (re-search-backward "^\(ns.*\\(\n.*\\)*\(:require" nil t nil)
    (mark-sexp)
    (align-regexp (region-beginning)
                  (region-end)
                  "\\(\\s-*\\)\\s-:")))



;; Corfu
;; (completion-in-region 2335 2337  '("ccc" "ccd" "cdd"))

(defvar my-d (lazy-completion-table my-d my-d) "my ht")
(defun my-d ()
  (let ((tab (make-hash-table :test #'equal :size 3)))
    (puthash "aaa" nil tab )
    (puthash "aab" nil tab )
    (puthash "abb" nil tab )
    tab))



;;; BQN

;; (add-to-list 'exec-path "~/src/CBQN")
;; (add-to-list 'load-path "~/src/bqn-mode")
;; (require 'bqn-mode)


;;; END (effectively)

(provide 'init)

;; '(safe-local-variable-values
;;    '((eval with-eval-after-load 'cider
;; 	   (setq cider-default-cljs-repl 'figwheel))
;;      (cider-lein-global-options . "with-profile +dev,+test")
;;      (scss-mode
;;       (css-indent-offset . 2))))

;; Make the active window more visible than others
;; https://github.com/yoshida-mediba/hiwin-mode
;; Ugh, make cider repl go blank when switching out
;; (require 'hiwin)
;; (hiwin-activate)
;; Set face color with customize
;; (set-face-background 'hiwin-face "gray80")

;; ;; https://stackoverflow.com/questions/1516830/custom-background-for-active-window
;; (defadvice handle-switch-frame (around switch-frame-set-background)
;;   (set-background-color "black")
;;   ad-do-it
;;   (set-background-color "blue"))
;; (ad-activate 'handle-switch-frame)

;; (defadvice delete-frame (after delete-frame-set-background)
;;   (set-background-color "yellow"))
;; (ad-activate 'delete-frame)

;;; init ends here




;; '(popwin:popup-window-height 30)

;; '(selectrum-count-style 'current/matches)
;; '(selectrum-display-style '(vertical))
;; '(selectrum-num-candidates-displayed 10)
;; '(selectrum-show-indices nil)

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
