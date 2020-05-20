;;; personal-init --- Micah's customizations

;;; Commentary:
;; Personal config instructions:
;; https://github.com/bbatsov/prelude/issues/596

;; Process for updating:
;; https://help.github.com/articles/syncing-a-fork/
;; git fetch upstream
;; git pull upstream master

;;; Code:


;; https://emacs.stackexchange.com/a/4258/11025
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))


;;; Packaging

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
    ag
    ample-theme
    auto-dim-other-buffers
    avy
    beacon
    buffer-move
    cider
    cider-eval-sexp-fu
    clj-refactor
    clojure-mode
    clojure-mode-extra-font-locking
    comment-dwim-2
    company
    company-quickhelp
    counsel
    counsel-projectile
    crux
    cucumber-goto-step
    cycle-quotes
    delight
    diff-hl
    diminish
    discover-clj-refactor
    dot-mode
    dumb-jump
    easy-kill
    edit-indirect
    exec-path-from-shell
    expand-region
    feature-mode
    fic-mode
    flx-ido
    flycheck-clj-kondo
    flycheck-clojure
    flycheck-joker
    flycheck-yamllint
    flymd
    git-messenger
    git-timemachine
    highlight-parentheses
    ido
    ido-completing-read+
    ivy
    jump-char
    key-chord
    kibit-helper
    nlinum-relative
    magit
    markdown-mode
    mode-icons
    move-text
    neotree
    page-break-lines
    paren-face
    popup
    projectile
    rainbow-delimiters
    ripgrep
    rubocop
    shrink-whitespace
    smartparens
    smart-mode-line
    smart-mode-line-powerline-theme
    string-inflection
    smex
    toggle-quotes
    typo
    undo-tree
    unfill
    which-key
    yaml-mode))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/vendor")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-mode t)
 '(beacon-blink-duration 0.4)
 '(beacon-blink-when-focused t)
 '(beacon-blink-when-point-moves-vertically 5)
 '(beacon-color "red")
 '(beacon-push-mark nil)
 '(browse-url-browser-function (quote browse-url-firefox))
 '(browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox")
 '(cider-special-mode-truncate-lines nil)
 '(cljr-favor-private-functions nil)
 '(cljr-hotload-dependencies t)
 '(cljr-magic-require-namespaces
   (quote
    (("str" . "clojure.string")
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
     ("rf" . "re-frame.core"))))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(fci-rule-color "#383838")
 '(flycheck-pycheckers-checkers (quote (pylint pep8 pyflakes bandit)))
 '(global-hl-line-mode nil)
 '(global-superword-mode t)
 '(global-yascroll-bar-mode t)
 '(hl-paren-colors (quote ("red" "IndianRed1" "IndianRed3" "IndianRed4")))
 '(ido-default-file-method (quote selected-window))
 '(inhibit-startup-screen nil)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "--stat" "-n10")))
 '(markdown-header-scaling t)
 '(markdown-wiki-link-search-subdirectories t)
 '(neo-show-hidden-files t)
 '(neo-theme (quote icons))
 '(neo-window-position (quote right))
 '(neo-window-width 40)
 '(package-selected-packages
   (quote
    (yaml-mode diminish which-key diff-hl git-timemachine delight company-quickhelp-terminal auto-dim-other-buffers key-chord visible-mark flycheck-pos-tip company-quickhelp move-text easy-kill ample-theme beacon unfill string-inflection undo-tree typo toggle-quotes smex smartparens smart-mode-line-powerline-theme shrink-whitespace rubocop ripgrep rainbow-delimiters paren-face page-break-lines neotree mode-icons markdown-mode magit kibit-helper jump-char ido-completing-read+ highlight-parentheses git-messenger flymd flycheck-yamllint flycheck-joker flycheck-clojure flycheck-clj-kondo flx-ido fic-mode feature-mode expand-region exec-path-from-shell edit-indirect dumb-jump dot-mode discover-clj-refactor cycle-quotes cucumber-goto-step crux counsel-projectile company comment-dwim-2 clojure-mode-extra-font-locking cider-eval-sexp-fu buffer-move all-the-icons-dired ag ace-window)))
 '(projectile-enable-caching t)
 '(projectile-file-exists-remote-cache-expire nil)
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "vendor")))
 '(projectile-indexing-method (quote hybrid))
 '(projectile-mode t nil (projectile))
 '(projectile-sort-order (quote recently-active))
 '(safe-local-variable-values
   (quote
    ((cider-lein-global-options . "with-profile +dev,+test")
     (scss-mode
      (css-indent-offset . 2)))))
 '(scroll-bar-mode nil)
 '(search-whitespace-regexp "\"[ \\t\\r\\n]+\"")
 '(show-trailing-whitespace t)
 '(standard-indent 2)
 '(text-scale-mode-step 1.1)
 '(tramp-default-method "ssh")
 '(which-key-max-description-length 45))


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
 '(default ((t (:inherit nil :stipple nil :background "gray16" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 60 :width normal :foundry "nil" :family "Fira Code"))))
 '(auto-dim-other-buffers-face ((t (:background "gray29"))))
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
 '(markdown-header-face ((t (:foreground "#A6E22E" :weight bold :family "Alegreya Sans SC"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :weight bold :height 1.5))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :weight bold :height 1.3))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :slant italic :height 1.05))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :slant italic :height 1.0))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))))
 '(markdown-italic-face ((t (:inherit italic :slant italic))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face))))
 '(org-block ((t (:background "#3E3D31" :foreground "#F8F8F0" :family "Fira Code"))))
 '(org-code ((t (:foreground "#75715E" :family "Fira Code"))))
 '(page-break-lines ((t (:slant normal :weight normal :height 180 :width condensed :family "Fira Code"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse" :weight bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue" :weight bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow" :weight bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1" :weight bold))))
 '(region ((t (:inherit highlight :background "slate blue"))))
 '(which-key-command-description-face ((t nil)))
 '(whitespace-tab ((t (:background "purple4" :foreground "#757575")))))

;; '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1" :weight bold))))
;; '(font-lock-comment-delimiter-face ((t (:foreground "#75715E"))))
;; '(font-lock-comment-face ((t (:foreground "#75715E" :height 0.9 :family "Delius"))))
;; '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "dodger blue" :height 1.1 :family "Alegreya Sans"))))
;; '(font-lock-function-name-face ((t (:foreground "#A6E22E" :underline t :weight ultra-bold))))
;; '(font-lock-type-face ((t (:foreground "#66D9EF" :slant italic :weight bold))))



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

(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
;; delegate theming to the currently active theme
(setq sml/theme nil)
(add-hook 'after-init-hook #'sml/setup)
(require 'smart-mode-line-powerline-theme)

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
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)



;; show the cursor when moving after big movements in the window
(require 'beacon)
(beacon-mode +1)
(global-set-key (kbd "C-S-c") 'beacon-blink)


;; ENABLE??
;; https://github.com/magnars/expand-region.el
;; (require 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)



;;; Tuning

;; https://github.com/hlissner/doom-emacs/issues/2217#issuecomment-568037014
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


;; mimic popular IDEs binding, note that it doesn't work in a terminal session
(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
(global-set-key [(shift return)] 'crux-smart-open-line)
(global-set-key [(control shift return)] 'crux-smart-open-line-above)
(global-set-key (kbd "C-c e") 'crux-eval-and-replace)
(global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "C-c t") 'crux-visit-term-buffer)
(global-set-key (kbd "C-c k") 'crux-kill-other-buffers)
(global-set-key (kbd "C-c I") 'crux-find-user-init-file)

(require 'move-text)
(global-set-key [(meta shift up)]  'move-text-up)
(global-set-key [(meta shift down)]  'move-text-down)

;; Undo-tree
;; http://pragmaticemacs.com/emacs/advanced-undoredo-with-undo-tree/
(global-undo-tree-mode 1)
(global-set-key (kbd "C-?") 'undo-tree-redo)




;;; Look-n-Feel

;; Displaying of tab widith; like vim's tabstop
(setq tab-width 20)

;; Use monospaced font faces in current buffer
(defun my-buffer-face-mode-fixed ()
  "Set a fixed width (monospace) font in current buffer."
  (interactive)
  (typo-mode 0)
  ;; (setq buffer-face-mode-face '(:family "Fantasque Sans Mono" :height 180))
  (setq buffer-face-mode-face '(:family "Fira Code" :height 60))
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




;;; Ivy, Counsel, Swiper

(require 'ivy)
(ivy-mode 1)
(counsel-mode)
(setq ivy-height 20)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq projectile-completion-system 'ivy)

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

;; Buffers (B) and File (F)
(global-set-key (kbd "C-S-b") 'counsel-ibuffer)
(global-set-key (kbd "C-S-f") 'counsel-recentf)

;; Enable counsel replacements for projectile.
;; https://github.com/ericdanan/counsel-projectile
(require 'counsel-projectile)
(counsel-projectile-mode)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-c k") 'ag-project-files)
(global-set-key (kbd "C-x l") 'counsel-locate)

;; Projectile
;; ENABLE
(require 'projectile)
(projectile-global-mode t)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-S-p") 'projectile-command-map)
;; (require 'helm-rg)
;; (define-key projectile-mode-map (kbd "C-S-p s") 'helm-projectile-rg)
;; (define-key projectile-mode-map (kbd "C-S-p S") 'helm-projectile-ag)

;; https://github.com/nlamirault/ripgrep.el
(require 'ripgrep)



;;; Linters

(require 'flycheck-yamllint)


;; Typopunct: fancy/pretty quotes, etc: — ‘’ “”
;; enable: M-x typopunct-mode
;; https://www.emacswiki.org/emacs/TypographicalPunctuationMarks
;; (prelude-require-package 'typopunct)
;; (typopunct-change-language 'english t)
;; (typopunct-mode 1)
;; https://github.com/jorgenschaefer/typoel
(setq-default typo-language "English")
(global-set-key (kbd "C-c T") 'typo-mode)
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
;; (global-set-key (kbd "C-'") 'toggle-quotes)
;; TEST: Can't "do" this.

;; Simpler attempt at typography.
(global-set-key (kbd "C-c '") "’")
(global-set-key (kbd "C-c `'") "‘")
(global-set-key (kbd "C-c \"") "“")
(global-set-key (kbd "C-c /") "”")
(global-set-key (kbd "C-c -") "—")

(global-set-key (kbd "C-'") 'toggle-quotes)
;; TEST: Can't "do" this.


;; Defvault to using typo mode for better/fancy typography
(typo-global-mode 1)
(add-hook 'text-mode-hook 'typo-mode)



;; Jump to help window when opened
;; http://stackoverflow.com/questions/36506141/emacs-dispatch-help-window-from-original-buffer
(setq help-window-select t)


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






;; CONTINUE FROM OLD INIT.RD



;; special treatment of FIXME, etc
;; ENABLE??
;; (require 'fic-mode)
;; (add-hook 'prog-mode-hook 'fic-mode)

;; Wow, hide comments!! Just blanks them out.
(require 'hide-comnt) ; in vendor/ since not in melpa
(global-set-key (kbd "C-c C") 'hide/show-comments-toggle)

;; Planck-friendly
(global-set-key (kbd "M-{") 'backward-paragraph)
(global-set-key (kbd "M-<") 'forward-paragraph)
(global-set-key (kbd "M->") 'end-of-buffer)
(global-set-key (kbd "M-}") 'beginning-of-buffer)



;; Highlight word matching point without doing anything
;; https://github.com/nonsequitur/idle-highlight-mode/blob/master/idle-highlight-mode.el
;; Disabling since might play badly with org-mode
;; Also screws with visible-mark.
;; ENABLE
;; (prelude-require-package 'idle-highlight-mode)
;; (add-hook 'prog-mode-hook 'idle-highlight-mode)


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
(require 'nlinum-relative)
(global-nlinum-relative-mode 1)
;; (global-display-line-numbers-mode)
(setq nlinum-relative-redisplay-delay 1)
(setq nlinum-relative-offset 0)
(global-set-key (kbd "C-S-M-L") 'nlinum-relative-toggle)
(global-set-key (kbd "C-S-L") 'nlinum-mode)
;; (setq nlinum-relative-current-symbol "->")      ; or "" for display current line number

;; Visible mark
;; http://pragmaticemacs.com/emacs/regions-marks-and-visual-mark/
(setq visible-mark-max 2)
(setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))
(global-visible-mark-mode)
(defface visible-mark-active ;; put this before (require 'visible-mark)
  '((((type tty) (class mono)))
    (t (:background "magenta"))) "")
(setq visible-mark-max 2)
(setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))
(require 'visible-mark)

;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
;; (global-set-key "\C-x\ \C-r" 'recentf-open-files)


;;; Behavior

;; Auto-save
(add-hook 'focus-out-hook 'save-buffer)

;; Register marking/jumping, closer to vim
(global-set-key (kbd "C-S-M") 'point-to-register)
;; Hmm, M-J is needed for sp-join-sexp
;; (global-set-key (kbd "M-J") 'jump-to-register)
(global-set-key (kbd "C-S-J") 'jump-to-register)

;; ISpell (I)
(global-set-key (kbd "C-S-i") 'flycheck-next-error)

(global-set-key (kbd "C-M-_") 'text-scale-decrease)
(global-set-key (kbd "C-M-+") 'text-scale-increase)


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
(global-set-key (kbd "M-Q") 'unfill-paragraph)
;; (global-set-key (kbd "M-S-q") 'unfill-paragraph)

;; Spelling/grammar help
;; https://github.com/mhayashi1120/Emacs-langtool
;; ENABLE
;; (prelude-require-package 'langtool)

(setq tab-stop-list (number-sequence 2 200 2))

;; Zsh, hopefully
(setq indent-tabs-mode t)
(setq tab-width 2)

;; Disable prompt for kill
;; http://superuser.com/questions/354849/emacs-kill-buffer-without-prompt
;; (global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-c k") 'kill-current-buffer)


;; Don't want to suspend emacs!
;; http://superuser.com/questions/349943/how-to-awake-emacs-gui-after-pressing-ctrlz#349997
(global-unset-key (kbd "C-z"))

(require 'dot-mode)
;; (require 'dot-mode)
;; https://www.emacswiki.org/emacs/dot-mode.el
;; C-.  C-M-.  C-c.
(autoload 'dot-mode "dot-mode" nil t)
;; (dot-mode t)
(global-dot-mode t)


;; Make number colorful.
;; ENABLE??
;; (require 'highlight-numbers)
;; (add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; Prelude badly sets C-- to zoom out, so keep as negative argument
(global-set-key (kbd "C--") 'negative-argument)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Line breaks are shown as pretty horizontal lines
;; https://stackoverflow.com/a/7577628/326516
(setq-default truncate-lines t)
(require 'page-break-lines)
(global-page-break-lines-mode)

;; Highlight across multiple buffers
;; https://stackoverflow.com/questions/15415504/highlighting-multiple-buffers-in-emacs
;; https://www.emacswiki.org/emacs/HighlightLibrary
;; ENABLE??
;; (require 'highlight)


;; https://github.com/ryuslash/mode-icons
(require 'mode-icons)

;; https://github.com/domtronn/all-the-icons.el
(require 'all-the-icons)

;; Decide on neotree, treemacs
;; ENABLE??

(require 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; http://emacs.stackexchange.com/questions/13662/a-confirmation-after-c-x-c-c
;; (setq confirm-kill-emacs 'yes-or-no-p)
(global-unset-key (kbd "C-x C-c"))

;; http://stackoverflow.com/questions/6464738/how-can-i-switch-focus-after-buffer-split-in-emacs
(global-set-key "\C-x2"
                (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key "\C-x3"
                (lambda () (interactive) (split-window-horizontally) (other-window 1)))




;;; WINDOWING

;; Ace Window
;; https://github.com/abo-abo/ace-window
(require 'ace-window)

(global-set-key (kbd "M-o") 'ace-window)
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

(global-set-key (kbd "C-<tab>") 'aw-flip-window)
;; (global-set-key (kbd "M-<tab>") 'ace-window)


(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)

(global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)

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

;; Shuffle/swap windows around
(require 'buffer-move)
;; https://github.com/bbatsov/prelude/issues/106
(global-set-key (kbd "C-S-<up>")     'buf-move-up)
(global-set-key (kbd "C-S-<left>")   'buf-move-left)
(global-set-key (kbd "C-S-<down>")   'buf-move-down)
(global-set-key (kbd "C-S-<right>")  'buf-move-right)

;; move line up/down (already enabled) -- M-S-up
;; move-text-down

(defun delete-window-balancedly ()
  (interactive)
  (delete-window)
  (balance-windows))
(defun kill-window-balancedly ()
  (interactive)
  (kill-current-buffer)
  (delete-window)
  (balance-windows))
;; (global-set-key (kbd "C-z")   'delete-window-balancedly)
;; Background window (Z: like shell's C-z)
(global-set-key (kbd "C-S-z") 'delete-window-balancedly)
;; Kill (K)
(global-set-key (kbd "C-S-k") 'kill-window-balancedly)


;; Window buffer switching (O: Only)
(global-set-key (kbd "C-S-o") 'delete-other-windows) ; think "Only"
;; Just use C-c left-arrow
;; (global-set-key (kbd "C-S-g") 'winner-undo)
;; (global-set-key (kbd "C-S-+") 'balance-windows)



;; Window buffer switching (O: Only)
(global-set-key (kbd "C-S-o") 'delete-other-windows) ; think "Only"
;; Just use C-c left-arrow

;; Should change focus to new window.
(defun split-window-balancedly ()
  (interactive)
  (split-window-horizontally)
  (balance-windows)
  (other-window 1))
;; New window (N)
(global-set-key (kbd "C-S-n") 'split-window-balancedly)

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
(global-set-key (kbd "C-S-E") 'scroll-up-stay)
(global-set-key (kbd "C-S-Y") 'scroll-down-stay)


;; Camel, Kebab cases
;; https://stackoverflow.com/a/27422814/326516
(require 'string-inflection)
(global-set-key (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
(global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
(global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle) ;; Cycle through Java styles
(global-set-key (kbd "C-c K") 'string-inflection-kebab-case) ;; Cycle through Java styles


(defun jump-to-bottom ()
  (interactive)
  (move-to-window-line-top-bottom)
  (move-to-window-line-top-bottom))

;; smartparens overrides M-r, so changing default
(global-set-key "\M-R" 'move-to-window-line-top-bottom)
(global-set-key "\M-\C-R" 'jump-to-bottom)
;; Since already holding M-S-R, enable recenter (usually C-l) to also be M-S
(global-set-key (kbd "M-L") 'recenter-top-bottom)


;;; Commenter: still stuggling with this
(require 'comment-dwim-2)
(global-set-key (kbd "M-;") 'comment-dwim-2)



;; Typo fancy typography/punctuation
;; https://github.com/jorgenschaefer/typoel
;; ENABLE??
;; (require 'typo)
;; (setq-default typo-language "English")
;; (global-set-key (kbd "C-c T") 'typo-mode)
;; ISSUE: Need to auto-enter typo-mode only while inside strings.


;; Simpler attempt at typography.
(global-set-key (kbd "C-c '") "’")
(global-set-key (kbd "C-c `'") "‘")
(global-set-key (kbd "C-c \"") "“")
(global-set-key (kbd "C-c /") "”")
(global-set-key (kbd "C-c -") "—")


;;; Git

;; https://github.com/emacsorphanage/git-messenger
(require 'popup)
(require 'git-messenger)
(require 'magit)
(require 'git-timemachine)
(require 'diff-hl)
(global-diff-hl-mode)

;; Magit: came with Super-based shortcuts; use C-c g ... instead
;; maGit (G)
(global-set-key (kbd "C-S-g") 'magit-status)
(global-set-key (kbd "C-c C-g B") 'github-browse-file)
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



;; Select/highlight with easy-kill
;; https://github.com/leoliu/easy-kill
;; http://stackoverflow.com/a/36631886/326516
(require 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)
(global-set-key [remap kill-ring-save] 'easy-kill)
;; (global-set-key [remap kill-ring-save] 'easy-mark)


;; Hide-Show (V: visible), like folding
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-S-v H") 'hs-hide-all)
(global-set-key (kbd "C-S-v S") 'hs-show-all)
(global-set-key (kbd "C-S-v h") 'hs-hide-block)
(global-set-key (kbd "C-S-v s") 'hs-show-block)
(global-set-key (kbd "C-S-v t") 'hs-toggle-hiding)
(global-set-key (kbd "C-S-v v") 'hs-toggle-hiding)



;;; Search/jump

(require 'jump-char)
;; But what about `back-to-indentation' (bound to M-m by default)?
;; You should customize C-a to toggle between indentation and
;; beginning of line like a civilized human being.
(global-set-key [(meta m)] 'jump-char-forward)
;; (global-set-key [(shift meta m)] 'jump-char-backward)


;;; Key Chords

;; Decide on using key-chords
;; Key-chords slow down typing of some natural characters; oh well.
;; It's a built-in feature of emacs, so shoud be pretty first-class.
;; Best to just use with a leader -like key for now.  Note that the
;; delay is really short, so combos on the same hand might be too slow
;; to type.
(require 'key-chord)
(key-chord-mode +1)

(setq key-chord-two-keys-delay .1
      key-chord-one-key-delay .2) ; defaults

(key-chord-define-global ",b" 'crux-switch-to-previous-buffer)
(key-chord-define-global ",c" 'avy-goto-word-1)
(key-chord-define-global ",n" 'neotree-toggle)
(key-chord-define-global ",f" 'windmove-right)
(key-chord-define-global ",s" 'windmove-left)
(key-chord-define-global ",p" 'crux-switch-to-previous-buffer)
(key-chord-define-global ",," 'aw-flip-window)

(global-set-key (kbd "C-S-x") 'avy-goto-word-1)
;; (global-set-key (kbd "C-S-x") 'crux-switch-to-previous-buffer)


;; avy allows us to effectively navigate to visible things
(require 'avy)
(setq avy-background t)
(setq avy-style 'at-full)

;; anzu-mode enhances isearch & query-replace by showing total matches and current match position
;; If we ever want to abandon swiper, anzu will be useful
;; (require 'anzu)
;; (global-anzu-mode)
;; (global-set-key (kbd "M-%") 'anzu-query-replace)
;; (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

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


;; (key-chord-define-global ",u" 'undo-tree-visualize)
;; (key-chord-define-global ",x" 'execute-extended-command)
;; (key-chord-define-global ",y" 'browse-kill-ring)

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

;; Ace window
;; https://github.com/abo-abo/ace-window
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-dispatch-always t)
(setq aw-scope 'frame)
(defvar aw-dispatch-alist
  '((?x aw-delete-window " Ace - Delete Window")
    (?m aw-swap-window " Ace - Swap Window")
    (?n aw-flip-window)
    (?v aw-split-window-vert " Ace - Split Vert Window")
    (?b aw-split-window-horz " Ace - Split Horz Window")
    (?i delete-other-windows " Ace - Maximize Window")
    (?o delete-other-windows))
  "List of actions for `aw-dispatch-default'.")


;;; Languages

;;; Rainbow Parens (already part of prelude?)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(rainbow-delimiters-mode +1)

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
(setq recentf-save-file (expand-file-name "recentf" savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(flyspell-mode +1)

;; enable change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)



;; TODO
;; https://www.emacswiki.org/emacs/ShowParenMode
;; (setq show-paren-delay 0)
;; (show-paren-mode 0)
;; (show-smartparens-mode 0)



;;; Completion
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

(with-eval-after-load 'company-quickhelp
  (company-quickhelp-terminal-mode 1))

;; (require 'pos-tip) ; just a dependency package of quickhelp
;; (require 'company-quickhelp)
;; (company-quickhelp-mode 1)



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




;; Clojure

;; Still need to highleight and press TAB to make work.
(setq clojure-align-forms-automatically t)

;; NOTE: also installed to ~/.lein/profiles.clj: kibit, eastwood
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(require 'cider)
(require 'cider-eval-sexp-fu)
(require 'clj-refactor)
;; (require 'clojure-snippets) ; yas for clojure
;; (prelude-require-package 'clojure-cheatsheet)
(require 'flycheck-clojure)
;; (require 'company-flx)
(require 'flycheck-joker)
(require 'kibit-helper)
;; (require 'sotclojure)
(require 'clojure-mode-extra-font-locking)

(setq error-tip-notify-keep-messages t)

;; For kondo: https://github.com/borkdude/flycheck-clj-kondo#multiple-linters
(require 'flycheck-clj-kondo)
(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
(dolist (checkers '((clj-kondo-clj . clojure-joker)
                    (clj-kondo-cljs . clojurescript-joker)
                    (clj-kondo-cljc . clojure-joker)
                    (clj-kondo-edn . edn-joker)))
  (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))

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
       (cljr-add-keybindings-with-prefix "C-c m")
       ;; (global-set-key (kbd "C-c R") 'cljr-helm)
       ;; (global-set-key (kbd "C-S-r") 'cljr-helm)
       ;; (global-set-key (kbd "C-c r") 'cljr-helm)
       ;; (global-set-key (kbd "C-S-T") 'cider-test-commands-map)
       ;; Disable flycheck next error in favor of Cider
       (global-set-key (kbd "C-c C-n") 'cider-ns-map)
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

(require 'discover-clj-refactor)

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



;; Other non-programming modes

(defun my-beginning-of-defun ()
  "Jump to start of name of function, since often want to search it."
  (interactive)
  (beginning-of-line)
  (beginning-of-defun)
  (forward-word 2)
  (backward-word))
;; (global-set-key (kbd "C-M-a") 'beginning-of-defun) ; replaced
(global-set-key (kbd "C-M-a") 'my-beginning-of-defun)


;; Copy filename to clipboard
;; https://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun my-copy-filename ()
  "Copy current buffer file name to clipboard"
  (interactive)
  (let ((fname (buffer-file-name)))
    (kill-new fname)
    (message "Copied buffer file name '%s' to the clipboard." fname)))
(global-set-key (kbd "C-c c") 'my-copy-filename)

(require 'dumb-jump)

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
(global-set-key (kbd "C-x ]") 'my-forward-jump-to-line-break)
(defun my-backward-jump-to-line-break ()
  (interactive)
  (backward-char 2)
  (backward-page)
  (backward-char)
  (recenter-top-bottom 1))
(global-set-key (kbd "C-x [") 'my-backward-jump-to-line-break)




(provide 'init)

;;; init ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
