;; https://github.com/oliyh/re-jump.el

;; But it's broken due to:
;; https://github.com/clojure-emacs/cider/issues/2912
;; So cider appears to not care, and the re-jump project looks dead. Thus this.

(require 'cider-util)
(require 'cider-resolve)
(require 'cider-client)
(require 'cider-common)
(require 'cider-find)
(require 'clojure-mode)

(defun re-frame-jump-to-reg ()
  (interactive)
  (let* (
	 ;; ":innsmouth.events/go-send-agreements"
	 ;; HACK This is the questionable part that not right in cider-symbol-at-point
	 (kw (nrepl-dict-get (cider-nrepl-sync-request:eval (thing-at-point 'symbol)
							    (cider-current-connection)
							    (cider-current-ns))
			     "value"))
	 ;; "innsmouth.events"
         (ns-qualifier (and (string-match "^:+\\(.+\\)/.+$" kw) (match-string 1 kw)))
	 ;; "innsmouth.events"
         (kw-ns (if ns-qualifier
                    (cider-resolve-alias (cider-current-ns) ns-qualifier)
                  (cider-current-ns)))
	 ;; "::go-send-agreements"
         (kw-to-find (concat "::" (replace-regexp-in-string "^:+\\(.+/\\)?" "" kw))))

    (when (and ns-qualifier (string= kw-ns (cider-current-ns)))
      (error "Could not resolve alias \"%s\" in %s" ns-qualifier (cider-current-ns)))

    (progn (cider-find-ns "-" kw-ns)
           (search-forward-regexp (concat "reg-[a-zA-Z-]*[ \\\n]+" kw-to-find) nil 'noerror))))

(provide 'my-re-jump)
