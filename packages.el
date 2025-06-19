(use-package
  yaml-mode
  :straight t
  :init
  (dolist (extension '("yml" "yaml" "sls"))
    (add-to-list
      'auto-mode-alist
      `(,(concat "\\." extension "$") . yaml-mode))))

(use-package
  web-mode
  :straight t
  :init
  (dolist
    (extension
      '
      ("html"
        "phtml"
        "tpl"
        "php"
        "jsp"
        "as[cp]x"
        "erb"
        "mustache"
        "djhtml"))
    (add-to-list
      'auto-mode-alist
      `(,(concat "\\." extension "\\'") . web-mode)))
  :config (setq web-mode-engines-alist '(("django" . "\\.html\\'"))))

(use-package
  yasnippet
  :straight t
  :hook (snippet-mode . (lambda () (setq require-final-newline nil)))
  :config
  (progn
    (set
      'yas-snippet-dirs
      `(,(concat user-emacs-directory "snippets")))
    (yas-global-mode t)
    (dolist
      (pair '(("<tab>" . nil) ("TAB" . nil) ("M-§" . yas-expand)))
      (define-key yas-minor-mode-map (kbd (car pair)) (cdr pair)))))

(use-package
  smartparens
  :straight t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (sp-pair
    "\""
    "\""
    :actions '(insert wrap autoskip navigate)
    :post-handlers nil)
  (sp-pair
    "'"
    "'"
    :actions '(insert wrap autoskip navigate)
    :post-handlers nil))

(use-package
  whitespace
  :straight t
  :config
  (progn
    (set
      'whitespace-style
      '(trailing tabs lines-tail indentation: :space face))
    (set 'whitespace-line-column 78)
    (setq whitespace-style '(face tabs trailing tab-mark))
    (global-whitespace-mode)))

(use-package powerline :straight t :config (powerline-center-theme))

(use-package
  virtualenvwrapper
  :straight t
  :config
  (progn
    (venv-initialize-interactive-shells) ;; if you want interactive shell support
    (venv-initialize-eshell) ;; if you want eshell support
    (setq venv-location
      (file-name-as-directory (expand-file-name ".envs" "~")))))

(use-package
  ls-lisp
  ;;  :straight t
  :config
  (progn
    (setq ls-lisp-use-insert-directory-program nil)
    (setq ls-lisp-verbosity nil)))

(use-package project
  :config
  (setq project-vc-merge-submodules nil))

(use-package
  multiple-cursors
  :straight t
  :config (multiple-cursors-mode)
  :bind
  (("C->" . mc/mark-next-like-this)
    ("C-<" . mc/mark-previous-like-this)))

;; (use-package
;;   phi-search
;;   :straight t
;;   :bind (("C-s" . phi-search)
;;          ("C-r" . phi-search-backward)))

;; (use-package
;;   phi-search-mc
;;   :straight t
;;   :bind (("C-c C-m" . phi-search-mc/mark-all)))


(use-package
  flycheck
  :straight t
  :init
  (progn
    (setq-default flycheck-disabled-checkers
      '(python-mypy python-pylint python-flake8))
    (setq python-flymake-command
      '("ruff" "check" "--quiet" "--stdin-filename=stdin" "-"))
    (setq flycheck-python-flake8-executable
      "~/.envs/emacs/bin/flake8")
    (setq flycheck-sh-shellcheck-executable "shellcheck")
    (setq flycheck-idle-change-delay 5)
    (global-flycheck-mode)))


;; (use-package
;;   flycheck-aspell
;;   :straight t
;;   :init (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic))

(use-package
  string-inflection
  :straight t
  :bind ("C-c i" . string-inflection-cycle))

(use-package scratch :straight t)

(use-package jade-mode :straight t)
(use-package json-mode :straight t)
(use-package racket-mode)

(use-package sx :straight t)

(use-package hydra :straight t)

(use-package
  dockerfile-mode
  :straight t
  :init
  (progn
    (add-to-list
      'auto-mode-alist
      '("Dockerfile\\'" . dockerfile-mode))
    (add-to-list
      'auto-mode-alist
      '("\\.docker\\'" . dockerfile-mode))))

(use-package editorconfig :straight t :config (editorconfig-mode 1))

(use-package
  markdown-mode
  :straight t
  :init
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  (setq markdown-asymmetric-header t)
  :bind
  (:map
    markdown-mode-map
    ("M-p" . backward-paragraph)
    ("M-n" . forward-paragraph)))

(use-package
  guide-key
  :straight t
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
    (setq guide-key/popup-window-position 'bottom))
  :init (guide-key-mode 1))

(use-package
  flx-ido
  :straight t
  :init
  (progn
    (ido-mode t)
    (ido-everywhere t)
    (flx-ido-mode t)))

(use-package
  org-journal
  :straight t
  :custom
  (org-journal-dir "~/.journal/")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package
  rainbow-delimiters
  :straight t
  :init
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'racket-repl-mode-hook #'rainbow-delimiters-mode)))

(use-package racket-mode :straight t)

(use-package which-key :straight t :init (which-key-mode))

;; (use-package
;;   company
;;   :straight t
;;   :init (company-mode))

(use-package
  spaceline
  :straight t
  :init
  (progn
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)))

(use-package google-this :straight t)

(use-package
  dumb-jump
  :straight t
  :defer t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-force-searcher 'rg
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  :bind
  (("M-g o" . dumb-jump-go-other-window)
    ("M-g j" . dumb-jump-go)
    ("M-g i" . dumb-jump-go-prompt)
    ("M-g x" . dumb-jump-go-prefer-external)
    ("M-g z" . dumb-jump-go-prefer-external-other-window)))

(use-package deadgrep :straight t)

(use-package s :straight t)

(use-package
  ef-themes
  :straight t
  :init (load-theme 'ef-bio :no-confirm))

(use-package
  hippie-expand
  :init
  (progn
    (setq my-hippie-expand-line
      (make-hippie-expand-function
        '(try-expand-line try-expand-line-all-buffers)
        t))
    (bind-key "±" my-hippie-expand-line)
    (setq hippie-expand-try-functions-list
      '
      (try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-all-abbrevs)))
  :bind (("M-/" . hippie-expand) ("§" . hippie-expand)))

(use-package
  magit
  :straight t
  :init
  (progn
    (setq magit-commit-show-diff nil))
  :config
  (progn
    (setq magit-git-executable "/opt/homebrew/bin/git")
    (magit-add-section-hook
      'magit-status-sections-hook
      'magit-insert-unpushed-to-upstream
      'magit-insert-unpushed-to-upstream-or-recent
      'replace))
  :bind (("C-x g" . magit-status)))

(use-package
  centered-cursor-mode
  :straight t
  :config
  (progn
    (global-centered-cursor-mode)
    (setq ccm-recenter-at-end-of-file t)))

(use-package
  expand-region
  :straight t
  :bind (("C-=" . er/expand-region)))

(use-package
  phi-rectangle
  :straight t
  :bind
  (("C-x r C-SPC" . phi-rectangle-set-mark-command)
    ;; ("C-x r C-x" . rm-exchange-point-and-mark)
    ("C-x r C-k" . phi-rectangle-kill-region)
    ("C-x r S-c" . phi-rectangle-kill-ring-save)))

(use-package
  consult
  :straight t
  :bind
  (("C-x b" . consult-buffer) ; orig. switch-to-buffer
    ("M-y" . consult-yank-pop) ; orig. yank-pop
    ("C-s" . consult-line) ; orig. isearch
    )
  :config
  ;; Avoid completion-style interference
  (setq
    completion-styles '(basic substring)
    completion-category-defaults nil
    completion-category-overrides '((consult-location (styles basic substring)))))

(use-package vertico :straight t :config (vertico-mode))

(use-package
  elisp-autofmt
  :straight t
  :config
  (setq
    elisp-autofmt-style 'fixed
    indent-tabs-mode nil
    lisp-indent-function nil
    lisp-indent-offset 2))

(use-package
  jinx
  :straight t
  :init
  (dolist (hook '(text-mode-hook))
    (add-hook hook #'jinx-mode))

  :config (setq jinx-menu-suggestions 5)
  :bind (("M-$" . jinx-correct) ("C-M-$" . jinx-languages)))


(use-package
  just-ts-mode
  :straight t
  :config (just-ts-mode-install-grammar))

(use-package s :straight t)

(use-package
  smerge-mode
  :ensure nil ;; built-in
  :bind
  (:map
    smerge-mode-map
    ("C-c m n" . smerge-next)
    ("C-c m p" . smerge-prev)
    ("C-c m u" . smerge-keep-upper)
    ("C-c m l" . smerge-keep-lower)
    ("C-c m a" . smerge-keep-all)
    ("C-c m RET" . smerge-keep-current)
    ("C-c m r" . smerge-refine)
    ("C-c m e" . smerge-ediff)))

(use-package
  aider
  :straight (:host github :repo "tninja/aider.el")
  :config
  (setq aider-args
    `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  (global-set-key (kbd "C-c a") 'aider-transient-menu))
