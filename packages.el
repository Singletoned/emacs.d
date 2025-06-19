(require 'package)

(add-to-list
  'package-archives
  '("melpa" . "https://melpa.org/packages/")
  t)
(package-initialize)

(use-package
  yaml-mode
  :ensure t
  :init
  (dolist (extension '("yml" "yaml" "sls"))
    (add-to-list
      'auto-mode-alist
      `(,(concat "\\." extension "$") . yaml-mode))))

(use-package
  web-mode
  :ensure t
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
  :ensure t
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
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (sp-pair "\"" "\""
    :actions '(insert wrap autoskip navigate)
    :post-handlers nil)
  (sp-pair "'" "'"
    :actions '(insert wrap autoskip navigate)
    :post-handlers nil))

(use-package
  whitespace
  :ensure t
  :config
  (progn
    (set
      'whitespace-style
      '(trailing tabs lines-tail indentation: :space face))
    (set 'whitespace-line-column 78)
    (setq whitespace-style '(face tabs trailing tab-mark))
    (global-whitespace-mode)))

(use-package powerline :ensure t :config (powerline-center-theme))

(use-package
  virtualenvwrapper
  :ensure t
  :config
  (progn
    (venv-initialize-interactive-shells) ;; if you want interactive shell support
    (venv-initialize-eshell) ;; if you want eshell support
    (setq venv-location
      (file-name-as-directory (expand-file-name ".envs" "~")))))

(use-package
  ls-lisp
  ;;  :ensure t
  :config
  (progn
    (setq ls-lisp-use-insert-directory-program nil)
    (setq ls-lisp-verbosity nil)))

(use-package
  projectile
  :ensure t
  :init
  (progn
    (projectile-global-mode)
    (set
      'projectile-mode-line
      '(:eval (format " P[%s]" (projectile-project-name))))))

(use-package
  multiple-cursors
  :ensure t
  :config (multiple-cursors-mode)
  :bind
  (("C->" . mc/mark-next-like-this)
    ("C-<" . mc/mark-previous-like-this)))

;; (use-package
;;   phi-search
;;   :ensure t
;;   :bind (("C-s" . phi-search)
;;          ("C-r" . phi-search-backward)))

;; (use-package
;;   phi-search-mc
;;   :ensure t
;;   :bind (("C-c C-m" . phi-search-mc/mark-all)))

(use-package
  flycheck
  :ensure t
  :init
  (progn
    (setq-default flycheck-disabled-checkers '(python-mypy python-pylint python-flake8))
    (setq python-flymake-command '("ruff" "check" "--quiet" "--stdin-filename=stdin" "-"))
    (setq flycheck-python-flake8-executable
      "~/.envs/emacs/bin/flake8")
    (setq flycheck-sh-shellcheck-executable "shellcheck")
    (setq flycheck-idle-change-delay 5)
    (global-flycheck-mode)))


;; (use-package
;;   flycheck-aspell
;;   :ensure t
;;   :init (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic))

(use-package
  string-inflection
  :ensure t
  :bind ("C-c i" . string-inflection-cycle))

(use-package scratch :ensure t)

(use-package jade-mode :ensure t)
(use-package json-mode :ensure t)
(use-package racket-mode)

(use-package sx :ensure t)

(use-package hydra :ensure t)

(use-package
  dockerfile-mode
  :ensure t
  :init
  (progn
    (add-to-list
      'auto-mode-alist
      '("Dockerfile\\'" . dockerfile-mode))
    (add-to-list
      'auto-mode-alist
      '("\\.docker\\'" . dockerfile-mode))))

(use-package editorconfig :ensure t :config (editorconfig-mode 1))

(use-package
  markdown-mode
  :ensure t
  :init (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  :bind
  (:map
    markdown-mode-command-map
    ("M-p" . backward-paragraph)
    ("M-n" . forward-paragraph)))

(use-package
  guide-key
  :ensure t
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
    (setq guide-key/popup-window-position 'bottom))
  :init (guide-key-mode 1))

(use-package
  flx-ido
  :ensure t
  :init
  (progn
    (ido-mode t)
    (ido-everywhere t)
    (flx-ido-mode t)))

(use-package
  org-journal
  :ensure t
  :custom
  (org-journal-dir "~/.journal/")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package
  rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'racket-repl-mode-hook #'rainbow-delimiters-mode)))

(use-package racket-mode :ensure t)

(use-package which-key :ensure t :init (which-key-mode))

;; (use-package
;;   company
;;   :ensure t
;;   :init (company-mode))

(use-package
  spaceline
  :ensure t
  :init
  (progn
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)))

(use-package google-this :ensure t)

(use-package
  dumb-jump
  :ensure t
  :config (setq dumb-jump-selector 'helm)
  :bind
  (("M-g o" . dumb-jump-go-other-window)
    ("M-g j" . dumb-jump-go)
    ("M-g i" . dumb-jump-go-prompt)
    ("M-g x" . dumb-jump-go-prefer-external)
    ("M-g z" . dumb-jump-go-prefer-external-other-window)))

(use-package deadgrep :ensure t)

(use-package s :ensure t)

(use-package
  ef-themes
  :ensure t
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
  :ensure t
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
  :ensure t
  :config
  (progn
    (global-centered-cursor-mode)
    (setq ccm-recenter-at-end-of-file t)))

(use-package
  expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package
  phi-rectangle
  :ensure t
  :bind
  (("C-x r C-SPC" . phi-rectangle-set-mark-command)
    ;; ("C-x r C-x" . rm-exchange-point-and-mark)
    ("C-x r C-k" . phi-rectangle-kill-region)
    ("C-x r S-c" . phi-rectangle-kill-ring-save)))

(use-package
  consult
  :ensure t
  ;; Other good things to bind: consult-ripgrep, consult-line-multi,
  ;; consult-history, consult-outline
  :bind
  (("C-x b" . consult-buffer) ; orig. switch-to-buffer
    ("M-y" . consult-yank-pop) ; orig. yank-pop
    ("C-s" . consult-line)) ; orig. isearch
  :config (setq consult-narrow-key "<"))

(use-package vertico :ensure t :config (vertico-mode))

(use-package
  elisp-autofmt
  :ensure t
  :config
  (setq
    elisp-autofmt-style 'fixed
    indent-tabs-mode nil
    lisp-indent-function nil
    lisp-indent-offset 2))
