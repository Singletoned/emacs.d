(require 'use-package)

(use-package yaml-mode
  :init (dolist (extension '("yml" "yaml" "sls"))
          (add-to-list 'auto-mode-alist `(,(concat "\\." extension "$") . yaml-mode))))

(use-package web-mode
  :init (dolist (extension '("html" "phtml" "tpl" "php" "jsp" "as[cp]x" "erb" "mustache" "djhtml"))
          (add-to-list 'auto-mode-alist `(,(concat "\\." extension "\\'") . web-mode)))
  :config (setq web-mode-engines-alist '(("django" . "\\.html\\'"))))

(use-package yasnippet
  :config (progn
            (set 'yas/snippet-dirs (concat user-emacs-directory "yasnippet-snippets"))
            (yas-global-mode)
            (dolist (pair '(("<tab>" . nil) ("TAB" . nil) ("M-§" . yas-expand)))
              (define-key yas-minor-mode-map (kbd (car pair)) (cdr pair)))))

(use-package centered-cursor-mode
  :config (progn
            (global-centered-cursor-mode t)
            (setq ccm-recenter-at-end-of-file t)))

(use-package smartparens
  :ensure t
  :config (progn
            (smartparens-global-mode 1)
            (set-default 'sp-autoescape-string-quote nil))
  :bind (("C-{" . sp-select-previous-thing)
         ("C-}" . sp-select-next-thing)))

(use-package whitespace
  :config (progn
            (set 'whitespace-style '(trailing tabs lines-tail indentation::space face))
            (set 'whitespace-line-column 78)
            (setq whitespace-style '(face tabs trailing tab-mark))
            (global-whitespace-mode)))

(use-package virtualenvwrapper
  :config (progn
            (venv-initialize-interactive-shells) ;; if you want interactive shell support
            (venv-initialize-eshell) ;; if you want eshell support
            (setq venv-location (file-name-as-directory (expand-file-name ".envs" "~")))))

(use-package hippie-expand
  :init (progn
          (setq my-hippie-expand-line
                (make-hippie-expand-function
                 '(try-expand-line try-expand-line-all-buffers) t))
	            (bind-key "±" my-hippie-expand-line)
          (setq hippie-expand-try-functions-list
                '(try-expand-dabbrev
                  try-expand-dabbrev-all-buffers
                  try-expand-dabbrev-from-kill
                  try-expand-all-abbrevs)))
  :bind (("M-/" . hippie-expand)
         ("§" . hippie-expand)))

(use-package ls-lisp
  :config (progn
            (setq ls-lisp-use-insert-directory-program nil)
            (setq ls-lisp-verbosity nil)))

(use-package projectile
  :init (progn
          (projectile-global-mode)
          (set 'projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))))
  :bind (("C-x p o" . projectile-find-file-other-window)))

(use-package helm
  :ensure t
  :config (progn
            (require 'helm-config)
            (setq helm-buffer-max-length nil)
            (define-key global-map [remap find-file] 'helm-find-files)
            (define-key global-map [remap occur] 'helm-occur)
            (define-key global-map [remap list-buffers] 'helm-buffers-list))
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-buffers-list)))
(use-package helm-projectile
  :bind (("C-x p h". helm-projectile-find-file)))

(use-package multiple-cursors
  :config (multiple-cursors-mode)
  :bind (
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package phi-search
  :ensure t
  :bind (
         ("C-s" . phi-search)
         ("C-r" . phi-search-backward)))

(use-package discover
  :ensure t
  :init (global-discover-mode t))

(use-package flycheck
  :ensure t
  :init (progn
          (add-to-list 'flycheck-disabled-checkers 'python-pycompile)
          (setq flycheck-python-pyflakes-executable "~/.envs/emacs/bin/pyflakes")
          (setq flycheck-idle-change-delay)
          (global-flycheck-mode)))
