(require 'use-package)

(use-package yaml-mode
  :init (dolist (extension '("yml" "yaml" "sls"))
          (add-to-list 'auto-mode-alist `(,(concat "\\." extension "$") . yaml-mode))))

(use-package web-mode
  :ensure t
  :init (dolist (extension '("html" "phtml" "tpl" "php" "jsp" "as[cp]x" "erb" "mustache" "djhtml"))
          (add-to-list 'auto-mode-alist `(,(concat "\\." extension "\\'") . web-mode)))
  :config (setq web-mode-engines-alist '(("django" . "\\.html\\'"))))

(use-package yasnippet
  :ensure t
  :config (progn
            (set 'yas-snippet-dirs (concat user-emacs-directory "yasnippet-snippets"))
            (yas-global-mode t)
            (dolist (pair '(("<tab>" . nil) ("TAB" . nil) ("M-§" . yas-expand)))
              (define-key yas-minor-mode-map (kbd (car pair)) (cdr pair)))))

(use-package centered-cursor-mode
  :ensure t
  :config (progn
            (global-centered-cursor-mode t)
            (setq ccm-recenter-at-end-of-file t)))

(use-package smartparens-mode
  :ensure smartparens
  :diminish smartparens-mode
  :init (progn
            (require 'smartparens-config)
            (smartparens-global-mode 1)
            (set-default 'sp-autoescape-string-quote nil))
  :bind (("C-{" . sp-select-previous-thing)
         ("C-}" . sp-select-next-thing)))

(use-package whitespace
  :ensure t
  :config (progn
            (set 'whitespace-style '(trailing tabs lines-tail indentation::space face))
            (set 'whitespace-line-column 78)
            (setq whitespace-style '(face tabs trailing tab-mark))
            (global-whitespace-mode)))

(use-package powerline
 :ensure t
 :config (powerline-center-theme))

(use-package virtualenvwrapper
  :ensure t
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
;;  :ensure t
  :config (progn
            (setq ls-lisp-use-insert-directory-program nil)
            (setq ls-lisp-verbosity nil)))

(use-package projectile
  :ensure t
  :init (progn
          (projectile-global-mode)
          (set 'projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))))
  :bind (("C-x p o" . helm-projectile)))

(use-package helm
  :ensure t
  :init (progn
            (require 'helm-config)
            (setq helm-buffer-max-length nil)
            (setq helm-split-window-default-side 'other)
            (define-key global-map [remap find-file] 'helm-find-files)
            (define-key global-map [remap occur] 'helm-occur)
            (define-key global-map [remap list-buffers] 'helm-buffers-list))
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-buffers-list)))

(use-package helm-projectile
  :ensure t
  :bind (("C-x p h". helm-projectile-find-file)))

(use-package multiple-cursors
  :ensure t
  :config (multiple-cursors-mode)
  :bind (
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package phi-search
  :ensure t
  :bind (
         ("C-s" . phi-search)
         ("C-r" . phi-search-backward)))

(use-package phi-search-mc
  :ensure t
  :bind (("C-c C-m" . phi-search-mc/mark-all)))

(use-package flycheck
  :ensure t
  :init (progn
          (setq flycheck-python-pyflakes-executable "~/.envs/emacs/bin/pyflakes")
          (setq flycheck-idle-change-delay 5)
          (global-flycheck-mode)))

(use-package string-inflection
  :ensure t
  :bind ("C-c i" . string-inflection-cycle))

(use-package phi-rectangle
  :ensure t
  :bind (
         ("C-x r C-SPC" . phi-rectangle-set-mark-command )
         ;; ("C-x r C-x" . rm-exchange-point-and-mark)
         ("C-x r C-k" . phi-rectangle-kill-region)
         ("C-x r S-c" . phi-rectangle-kill-ring-save)))

(use-package scratch
  :ensure t)

(use-package magit
  :ensure t
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :bind (("C-x g" . magit-status)))

(use-package sws-mode)
(use-package jade-mode)

(use-package sx
  :ensure t)

(use-package hydra
  :ensure t)

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package docker-mode
  :init (add-to-list 'auto-mode-alist '("\\.docker\\'" . dockerfile-mode)))

(use-package editorconfig
  :ensure t)

(use-package markdown-mode
  :ensure t
  :init (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  :bind (
         ("M-p" . backward-paragraph)
         ("M-n" . forward-paragraph)))

(use-package guide-key
  :ensure t
  :config (progn
            (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
            (setq guide-key/popup-window-position 'bottom))
  :init (guide-key-mode 1))
