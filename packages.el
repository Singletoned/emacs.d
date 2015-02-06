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

(use-package whitespace-mode
  :config (progn
            (set 'whitespace-style '(trailing tabs lines-tail indentation::space face))
            (set 'whitespace-line-column 78)
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
          (setq hippie-expand-try-functions-list
                '(try-expand-dabbrev
                  try-expand-dabbrev-all-buffers
                  try-expand-dabbrev-from-kill
                  try-expand-all-abbrevs)))
  :bind (("M-/" . hippie-expand)
         ("§" . hippie-expand)
         ("±" . my-hippie-expand-line)))
