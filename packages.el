(require 'use-package)

(use-package yaml-mode
  :init (dolist (extension '("yml" "yaml" "sls"))
          (add-to-list 'auto-mode-alist `(,(concat "\\." extension "$") . yaml-mode))))

(use-package web-mode
  :init (dolist (extension '("html" "phtml" "tpl" "php" "jsp" "as[cp]x" "erb" "mustache" "djhtml"))
          (add-to-list 'auto-mode-alist `(,(concat "\\." extension "\\'") . web-mode)))
  :config (setq web-mode-engines-alist '(("django" . "\\.html\\'"))))

