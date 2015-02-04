(require 'use-package)

(use-package yaml-mode
  :init (dolist (extension '("yml" "yaml" "sls"))
          (add-to-list 'auto-mode-alist `(,(concat "\\." extension "$") . yaml-mode))))



