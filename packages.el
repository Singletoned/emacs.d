(use-package which-key
  :ensure t
  :config (which-key-mode))


(use-package emacs
  :config (load-theme 'modus-vivendi))


(use-package haki-theme
  :ensure t
  :config (load-theme 'haki t))


(use-package
  hippie-expand
  :init (progn
          (setq my-hippie-expand-line (make-hippie-expand-function '(try-expand-line
                                                                     try-expand-line-all-buffers)
                                                                   t))
          (bind-key "±" my-hippie-expand-line)
          (setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers
                                                                      try-expand-dabbrev-from-kill
                                                                      try-expand-all-abbrevs)))
  :bind (("M-/" . hippie-expand)
         ("§" . hippie-expand)))

(use-package
  magit
  :ensure t
  :init (progn
          (setq magit-commit-show-diff nil))
  :config (progn (magit-add-section-hook 'magit-status-sections-hook
                                         'magit-insert-unpushed-to-upstream
                                         'magit-insert-unpushed-to-upstream-or-recent 'replace))
  :bind (("C-x g" . magit-status)))

(use-package centered-cursor-mode
  :ensure t
  :config (progn (global-centered-cursor-mode) (setq ccm-recenter-at-end-of-file t))
)
