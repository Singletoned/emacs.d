(use-package which-key
  :ensure t
  :config (which-key-mode))


(use-package ef-themes
  :ensure t
  :init (load-theme 'ef-bio :no-confirm))

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
  :config (progn (setq magit-git-executable "/opt/homebrew/bin/git")
		 (magit-add-section-hook 'magit-status-sections-hook
                                         'magit-insert-unpushed-to-upstream
                                         'magit-insert-unpushed-to-upstream-or-recent 'replace))
  :bind (("C-x g" . magit-status)))

(use-package centered-cursor-mode
  :ensure t
  :config (progn (global-centered-cursor-mode) (setq ccm-recenter-at-end-of-file t))
)

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package
  phi-rectangle
  :ensure t
  :bind (("C-x r C-SPC" . phi-rectangle-set-mark-command )
         ;; ("C-x r C-x" . rm-exchange-point-and-mark)
         ("C-x r C-k" . phi-rectangle-kill-region)
         ("C-x r S-c" . phi-rectangle-kill-ring-save)))

