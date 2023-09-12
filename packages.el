;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(use-package emacs
  :config
  (load-theme 'modus-vivendi))


(use-package dracula-theme
  :ensure t
  :config

  (load-theme 'dracula t))

