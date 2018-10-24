(require 'package)

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

 (load (concat user-emacs-directory "packages.el"))

(setq system-config (concat user-emacs-directory system-name ".el")
      user-config (concat user-emacs-directory user-login-name ".el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ebe16ce036d354549df68f90586854517b55fbeb738b60708f6f4554e540b801" "2e898e39728a53480a496370cd05fc9923ca1614" "17e8ebc51fc3791f83b2a8853d90a947bca4d814603e6ea8aa42275ff37d915a" "ebf26995fca2e06dfcaaa523e5c9d07f5594f2be" "c0fccdad64d0c5d45b77ae4183076f813fa700f7b474e859aa2dad83955205fa" "b3221d30b128ea2c03ce2f6bf5239b979ca1b6f1" default)))
 '(package-selected-packages
   (quote
    (clojure-mode svg rjsx-mode js2-mode sass-mode elisp-format exec-path-from-shell lua-mode feature-mode persistant-scratch elpy dumb-jump docker-compose-mode dockerfile-mode docker-mode json-mode jade-mode graphql-mode yaml-mode helm-ag ag yasnippet which-key web-mode virtualenvwrapper use-package sx string-inflection spaceline smex smartparens scratch rainbow-delimiters racket-mode phi-search-mc phi-rectangle paredit org-journal magit ido-ubiquitous hydra helm-projectile guide-key google-this flycheck flx-ido expand-region editorconfig company centered-cursor-mode)))
 '(safe-local-variable-values (quote ((pyvenv-workon . ~/\.env/emacs)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (file-exists-p system-config) (load system-config))
(when (file-exists-p user-config) (load user-config))
