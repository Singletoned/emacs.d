(server-start)

(load-file (expand-file-name "packages.el" user-emacs-directory))
(load-file (expand-file-name "functions.el" user-emacs-directory))
(load-file (expand-file-name "keyboard.el" user-emacs-directory))
(load-file (expand-file-name "settings.el" user-emacs-directory))

(setq os-config (concat user-emacs-directory (symbol-name system-type) ".el"))

(when (file-exists-p os-config) (load os-config))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in customization framework
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(orderless kind-icon corfu-terminal corfu marginalia vertico embark-consult embark consult avy which-key dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
