(server-start)

(defvar bootstrap-version)
(let
  (
    (bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
          user-emacs-directory)))
    (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent
        'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq os-config
  (concat user-emacs-directory (symbol-name system-type) ".el"))

(when (file-exists-p os-config)
  (load os-config))

(load-file (expand-file-name "packages.el" user-emacs-directory))
(load-file (expand-file-name "functions.el" user-emacs-directory))
(load-file (expand-file-name "keyboard.el" user-emacs-directory))
(load-file (expand-file-name "settings.el" user-emacs-directory))


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
  '
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )
