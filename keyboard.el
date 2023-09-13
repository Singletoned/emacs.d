(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "s-w") (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key (kbd "s-/") 'my-comment-dwim)
(global-set-key (kbd "s-[") 'unindent-dwim)
(global-set-key (kbd "s-]") 'indent-dwim)

(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-l") 'goto-line)

(global-set-key (kbd "C-x j") 'dired-jump)
(global-set-key (kbd "C-x C-j") 'dired-jump-other-window)

(global-set-key (kbd "C-x ~") (lambda () (interactive) (dired-other-window user-emacs-directory)))

(global-set-key (kbd "s-h") 'help-command)
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "C-w") 'backward-kill-word)

(global-set-key (kbd "C-c q") 'query-replace)
(global-set-key (kbd "C-c Q") 'query-replace-regexp)
(global-set-key (kbd "C-c u") 'delete-indentation)

(global-set-key (kbd "s-[") 'unindent-dwim)
(global-set-key (kbd "s-]") 'indent-dwim)
