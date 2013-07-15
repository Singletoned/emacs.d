(server-start)

;; Set shell path

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Load Path

(add-to-list 'load-path (concat user-emacs-directory "yasnippet"))
(add-to-list 'load-path (concat user-emacs-directory "eproject"))


;; Requirements

(require 'saveplace)
(require 'whitespace)
(require 'smartparens)
(require 'smartparens-config)
(require 'uniquify)
(require 'centered-cursor-mode)
(require 'rect-mark)
(require 'yasnippet)
(require 'scratch)
(require 'eproject)
(require 'eproject-extras)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(require 'sws-mode)
(require 'jade-mode)

;; Colors

(set 'custom-safe-themes
     (cons (with-temp-buffer
               (insert-file-contents
                (concat user-emacs-directory "oldskool-theme.el"))
               (sha1 (current-buffer)))
           custom-safe-themes))

(load-theme 'oldskool)


;; Config

(global-hl-line-mode t)
(set-default 'hl-line-sticky-flag nil)
(global-linum-mode t)
(show-paren-mode t)
(set-default 'indicate-empty-lines t)
(delete-selection-mode nil)
(tool-bar-mode -1)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)

(set-default 'require-final-newline t)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      uniquify-buffer-name-style 'forward
      oddmuse-directory "~/.emacs.d/oddmuse"
      save-place-file "~/.emacs.d/places"
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      diff-switches "-u")


;; Centered Cursor

(global-centered-cursor-mode t)
(setq ccm-recenter-at-end-of-file t)


;; Whitespace

(set 'whitespace-style '(trailing tabs lines-tail indentation::space face))
(set 'whitespace-line-column 78)
(global-whitespace-mode)


;; Tabs

(set-default 'indent-tabs-mode nil)

;; Try really, really hard to force tabs to be 4 spaces

(set-default 'tab-width 4)
(set-default 'python-indent 4)
(set-default 'python-tab-width 4)
(set 'default-tab-width 4)
(set-default 'python-indent-guess-indent-offset nil)


;; Extra Keywords

(font-lock-add-keywords
nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
       1 font-lock-warning-face t)))


;; Smex

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)


;; SmartParens

(smartparens-global-mode 1)

(set-default 'sp-autoescape-string-quote nil)


;; YASnippet

(set-default 'yas/trigger-key "M-§")
(set 'yas/snippet-dirs (concat user-emacs-directory "yasnippet/snippets"))
(yas/initialize)


;; EProject

(define-project-type git (generic)
  (look-for ".git")
  :list-project-files (lambda (root) (lambda (root file) (eproject-list-project-files-git root))))

(defun* eproject-list-project-files-git (&optional (root (eproject-root)))
  (mapcar (lambda (name) (concat root name))
          (split-string
           (shell-command-to-string
            (concat "git --git-dir=" root ".git ls-files")))))

(define-project-type python (generic)
  (look-for "setup.py"))

(define-project-type bzr (generic)
  (look-for ".bzr"))

(set 'eproject-completing-read-function 'eproject--ido-completing-read)

(global-set-key (kbd "C-x p RET") 'eproject-revisit-project)
(global-set-key (kbd "C-x p o") 'eproject-find-file)
(global-set-key (kbd "C-x p b") 'eproject-ibuffer)
(global-set-key (kbd "C-x p r") (lambda () (interactive) (find-file (eproject-root))))


;; HippieExpand Completion

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "§") 'hippie-expand)

(global-set-key (kbd "±") (make-hippie-expand-function
                                          '(try-expand-line
                                            try-expand-line-all-buffers) t))

(setq hippie-expand-try-functions-list 
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs))


;; Ido mode

(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)


;; RectMark

(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-k") 'rm-kill-region)
(global-set-key (kbd "C-x r S-x") 'rm-kill-region)
(global-set-key (kbd "C-x r S-c") 'rm-kill-ring-save)


;; Keyboard shortcuts

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "s-w")
                (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key (kbd "s-<return>") 'eol-and-next-line-and-indent)
(global-set-key (kbd "s-/") 'my-comment-dwim)
(global-set-key (kbd "s-[") 'unindent-dwim)
(global-set-key (kbd "s-]") 'indent-dwim)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Some Yegge-isations
(global-set-key (kbd "s-h") 'help-command)
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)


;; Functions

(defun dired-dwim-copy ()
  "Copy file to other dired window"
  (interactive)
  (let ((dired-dwim-target t))
    (dired-do-copy)))

(defun eol-and-next-line-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun my-comment-dwim (arg)
  "If region is active, call `comment-or-uncomment-region'.
Else, if the line is empty, call `comment-insert-comment-function'
if it is defined, otherwise insert a comment and indent it.
Else, call `comment-or-uncomment-region' on the whole line"
  (interactive "P")
  (comment-normalize-vars)
  (if (and mark-active transient-mark-mode)
      (comment-or-uncomment-region (region-beginning) (region-end) arg)
    (if (save-excursion (beginning-of-line) (not (looking-at "\\s-*$")))
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (if comment-insert-comment-function
          (funcall comment-insert-comment-function)
        (let ((add (comment-add arg)))
          ;; Some modes insist on keeping column 0 comment in column 0
          ;; so we need to move away from it before inserting the comment.
          (indent-according-to-mode)
          (insert (comment-padright comment-start add))
          (save-excursion
            (unless (string= "" comment-end)
              (insert (comment-padleft comment-end add)))
            (indent-according-to-mode)))))))

(defun indent-dwim ()
  "Indents to the next multiple of the tab spacing"
  (interactive)
  (let ((deactivate-mark nil)
        (beg (and mark-active (region-beginning)))
        (end (and mark-active (region-end)))
        (indentation-list))
    (if (= (line-number-at-pos beg) (line-number-at-pos end))
        (let ((indent-amount (mod (current-indentation) tab-width)))
          (if (> 0 indent-amount)
              (indent-line-to (+ indent-amount (current-indentation)))
            (indent-line-to (+ tab-width (current-indentation)))))
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (if (not (looking-at "\\s-*$"))
              (add-to-list 'indentation-list (current-indentation)))
          (forward-line)))
      (let ((indent-amount (- tab-width (mod (apply 'min indentation-list) tab-width))))
        (if (> 0 indent-amount)
            (indent-rigidly beg end indent-amount)
          (indent-rigidly beg end tab-width))))))

(defun unindent-dwim (&optional count-arg)
 "Keeps relative spacing in the region.  Unindents to the next multiple of the current tab-width"
 (interactive)
 (let ((deactivate-mark nil)
       (beg (or (and mark-active (region-beginning)) (line-beginning-position)))
       (end (or (and mark-active (region-end)) (line-end-position)))
       (min-indentation)
       (count (or count-arg 1)))
   (if (= (line-number-at-pos beg) (line-number-at-pos end))
       (let ((unindent-amount (mod (current-indentation) tab-width)))
         (if (< 0 unindent-amount)
             (indent-line-to (- (current-indentation) unindent-amount))
           (indent-line-to (- (current-indentation) tab-width))))
     (save-excursion
       (goto-char beg)
       (while (< (point) end)
         (if (not (looking-at "\\s-*$"))
             (add-to-list 'min-indentation (current-indentation)))
         (forward-line)))
   ;; (message min-indentation)
     (if (< 0 count)
         (if (not (< 0 (apply 'min min-indentation)))
             (error "Can't indent any more.  Try `indent-rigidly` with a negative arg.")))
     (if (> 0 count)
         (indent-rigidly beg end (* (- 0 tab-width) count))
       (let (
             (indent-amount
              (apply 'min (mapcar (lambda (x) (- 0 (mod x tab-width))) min-indentation))))
         (indent-rigidly beg end (or
                                  (and (< indent-amount 0) indent-amount)
                                  (* (or count 1) (- 0 tab-width)))))))))

;; Stolen functions


(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun lorem-ipsum ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode t))
