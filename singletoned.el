(server-start)

(set 'confirm-kill-emacs `yes-or-no-p)

(set-default-font "Inconsolata 14")

;; Set shell path

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))

(set 'python-shell-buffer-name "Python Shell")

;; Requirements

(require 'uniquify)
(require 'expand-region)
(require 'cl)

(defhydra hydra-whitespace (global-map "C-c t")
  "whitespace"
  ("u" delete-indentation "join-lines")
  ("j" next-line "down")
  ("k" previous-line "up")
  ("h" backward-char "back")
  ("l" forward-char "forward")
  ("." (lambda () (interactive) (re-search-forward "^[[:space:]]+[](),{}]+$")) "punctuation"))

;; Modes


;; Colors

(set 'custom-safe-themes
     (cons (with-temp-buffer
               (insert-file-contents
                (concat user-emacs-directory "oldskool-theme.el"))
               (sha1 (current-buffer)))
           custom-safe-themes))

(load-theme 'oldskool)


;; Config

;(modify-syntax-entry ?_ "w" python-mode-syntax-table)

(global-hl-line-mode t)
(set-default 'hl-line-sticky-flag nil)
(global-linum-mode t)
(set-default 'linum-format "%3d")
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
(ansi-color-for-comint-mode-on)

(set-default 'electric-indent-mode nil)

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

;; Dired

(setq dired-auto-revert-buffer t)

(eval-after-load "dired"
  '(require 'dired-x))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))

(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "_") 'my-dired-create-file)
     (defun my-dired-create-file (file)
       "Create a file called FILE. If FILE already exists, signal an error.
        Stolen from http://stackoverflow.com/questions/2592095/how-do-i-create-an-empty-file-in-emacs"
       (interactive
        (list (read-file-name "Create file: " (dired-current-directory))))
       (let* ((expanded (expand-file-name file))
              (try expanded)
              (dir (directory-file-name (file-name-directory expanded)))
              new)
         (if (file-exists-p expanded)
             (error "Cannot create file %s: file exists" expanded))
         ;; Find the topmost nonexistent parent dir (variable `new')
         (while (and try (not (file-exists-p try)) (not (equal new try)))
           (setq new try
                 try (directory-file-name (file-name-directory try))))
         (when (not (file-exists-p dir))
           (make-directory dir t))
         (write-region "" nil expanded t)
         (when new
           (dired-add-file new)
           (dired-move-to-filename))))))


;; Tabs

(set-default 'indent-tabs-mode nil)

;; Try really, really hard to force tabs to be 4 spaces

(set-default 'tab-width 4)
(set-default 'python-indent 4)
(set-default 'python-tab-width 4)
(set 'default-tab-width 4)
(set-default 'python-indent-guess-indent-offset nil)


;; Fewer Garbage collections

(setq gc-cons-threshold 20000000)


;; Extra Keywords

(font-lock-add-keywords
nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
       1 font-lock-warning-face t)))




;; Ido mode

(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'prompt
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-buffer-disable-smart-matches t)



;; Keyboard shortcuts

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "s-w")
                (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key (kbd "s-<return>") 'eol-and-next-line-and-indent)
(global-set-key (kbd "s-/") 'my-comment-dwim)
(global-set-key (kbd "s-[") 'unindent-dwim)
(global-set-key (kbd "s-]") 'indent-dwim)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x j") 'dired-jump)
(global-set-key (kbd "C-x C-j") 'dired-jump-other-window)

(global-set-key (kbd "C-x ~") (lambda () (interactive) (dired-other-window user-emacs-directory)))

;; Some Yegge-isations
(global-set-key (kbd "s-h") 'help-command)
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

(global-set-key (kbd "C-c q") 'query-replace)
(global-set-key (kbd "C-c Q") 'query-replace-regexp)
(global-set-key (kbd "C-c u") 'delete-indentation)


;; Functions

;; Reopen last closed file

(defun find-last-killed-file ()
  (interactive)
  (let ((active-files (loop for buf in (buffer-list)
                            when (buffer-file-name buf) collect it)))
    (loop for file in recentf-list
          unless (member file active-files) return (find-file file))))

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

(defconst my-indent-var-list
  '((awk-mode c-basic-offset)
    (c++-mode c-basic-offset)
    (c-mode c-basic-offset)
    (cmake-mode cmake-tab-width)
    (coffee-mode coffee-tab-width)
    (cperl-mode cperl-indent-level)
    (css-mode css-indent-offset)
    (erlang-mode erlang-indent-level)
    (groovy-mode c-basic-offset)
    (haskell-mode haskell-indent-offset)
    (idl-mode c-basic-offset)
    (java-mode c-basic-offset)
    (js-mode js-indent-level)
    (js2-mode js2-basic-offset)
    (js3-mode js3-indent-level)
    (json-mode js-indent-level)
    (lisp-mode lisp-indent-offset)
    (livescript-mode livescript-tab-width)
    (mustache-mode mustache-basic-offset)
    (nxml-mode nxml-child-indent)
    (objc-mode c-basic-offset)
    (perl-mode perl-indent-level)
    (pike-mode c-basic-offset)
    (puppet-mode puppet-indent-level)
    (python-mode python-indent)
    (ruby-mode ruby-indent-level)
    (scala-mode scala-indent:step)
    (sgml-mode sgml-basic-offset)
    (sh-mode sh-basic-offset sh-indentation)
    (web-mode web-mode-indent-style)
    (emacs-lisp-mode lisp-indent-offset)
    (yaml-mode yaml-indent-offset)))

(defun my-get-indent-var ()
  "Gets the name of the indentation variable for the current mode"
  (let ((parent major-mode)
        entry
        indent-width)
    ;; Find the closet parent mode of `major-mode' in
    ;; `edconf-indentation-alist'.
    (while (and
            (not (setq entry (assoc parent my-indent-var-list)))
            (setq parent (get parent 'derived-mode-parent))))
    (if entry
        (setq indent-width (eval (cadr entry))))
    (if indent-width
        indent-width
      tab-width)))

(defun indent-dwim ()
  "Indents to the next multiple of the tab spacing"
  (interactive)
  (let ((deactivate-mark nil)
        (beg (and mark-active (region-beginning)))
        (end (and mark-active (region-end)))
        (indentation-list)
        (indent-width (my-get-indent-var)))
    (if (= (line-number-at-pos beg) (line-number-at-pos end))
        (let ((indent-amount (mod (current-indentation) indent-width)))
          (if (> 0 indent-amount)
              (indent-line-to (+ indent-amount (current-indentation)))
            (indent-line-to (+ indent-width (current-indentation)))))
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (if (not (looking-at "\\s-*$"))
              (add-to-list 'indentation-list (current-indentation)))
          (forward-line)))
      (let ((indent-amount (- indent-width (mod (apply 'min indentation-list) indent-width))))
        (if (> 0 indent-amount)
            (indent-rigidly beg end indent-amount)
          (indent-rigidly beg end indent-width))))))

(defun unindent-dwim (&optional count-arg)
  "Keeps relative spacing in the region.  Unindents to the next multiple of the current indent-width"
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning)) (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position)))
        (min-indentation)
        (count (or count-arg 1))
        (indent-width (my-get-indent-var)))
    (if (= (line-number-at-pos beg) (line-number-at-pos end))
        (let ((unindent-amount (mod (current-indentation) indent-width)))
          (if (< 0 unindent-amount)
              (indent-line-to (- (current-indentation) unindent-amount))
            (indent-line-to (- (current-indentation) indent-width))))
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
          (indent-rigidly beg end (* (- 0 indent-width) count))
        (let (
              (indent-amount
               (apply 'min (mapcar (lambda (x) (- 0 (mod x indent-width))) min-indentation))))
          (indent-rigidly beg end (or
                                   (and (< indent-amount 0) indent-amount)
                                   (* (or count 1) (- 0 indent-width)))))))))

;; Stolen functions

;; (defun underscore-region ()
;;   (interactive)
;;   (let ((deactivate-mark nil)
;;         (beg (and mark-active (region-beginning)))
;;         (end (and mark-active (region-end)))
        
;; ))

;;  )

;; (defun split-name (s)
;;   (split-string
;;    (let ((case-fold-search nil))
;;      (downcase
;;       (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
;;    "[^A-Za-z0-9]+"))

;; (defun camelcase  (s) (mapconcat 'capitalize (split-name s) ""))
;; (defun underscore (s) (mapconcat 'downcase   (split-name s) "_"))
;; (defun dasherize  (s) (mapconcat 'downcase   (split-name s) "-"))
;; (defun colonize   (s) (mapconcat 'capitalize (split-name s) "::"))

;;     (defun camelscore (s)	
;;       (cond ((string-match-p "\:"  s)	(camelcase s))
;; 	    ((string-match-p "-" s)     (colonize s))
;; 	    ((string-match-p "_" s)	(dasherize s))
;; 	    (t                          (underscore s)) ))

;;     (defun camelscore-word-at-point ()
;;       (interactive)
;;       (let* ((case-fold-search nil)
;; 	     (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
;; 	     (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
;; 	     (txt (buffer-substring beg end))
;; 	     (cml (camelscore txt)) )
;; 	(if cml (progn (delete-region beg end) (insert cml))) ))

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

(defun dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive
   (list
    (let ((print-level nil)
          (minibuffer-history-position 0)
          (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
      (unwind-protect
          (read-from-minibuffer
           "Command: " (prin1-to-string (nth 0 command-history))
           read-expression-map t
           (cons 'command-history 0))

        ;; If command was added to command-history as a
        ;; string, get rid of that.  We want only
        ;; evaluable expressions there.
        (if (stringp (car command-history))
            (setq command-history (cdr command-history)))))))
  (dolist (filename (dired-get-marked-files))
    (with-current-buffer (find-file-noselect filename)
      (if (symbolp command)
          (call-interactively command)
        (eval command)))))

(defun html-escape ()
  (interactive)
  (save-excursion
    (save-restriction
      (let
          ((deactivate-mark nil)
           (beg (and mark-active (region-beginning)))
           (end (and mark-active (region-end))))
        (if (and beg end)
            (progn
              (narrow-to-region beg end)))
        (goto-char (point-min))
        (replace-string "’" "&rsquo;")
        (goto-char (point-min))
        (replace-string "£" "&pound;")
        (goto-char (point-min))
        (replace-string "’" "&lsquo;")))))

(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'"))
                          nil beg end))

