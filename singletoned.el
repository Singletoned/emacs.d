(server-start)

(set 'confirm-kill-emacs `yes-or-no-p)

(add-to-list 'default-frame-alist '(font . "Inconsolata 14"))
(set-face-attribute 'default t :font "Inconsolata 14")

;; Set shell path

(setenv "PATH" (concat "/usr/local/bin:" "~/.emacs.d/node_modules/.bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("/opt/homebrew/bin" "/opt/homebrew/sbin" "/usr/local/bin")))

(set 'python-shell-buffer-name "Python Shell")
(setq python-shell-interpreter "python3")

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(setenv "RIPGREP_CONFIG_PATH" (concat (getenv "HOME") "/.config/ripgrep"))

;; Requirements

(require 'uniquify)
(require 'expand-region)
(require 'cl)
(require 'org-table)

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

(setq-default fill-column 78)

;(modify-syntax-entry ?_ "w" python-mode-syntax-table)

(set-default 'split-height-threshold 999)
(set-default 'split-width-threshold 999)

(set-default 'enable-recursive-minibuffers t)

(set-default 'vc-follow-symlinks t)
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

(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      uniquify-buffer-name-style 'forward
      oddmuse-directory "~/.emacs.d/oddmuse"
      save-place-file "~/.emacs.d/places"
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      diff-switches "-u")

(set-default 'reb-re-syntax 'string)

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
(set-default 'python-indent-guess-indent-offset nil)


;; Fewer Garbage collections

(setq gc-cons-threshold 20000000)


;; Extra Keywords

(font-lock-add-keywords
nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
       1 font-lock-warning-face t)))


;; Keyboard shortcuts

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "s-w")
                (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key (kbd "s-<return>") 'eol-and-next-line-and-indent)
(global-set-key (kbd "s-/") 'my-comment-dwim)
(global-set-key (kbd "s-[") 'unindent-dwim)
(global-set-key (kbd "s-]") 'indent-dwim)


(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-l") 'goto-line)

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
    (web-mode web-mode-markup-indent-offset)
    (emacs-lisp-mode lisp-indent-offset)
    (jade-mode jade-tab-width)
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


(defun insert-random-uuid ()
  "Insert a UUID. This uses a simple hashing of variable data.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

Note: this code uses https://en.wikipedia.org/wiki/Md5 , which is not cryptographically safe. I'm not sure what's the implication of its use here.

Version 2015-01-30
URL `http://ergoemacs.org/emacs/elisp_generate_uuid.html'
"
;; by Christopher Wellons, 2011-11-18. Editted by Xah Lee.
;; Edited by Hideki Saito further to generate all valid variants for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
  (interactive)
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                            (user-uid)
                            (emacs-pid)
                            (system-name)
                            (user-full-name)
                            (current-time)
                            (emacs-uptime)
                            (garbage-collect)
                            (buffer-string)
                            (random)
                            (recent-keys)))))

    (insert (format "%s-%s-4%s-%s%s-%s"
                    (substring myStr 0 8)
                    (substring myStr 8 12)
                    (substring myStr 13 16)
                    (format "%x" (+ 8 (random 4)))
                    (substring myStr 17 20)
                    (substring myStr 20 32)))))


(defun python-prprint-region ()
  "PPrints the region using python shell"
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "python -c 'import sys; from pprint import pprint as pp; pp(eval(sys.stdin.read()))'"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun insert-datetime ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))

(require 'google-this)

(defun google-this-lucky-and-insert-markdown-url (term &optional insert)
  "Fetch the url that would be visited by `google-this-lucky'.
If you just want to do an \"I'm feeling lucky search\", use
`google-this-lucky-search' instead.
Interactively:
* Insert the URL at point,
* Kill the searched term, removing it from the buffer (it is killed, not
  deleted, so it can be easily yanked back if desired).
* Search term defaults to region or line, and always queries for
  confirmation.
Non-Interactively:
* Runs synchronously,
* Search TERM is an argument without confirmation,
* Only insert if INSERT is non-nil, otherwise return."
  (interactive '(needsQuerying t))
  (let ((nint (null (called-interactively-p 'any)))
        (l (if (region-active-p) (region-beginning) (line-beginning-position)))
        (r (if (region-active-p) (region-end) (line-end-position)))
        ;; We get current-buffer and point here, because it's
        ;; conceivable that they could change while waiting for input
        ;; from read-string
        (p (point))
        (b (current-buffer)))
    (when nint (setq google-this--last-url nil))
    (when (eq term 'needsQuerying)
      (setq term (read-string "Lucky Term: " (buffer-substring-no-properties l r))))
    (unless (stringp term) (error "TERM must be a string!"))

    (defalias '-insert-link (apply-partially 'insert-markdown-link term) "Inert link")
    (google-this--do-lucky-search
     term
     `(lambda (url)
        (unless url (error "Received nil url"))
        (with-current-buffer ,b
          (save-excursion
            (if ,nint (goto-char ,p)
              (kill-region ,l ,r)
              (goto-char ,l))
            (insert "[")
            (insert ,term)
            (insert "](")
            (insert url)
            (insert ")")))))
    (unless nint (deactivate-mark))
    (when nint
      (while (null google-this--last-url) (sleep-for 0 10))
      google-this--last-url)))




(defun html-to-jade ()
  "Reformat html to jade."
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "html2jade --bodyless"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))


(defun markdown-to-html ()
  "Reformat markdown to html."
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "npx markdown"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))


(defun jade-to-html ()
  "Reformat markdown to html."
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "npx jade"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))


(defun html-to-markdown ()
  "Reformat html to markdown."
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "pandoc --from=html --to=markdown"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))


(defun format-json ()
  "Pretty format buffer using Python json lib."
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "python3 -c 'import json, sys; print(json.dumps(json.loads(sys.stdin.read()), indent=2, sort_keys=True))'"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))


(defun format-css ()
  "Pretty format buffer using cssbeautify-cli."
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "npx cssbeautify-cli -s --indent=2"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))

(defun format-js ()
  "Pretty format buffer using esformatter."
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "esformatter"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))

(defun format-sql ()
  "Pretty format buffer using sqlparse"
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "python3 -c 'import sys, sqlparse; print(sqlparse.format(sys.stdin.read(), reindent=True))'"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))

(defun python-to-json ()
  "Pretty format buffer using Python json lib."
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "python3 -c 'import json, sys, ast; print(json.dumps(ast.literal_eval(sys.stdin.read()), indent=2, sort_keys=True))'"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))



(defun python-to-python ()
  "Eval python"
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "python3 -c 'import sys; print(eval(sys.stdin.read()))'"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))



(defun format-python ()
  "Format Python using Black"
  (interactive)
  (let ((deactivate-mark nil)
        (beg (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (message "%d %d" beg end)
    (shell-command-on-region
     ;; beginning and end of buffer
     beg
     end
     ;; command and parameters
     "~/.envs/emacs/bin/black -"
     ;; output buffer
     (current-buffer)
     ;; replace?
     t
     ;; name of the error buffer
     "*Tidy Error Buffer*"
     ;; show error buffer?
     t)))


(defun json-to-yaml ()
  "Pretty format buffer using Python yaml lib."
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "json2yaml"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))


(defun lint-shell ()
  "Reformat shell"
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "shfmt"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))

(defun lint-graphql ()
  "Reformat graphql"
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       ;; beginning and end of buffer
       (region-beginning)
       (region-end)
       ;; command and parameters
       "python3 -c 'import graphql, sys; print(graphql.print_ast(graphql.parse(graphql.Source(sys.stdin.read()))))'"
       ;; output buffer
       (current-buffer)
       ;; replace?
       t
       ;; name of the error buffer
       "*Tidy Error Buffer*"
       ;; show error buffer?
       t)))


;; This line deliberately not left blank
