(defun my-comment-dwim (arg)
  "If region is active, call `comment-or-uncomment-region'.
  Else, if the line is empty, call `comment-insert-comment-function'
  if it is defined, otherwise insert a comment and indent it.
  Else, call `comment-or-uncomment-region' on the whole line"
  (interactive "P")
  (comment-normalize-vars)
  (if (and mark-active transient-mark-mode)
    (comment-or-uncomment-region (region-beginning) (region-end) arg)
    (if
      (save-excursion
        (beginning-of-line)
        (not (looking-at "\\s-*$")))
      (comment-or-uncomment-region
        (line-beginning-position)
        (line-end-position))
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
  '
  ((awk-mode c-basic-offset)
    (c++-mode c-basic-offset)
    (c-mode c-basic-offset)
    (cmake-mode cmake-tab-width)
    (coffee-mode coffee-tab-width)
    (cperl-mode cperl-indent-level)
    (css-mode css-indent-offset)
    (erlang-mode erlang-indent-level)
    (feature-mode feature-indent-offset)
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
    (python-mode python-indent-offset)
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
  (let
    (
      (parent major-mode)
      entry
      indent-width)
    ;; Find the closet parent mode of `major-mode' in
    ;; `edconf-indentation-alist'.
    (while
      (and (not (setq entry (assoc parent my-indent-var-list)))
        (setq parent (get parent 'derived-mode-parent))))
    (if entry
      (setq indent-width (eval (cadr entry))))
    (if indent-width
      indent-width
      tab-width)))


(defun indent-dwim ()
  "Indents to the next multiple of the tab spacing"
  (interactive)
  (let
    (
      (deactivate-mark nil)
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
      (let
        (
          (indent-amount
            (- indent-width
              (mod (apply 'min indentation-list) indent-width))))
        (if (> 0 indent-amount)
          (indent-rigidly beg end indent-amount)
          (indent-rigidly beg end indent-width))))))

(defun unindent-dwim (&optional count-arg)
  "Keeps relative spacing in the region.  Unindents to the next multiple of the current indent-width"
  (interactive)
  (let
    (
      (deactivate-mark nil)
      (beg
        (or (and mark-active (region-beginning))
          (line-beginning-position)))
      (end (or (and mark-active (region-end)) (line-end-position)))
      (min-indentation)
      (count (or count-arg 1))
      (indent-width (my-get-indent-var)))
    (if (= (line-number-at-pos beg) (line-number-at-pos end))
      (let
        ((unindent-amount (mod (current-indentation) indent-width)))
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
          (error
            "Can't indent any more.  Try `indent-rigidly` with a negative arg.")))
      (if (> 0 count)
        (indent-rigidly beg end (* (- 0 indent-width) count))
        (let
          (
            (indent-amount
              (apply 'min
                (mapcar
                  (lambda (x) (- 0 (mod x indent-width)))
                  min-indentation))))
          (indent-rigidly
            beg end
            (or (and (< indent-amount 0) indent-amount)
              (* (or count 1) (- 0 indent-width)))))))))


(defun format-json ()
  "Pretty format buffer using Python json lib."
  (interactive)
  (if (use-region-p)
    (shell-command-on-region
      ;; beginning and end of buffer
      (region-beginning) (region-end)
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

(defun format-python ()
  "Format Python using Ruff"
  (interactive)
  (let
    (
      (deactivate-mark nil)
      (beg
        (if mark-active
          (region-beginning)
          (point-min)))
      (end
        (if mark-active
          (region-end)
          (point-max))))
    (message "%d %d" beg end)
    (shell-command-on-region
      ;; beginning and end of buffer
      beg end
      ;; command and parameters
      "uvx ruff format - | uvx ruff check --fix --unsafe-fixes --extend-select I --exit-zero --silent -"
      ;; output buffer
      (current-buffer)
      ;; replace?
      t
      ;; name of the error buffer
      "*Tidy Error Buffer*"
      ;; show error buffer?
      t)))


(defun format-markdown ()
  "Format Markdown using mdformat"
  (interactive)
  (let
    (
      (deactivate-mark nil)
      (beg
        (if mark-active
          (region-beginning)
          (point-min)))
      (end
        (if mark-active
          (region-end)
          (point-max))))
    (message "%d %d" beg end)
    (shell-command-on-region
      ;; beginning and end of buffer
      beg end
      ;; command and parameters
      "uvx mdformat -"
      ;; output buffer
      (current-buffer)
      ;; replace?
      t
      ;; name of the error buffer
      "*Tidy Error Buffer*"
      ;; show error buffer?
      t)))

(defun insert-date ()
  "Insert the current date in ISO 8601 format (YYYY-MM-DD)."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun insert-datetime ()
  "Insert the current date and time in ISO 8601 format (YYYY-MM-DDTHH:MM:SS)."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))

(defun docker-region-to-exec-CMD (start end)
  "Convert selected shell command to Docker CMD exec form and replace region."
  (interactive "r")
  (let*
    (
      (cmd (buffer-substring-no-properties start end))
      (words (split-string cmd " +" t))
      (quoted (mapcar (lambda (w) (format "\"%s\"" w)) words))
      (json-array (concat "[" (mapconcat 'identity quoted ", ") "]")))
    (delete-region start end)
    (insert json-array)
    (kill-new json-array)
    (message "Replaced with: %s" json-array)))

(defmacro make-s-command (s-function command-name)
  "Create an interactive command named COMMAND-NAME that applies S-FUNCTION to the current region.
S-FUNCTION should be a symbol of a function from s.el.
COMMAND-NAME should be a string for the new command name."
  `
  (defun ,(intern command-name) (start end)
    ,(format "Apply `%s' to the current region." s-function)
    (interactive "r")
    (let ((text (buffer-substring-no-properties start end)))
      (delete-region start end)
      (insert (funcall ',s-function text)))))

(make-s-command s-dashed-words "to-kebab-case-region")
(make-s-command s-lower-camel-case "to-camel-case-region")
(make-s-command s-snake-case "to-snake-case-region")

(defun dired-dwim-copy ()
  "Copy file to other dired window"
  (interactive)
  (let ((dired-dwim-target t))
    (dired-do-copy)))


(defun strip-common-line-prefix (start end)
  "Strip the longest common prefix of all lines in the region or buffer."
  (interactive
    (if (use-region-p)
      (list (region-beginning) (region-end))
      (list (point-min) (point-max))))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((lines '()))
        ;; Collect all lines into a list
        (while (not (eobp))
          (push
            (buffer-substring-no-properties
              (line-beginning-position)
              (line-end-position))
            lines)
          (forward-line 1))
        (setq lines (nreverse lines))
        ;; Find the common prefix
        (let ((prefix (car lines)))
          (dolist (line (cdr lines))
            (let
              (
                (i 0)
                (max (min (length prefix) (length line))))
              (while
                (and (< i max)
                  (char-equal (aref prefix i) (aref line i)))
                (setq i (1+ i)))
              (setq prefix (substring prefix 0 i))))
          ;; Now remove the prefix from each line
          (goto-char (point-min))
          (while (not (eobp))
            (when (looking-at (regexp-quote prefix))
              (replace-match ""))
            (forward-line 1)))))))
