(server-start)

;; Some Yegge-isations
(global-set-key (kbd "M-h") 'help-command)
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;; Functions

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
