
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
