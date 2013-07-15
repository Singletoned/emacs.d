;;; oldskool-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2011 Ed Singleton

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme oldskool
  "This is the default Emacs theme.")

(custom-theme-set-faces
 'oldskool
 '(default ((t (:background "black" :foreground "#2D2"))))
 '(cursor ((t (:background "orange" :foreground "grey"))))
 '(region ((t (:background "#444"))))
 '(mode-line ((t (:background "#bfbfbf" :foreground "#000000"))))
 '(mode-line-inactive ((t (:background "#e5e5e5" :foreground "#333333"))))
 '(fringe ((t (:background "#191919"))))
 '(minibuffer-prompt ((t (:foreground "#FF0000"))))
 '(font-lock-builtin-face ((t (:foreground "#D2D"))))
 '(font-lock-comment-face ((t (:foreground "#666"))))
 '(font-lock-constant-face ((t (:foreground "#D2D"))))
 '(font-lock-function-name-face ((t (:foreground "#2DD"))))
 '(font-lock-keyword-face ((t (:foreground "#D22"))))
 '(font-lock-string-face ((t (:foreground "#DDD"))))
 '(font-lock-type-face ((t (:foreground "#DD2"))))
 '(font-lock-variable-name-face ((t (:foreground "green"))))
 '(font-lock-warning-face ((t (:slant italic :foreground "magenta" :underline t :weight bold))))
 '(isearch ((t (:background "#cd00cd" :foreground "#b0e2ff"))))
 '(lazy-highlight ((t (:background "#eee" :foreground "#000"))))
 '(link ((t (:foreground "#0000ff"))))
 '(link-visited ((t (:foreground "#8b008b"))))
 '(header-line ((t (:background "#e5e5e5" :foreground "#333333"))))
 '(hl-line ((t (:background "#222"))))
 '(diff-added ((t (:foreground "green4"))))
 '(diff-removed ((t (:foreground "red3"))))
 '(magit-diff-add ((t (:foreground "green4"))))
 '(magit-diff-del ((t (:foreground "red3"))))
 '(magit-diff-none ((t (:foreground "white"))))
 '(magit-header ((t (:foreground "white" :background "black"))))
 '(magit-section-title ((t (:foreground "blue"))))
 '(magit-diff-file-header ((t (:foreground "cyan" :background "black"))))
 '(magit-diff-hunk-header ((t (:foreground "blue" :background "black"))))
 '(magit-item-highlight ((t (:background "#222"))))
)

(provide-theme 'oldskool)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; oldskool-theme.el  ends here
