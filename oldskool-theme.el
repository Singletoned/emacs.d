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
 '(default ((t (:background "black" :foreground "green"))))
 '(cursor ((t (:background "grey" :foreground "blue"))))
 '(region ((t (:background "#eedc82"))))
 '(mode-line ((t (:background "#bfbfbf" :foreground "#000000"))))
 '(mode-line-inactive ((t (:background "#e5e5e5" :foreground "#333333"))))
 '(fringe ((t (:background "#f2f2f2"))))
 '(minibuffer-prompt ((t (:foreground "#0000cd"))))
 '(font-lock-builtin-face ((t (:foreground "blue"))))
 '(font-lock-comment-face ((t (:foreground "cyan"))))
 '(font-lock-constant-face ((t (:foreground "magenta"))))
 '(font-lock-function-name-face ((t (:foreground "cyan"))))
 '(font-lock-keyword-face ((t (:foreground "red"))))
 '(font-lock-string-face ((t (:foreground "white"))))
 '(font-lock-type-face ((t (:foreground "yellow"))))
 '(font-lock-variable-name-face ((t (:foreground "blue"))))
 '(font-lock-warning-face ((t (:slant italic :foreground "magenta" :underline t :weight bold))))
 '(isearch ((t (:background "#cd00cd" :foreground "#b0e2ff"))))
 '(lazy-highlight ((t (:background "#afeeee"))))
 '(link ((t (:foreground "#0000ff"))))
 '(link-visited ((t (:foreground "#8b008b"))))
 '(header-line ((t (:background "#e5e5e5" :foreground "#333333")))))

(provide-theme 'oldskool)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; oldskool-theme.el  ends here
