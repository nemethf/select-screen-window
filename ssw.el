;;; ssw.el --- Select a screen window with incremental search -*- lexical-binding: t -*-

;; Copyright (C) 2022 Felicián Németh

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;  See README.md.
;;
;;  TODO: Emacs-29 has some alternative complation methods:
;;  https://www.scss.tcd.ie/~sulimanm/posts/default-emacs-completion.html

;;; Code:

(require 'ido)

(defvar ssw-msgwait 5
  "msgwait value after selection.
ssw sets msgwait to 0 before querying the list of windows.  It
resets msgwait to `ssw-msgwait' afterwards.")

(defvar ssw-separator "#G!3#"
  "A string that is never part of a window title.")

(defun ssw (&optional sty)
  "Select a screen window with incremental search.
If STY is non-nil, it should be the screen's sessionname.
If STY is non-nil, assume `ssw' called from emacsclient."
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer (get-buffer-create " *ssw*"))
    (let* ((ido-max-prospects 0)          ; No limit.
           (ido-decorations '("\n" "" "\n" ",..." "[" "]" " [No match]"
                              " [Matched]" " [Not readable]" " [Too big]"
                              " [Confirm]"))
           (ido-enable-flex-matching t)
           (max-mini-window-height 0.99)
           (scmd (if sty (format "screen -S %s" sty) "screen"))
           (wtitle "%n. %t")         ; (info "(screen)String Escapes")
           (winlist-cmd (format "%s -Q windows '%s%s'"
                                scmd wtitle ssw-separator))
           (mode-line-format nil)
           (menu-bar-mode menu-bar-mode)
           winlist selection)
      (menu-bar-mode -1)
      (shell-command (format "%s -X title SSW" scmd))
      (shell-command (format "%s -X msgwait 0" scmd))
      (setq winlist
            (split-string (shell-command-to-string winlist-cmd) ssw-separator))
      (setq selection
            (ido-completing-read
	     "Switch to window: "
	     (seq-remove (lambda (w) (string-match "^[[:digit:]]+\\. SSW$" w))
                         winlist)
             nil t))
      (setq selection (car (split-string selection "\\.")))
      (menu-bar-mode menu-bar-mode)
      (shell-command (format "%s -X msgwait %s" scmd ssw-msgwait))
      (if (string-equal "" selection)
          (shell-command (format "%s -X other" scmd))
        (shell-command (format "%s -X select %s" scmd selection)))
      (if sty
          (delete-frame)
        (kill-emacs)))))

;;; ssw.el ends here
