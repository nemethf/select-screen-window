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

(defvar ssw-completing-read-function 'ssw-completing-read
  "A `completing-read' function to use.")

(defun ssw (&optional sty)
  "Select a screen window with incremental search.
If STY is non-nil, it should be the screen's sessionname.
If STY is non-nil, assume `ssw' called from emacsclient."
  (unwind-protect
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer (get-buffer-create " *ssw*"))
        (let* ((scmd (if sty (format "screen -S %s" sty) "screen"))
               (mode-line-format nil)
               (menu-bar-mode menu-bar-mode)
               (dummy (menu-bar-mode -1))
               (default-directory "/")
               (winlist (ssw-get-winlist scmd))
               (selection (apply ssw-completing-read-function
                                 "Switch to window: " winlist nil t nil))
               (winnum (car (split-string selection "\\."))))
          (ssw-switch-to-window scmd winnum)
          (menu-bar-mode menu-bar-mode)))
    (if sty
        (delete-frame)
      (kill-emacs))))

(defun ssw-get-winlist (scmd &optional keep-current)
  "Call `screen' to get the list of windows.
If KEEP-CURRENT is non-nil, don't exclude current window from the list."
  (let* ((wtitle "%n. %t")           ; (info "(screen)String Escapes")
         (winlist-cmd
          (format "%s -Q windows '%s%s'" scmd wtitle ssw-separator)))
    (unless keep-current
      (shell-command (format "%s -X title SSW" scmd)))
    (shell-command (format "%s -X msgwait 0" scmd))
    (prog1
        (seq-remove
         (lambda (w) (string-match "^[[:digit:]]+\\. SSW$" w))
         (split-string
          (shell-command-to-string winlist-cmd) ssw-separator))
      (shell-command (format "%s -X msgwait %s" scmd ssw-msgwait)))))

(defun ssw-switch-to-window (scmd window-number)
  "Call `screen' to switch to WINDOW-NUMBER.
WINDOW-NUMBER can be an empty string."
  (unless (string-equal "" window-number)
    (shell-command (format "%s -X select %s" scmd window-number))))


(defun ssw-completing-read (prompt choices &optional predicate require-match
                            initial-input hist def inherit-input-method)
  "A vertical `completing-read' with flex matching."
  (let ((ido-max-prospects 0)          ; No limit.
        (ido-decorations '("\n" "" "\n" ",..." "[" "]" " [No match]"
                           " [Matched]" " [Not readable]" " [Too big]"
                           " [Confirm]"))
        (ido-enable-flex-matching t)
        (max-mini-window-height 0.99))
    (ido-completing-read prompt choices predicate require-match initial-input
                         hist def inherit-input-method)))

(defun ssw-select-sty ()
  (let (sty-list)
    (with-temp-buffer
      (shell-command "screen -ls" t)
      (goto-char (point-min))
      (while (re-search-forward "^\t\\([^\t]+\\).*$" nil t)
        (setq sty-list (cons (match-string 1) sty-list))))
    (if (= 1 (length sty-list))
        (car sty-list)
      (completing-read "Select screen: " sty-list nil t))))

(defun ssw-insert-window ()
  "Insert the content of a screen window at point."
  (interactive)
  (let* ((sty (ssw-select-sty))
         (scmd (if sty (format "screen -S %s" sty) "screen"))
         (window (completing-read "Window to insert: "
                                  (ssw-get-winlist scmd t) nil t))
         (winnum (car (split-string window "\\.")))
         (tempfile (make-temp-file "ssw-")))
    (ssw-switch-to-window scmd winnum)
    (shell-command (format "%s -X hardcopy %s" scmd tempfile))
    (insert-file-contents tempfile)
    (delete-file tempfile nil)))

;;; ssw.el ends here
