;;; init-eldoc.el --- ElDoc configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (setup))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; eldoc-mode is a MinorMode which shows you, in the echo area, the argument
;; list of the function call you are currently writing.
;; https://www.emacswiki.org/emacs/ElDoc
;; https://github.com/joaotavora/eglot/discussions/1328

;;; Code:

(require 'setup)

;; `eldoc-display-in-echo-area' stops execution if any of the following are true:
;; 1. eldoc-display-message-no-interference-p returns non-nil
;; 2. this-command is set
;; 3. eldoc--message-commands does not contain last-command
;;
;; The function originally does not continue if `this-command' is set,
;; because that would mean a command is currently executing.
;;
;; > Check if we have permission to mess with echo area at all.  For
;; > example, if this-command is non-nil while running via an idle
;; > timer, we're still in the middle of executing a command, e.g. a
;; > query-replace where it would be annoying to overwrite the echo
;; > area.
;;
;; Unfortunately what seems to happen is if
;; `eldoc-documentation-functions' is immediate, `this-command' is
;; whatever called `eldoc-print-current-symbol-info'. If
;; eldoc-documentation-function is asynchronous, `this-command' is
;; nil.
;;
;; This means executing `eldoc-print-current-symbol-info' from a user
;; function works for asynchronous documentation functions
;; (e.g. buffers with eglot) but not from immediate documentation
;; functions (e.g. buffers with emacs lisp).
;;
;; Adjusting this function so that it stops if `this-command' is set
;; and it is not the `eglot-manual' symbol keeps similar functionality
;; while allowing the function to execute from a user initiated
;; request.
(defun +eldoc-display-in-echo-area (docs _interactive)
  "Adjusts `eldoc-display-in-echo-area' when checking `this-command'.

`eldoc-display-in-echo-area': Display DOCS in echo area. Honor
`eldoc-echo-area-use-multiline-p' and
`eldoc-echo-area-prefer-doc-buffer'."
  (cond
   (;; Check if we have permission to mess with echo area at all.  For
    ;; example, if this-command is non-nil while running via an idle
    ;; timer, we're still in the middle of executing a command, e.g. a
    ;; query-replace where it would be annoying to overwrite the echo
    ;; area.
    (or
     (not (eldoc-display-message-no-interference-p))
     (and this-command (not (eq this-command 'eldoc-manual))) ;; Modification
     (not (eldoc--message-command-p last-command))))
   (;; If we do but nothing to report, clear the echo area.
    (null docs)
    (eldoc--message nil))
   (t
    ;; Otherwise, establish some parameters.
    (let*
        ((width (1- (window-width (minibuffer-window))))
         (val (if (and (symbolp eldoc-echo-area-use-multiline-p)
                       eldoc-echo-area-use-multiline-p)
                  max-mini-window-height
                eldoc-echo-area-use-multiline-p))
         (available (cl-typecase val
                      (float (truncate (* (frame-height) val)))
                      (integer val)
                      (t 'just-one-line)))
         single-doc single-doc-sym)
      (let ((echo-area-message
             (cond
              (;; To output to the echo area, we handle the
               ;; `truncate-sym-name-if-fit' special case first, by
               ;; checking for a lot of special conditions.
               (and
                (eq 'truncate-sym-name-if-fit eldoc-echo-area-use-multiline-p)
                (null (cdr docs))
                (setq single-doc (caar docs))
                (setq single-doc-sym
                      (format "%s" (plist-get (cdar docs) :thing)))
                (< (length single-doc) width)
                (not (string-match "\n" single-doc))
                (> (+ (length single-doc) (length single-doc-sym) 2) width))
               single-doc)
              ((and (numberp available)
                    (cl-plusp available))
               ;; Else, given a positive number of logical lines, we
               ;; format the *eldoc* buffer, using as most of its
               ;; contents as we know will fit.
               (with-current-buffer (eldoc--format-doc-buffer docs)
                 (save-excursion
                   (eldoc--echo-area-substring available))))
              (t ;; this is the "truncate brutally" situation
               (let ((string
                      (with-current-buffer (eldoc--format-doc-buffer docs)
                        (buffer-substring (goto-char (point-min))
                                          (progn (end-of-visible-line)
                                                 (point))))))
                 (if (> (length string) width)  ; truncation to happen
                     (unless (eldoc--echo-area-prefer-doc-buffer-p t)
                       (truncate-string-to-width string width))
                   (unless (eldoc--echo-area-prefer-doc-buffer-p nil)
                     string)))))))
        (when echo-area-message
          (eldoc--message echo-area-message)))))))

(defun +eldoc-print-current-symbol-info ()
  "`eldoc-print-current-symbol-info' but signals that this is a manual
trigger and allows use of the echo area."
  (interactive)
  (setq this-command 'eldoc-manual)
  (eldoc-print-current-symbol-info))

(setup eldoc
  (:option eldoc-idle-delay 600
           eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (:general
   (:states '(normal)
            :keymaps '(override)
            :prefix "SPC"
            "ci" '(+eldoc-print-current-symbol-info :which-key "Show documentation")))
  (:after-load
   (advice-add 'eldoc-display-in-echo-area :override '+eldoc-display-in-echo-area)
   (eldoc-add-command 'eldoc-manual)))

(provide 'init-eldoc)
;; init-eldoc.el ends here
