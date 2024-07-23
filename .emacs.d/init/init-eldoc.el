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

(defun +elisp-get-fnsym-args-string (original-function symbol &rest args)
  "If SYM is a function, append its docstring."
  (concat
   (apply original-function symbol args)
   (if (fboundp symbol)
       (let ((docstring (documentation symbol 'raw)))
         (if (not (string-empty-p docstring))
             (concat "\n" (propertize docstring 'face '(:inherit (italic font-lock-comment-face)))))))))
(advice-add 'elisp-get-fnsym-args-string :around #'+elisp-get-fnsym-args-string)

(defun +eldoc-print-current-symbol-info (original-function &optional interactive &rest args)
  "Advice for `eldoc-print-current-symbol-info' to move focus the eldoc-buffer."
  (interactive '(t))
  (apply original-function interactive args)
  (pop-to-buffer (get-buffer eldoc--doc-buffer)))
(advice-add 'eldoc-print-current-symbol-info :around #'+eldoc-print-current-symbol-info)

(setup eldoc
  (:option eldoc-idle-delay 50
           eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
           ;; eldoc-echo-area-prefer-doc-buffer t
           max-mini-window-height 0.5)
  (:general
   (:states '(normal)
            :keymaps '(override)
            :prefix "SPC"
            "ci" '(eldoc-print-current-symbol-info :which-key "Show documentation"))))

(provide 'init-eldoc)
;; init-eldoc.el ends here
