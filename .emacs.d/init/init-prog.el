;;; init-prog.el --- Programming mode customzations -*- lexical-binding: t; -*-

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
;; Programming mode customizations. Also configures some specific
;; programming modes.

;;; Code:

(require 'setup)
(require 'evil)

(defun +evil-indent-buffer ()
  "Indent the entire buffer using the 'evil-indent' function."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (evil-indent (point-min) (point-max))))

(setup prog
  (:option tab-width 4
           indent-tabs-mode nil      ;; Don't use tabs
           truncate-lines t)         ;; Don't wrap lines in programming mode
  (:general
   (:states '(normal)
            :keymaps '(override)
            :prefix "SPC"
            "c"  '(:ignore t :which-key "Code")
            "cf" '(+evil-indent-buffer :which-key "Format buffer")))
  (:hook #'display-line-numbers-mode)) ;; Show line numbers

(setup emacs-lisp-mode
  (:general
   (:states '(normal)
            :keymaps '(override)
            :prefix "SPC"
            "e"   '(:ignore t :which-key "Eval...")
            "eb"  '(eval-buffer :which-key "Eval buffer")
            "ee"  '(eval-last-sexp :which-key "Eval s-exp")
            "ef"  '(eval-defun :which-key "Eval function")
            "er"  '(eval-region :which-key "Eval region"))))

(provide 'init-prog)
;; init-prog.el ends here
