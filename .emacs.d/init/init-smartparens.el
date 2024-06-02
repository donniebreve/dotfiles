;;; init-smartparens.el --- Smartparens initialization -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (elpaca) (setup) (general))

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
;; Smartparens is a minor mode for dealing with pairs in Emacs.
;; https://github.com/Fuco1/smartparens
;;
;; See https://github.com/Fuco1/smartparens/issues/80 for the newline idea

;;; Code:

(require 'elpaca)
(require 'setup)
(require 'general)

(defun +create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode)
  (sp-remove-active-pair-overlay))

(elpaca smartparens
  (setup smartparens
    (:hook-into c-mode)
    (:hook-into c-ts-mode)
    (:hook-into csharp-mode)
    (:hook-into csharp-ts-mode)
    (:hook-into emacs-lisp-mode)
    (require 'smartparens)
    (dolist (mode '(c-mode c-ts-mode csharp-mode csharp-ts-mode))
      (sp-local-pair mode "{" nil :post-handlers
                     '((+create-newline-and-enter-sexp "RET"))))))

(provide 'init-smartparens)
;; init-smartparens.el ends here
