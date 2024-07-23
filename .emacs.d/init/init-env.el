;;; init-env.el --- Environment initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29"))

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
;; Environment related customizations.

;;; Code:

(defconst env-graphic-p (display-graphic-p))
(defconst env-root-p (string-equal "root" (getenv "USER")))

(defconst env-sys-mac-p (eq system-type 'darwin))
(defconst env-sys-linux-p (eq system-type 'gnu/linux))
(defconst env-sys-windows-p
  (or
   (eq system-type 'ms-dos)
   (eq system-type 'windows-nt)
   (eq system-type 'cygwin)))

(defconst env-sys-name (system-name))

;; The default memory setting is too low for lsp/eglot due to the fact that client/server communication generates a lot of memory/garbage. This is 100mb.
(setq gc-cons-threshold (* 1024 1024 100))

;; Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024))

;; Disable native compilation warnings
(setq native-comp-async-report-warnings-errors nil)

;; Source code for built in functions
(setq find-function-C-source-directory "~/Projects/emacs-29.1/src/")

;; Save backup files to the temp directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

;; Ignore other version control systems
(setq vc-handled-backends '(Git))

;; Recent files
(setq recentf-max-saved-items 256) ; Remember up to this limit
(recentf-mode 1)                   ; Remember files

;; I was getting errors from project.el (and eglot.el) about invalid
;; arguments. Took a while, but finally figured out that find-program
;; default is just "find". I have find aliased to fd, and the
;; arguments aren't exactly the same. Really, Emacs should do this by
;; default.
(setq find-program (executable-find "find"))

(defun +advice-remove-all (symbol)
  "Remove all advice from symbol SYMBOL."
  (interactive)
  (message "advice for: %s" symbol)
  (advice--symbol-function symbol)
  (advice-mapc (lambda (function _props)
                 (message "removing: %s" function)
                 (advice-remove symbol function)) symbol))

(provide 'init-env)
;;; init-env.el ends here
