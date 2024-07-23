;;; init-whitespace-mode.el --- whitespace-mode initialization -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (whitespace) (color))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistringibute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distringibuted in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Whitespace mode uses a number of faces to visualize the whitespace;
;; see the customization group whitespace for details.
;;
;; Apparently it isn't possible to disable whitespace-mode for company popups.
;; See: https://github.com/company-mode/company-mode/issues/1231
;; Workaround taken from company-mode github.
;;
;; '?' is the elisp syntax for "character". "?a" -> (97 a).  Lots of
;; examples use '?\ ' for space.  When displaying newline or tab, we
;; have to instruct whitespace mode to show the character we want and
;; then still show the special character. This is the reason for, for
;; example, [?» ?\t].

;;; Code:

(require 'whitespace)
(require 'color)

(setq whitespace-style '(face                       ;; use faces
                         spaces space-mark          ;; mark spaces
                         ;;newline newline-mark       ;; mark newlines
                         tabs tab-mark              ;; mark tabs
                         ))

(setq-default whitespace-display-mappings
              '((space-mark   ?\       [?·]     [?.])       ;; space -> · else .
                (newline-mark ?\n  [?¬ ?\n]     [?$ ?\n])   ;; new line -> ¬ else $
                (tab-mark     ?\t  [?» ?\t]     [?> ?\t])   ;; tabs -> » else >
                ))

;; Since we rely on the theme background color, wait until our load-theme-hook is called
(defun +set-whitespace-faces ()
  (let ((whitespace-color (color-lighten-name (face-attribute 'default :background) 20)))
    (set-face-attribute 'whitespace-space nil :foreground whitespace-color)
    (set-face-attribute 'whitespace-newline nil :foreground whitespace-color)
    (set-face-attribute 'whitespace-tab nil :foreground whitespace-color :background nil)))
(add-hook 'load-theme-hook #'+set-whitespace-faces)

;; Use company-mode workaround
;; Using " +" rather than " " might improve performance a little bit
(defun +company--add-whitespace-space-face (string)
  (if (and (or global-whitespace-mode whitespace-mode)
           (memq 'space-mark whitespace-active-style)
           (memq 'face whitespace-active-style)
           (memq 'spaces whitespace-active-style))
      (let ((face `(:foreground ,(face-attribute 'whitespace-space :foreground))))
        (replace-regexp-in-string
         " +"
         (lambda (match)
           (setq replacement (copy-sequence match))
           (add-face-text-property 0 (length replacement) face nil replacement)
           replacement)
         string))
    string))
(advice-add #'company--replacement-string :filter-return #'+company--add-whitespace-space-face)

(provide 'init-whitespace-mode)
;; init-whitespace-mode.el ends
