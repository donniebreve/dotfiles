;;; init-org.el --- Org configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (setup) (general) (org))

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
;; A GNU Emacs major mode for keeping notes, authoring documents,
;; computational notebooks, literate programming, maintaining to-do lists,
;; planning projects, and more — in a fast and effective plain text system.
;; https://orgmode.org

;;; Code:

(require 'setup)
(require 'general)

(defun +insert-current-date ()
  "Inserts the current date in Org format `<yyyy-MM-dd ddd>`"
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))

(setup org
  (:option 
   org-directory "~/Documents/Notes"
   org-agenda-files '("~/Documents/Notes")
   org-todo-keywords '((sequence "TODO(t)" "CURRENT(c)" "HOLD(h)" "|" "DONE(d)" "REMOVED(r)"))
   org-todo-keyword-faces '(("CURRENT" . org-level-3) ("HOLD" . org-warning)))
  (:general
   (:states '(normal)
            :keymaps '(org-mode-map)
            :prefix "SPC"
            "id" #'+insert-current-date)
   (:keymaps '(org-agenda-mode-map)
             "j" 'evil-next-line
             "k" 'evil-previous-line)))

(provide 'init-org)
;; init-org.el ends here
