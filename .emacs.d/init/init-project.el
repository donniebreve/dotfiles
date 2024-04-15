;;; init-project.el --- Project configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (setup) (consult) (project))

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
;; Project customzations and helpers.
;; https://vannilla.org/write/1609258895/article.html

;;; Code:

(cl-defmethod project-root ((project (head manual)))
  "Defines a project-root method that operates on a list that starts
with `'manual` and subsequently contains the root path."
  (car (cdr project)))

(defun +project-try-manual (path)
  "Determines if the project has been manually identified with a
`.project` file and then returns the root folder."
  (let ((root (locate-dominating-file path ".project")))
    (if root
        `(manual ,root)
      nil)))

(defun +project-switch-to-buffer ()
  "Use `project-switch-to-buffer` if current buffer is in a project,
otherwise `consult-buffer`"
  (interactive)
  (if (project--find-in-directory default-directory)
      (call-interactively #'project-switch-to-buffer)
    (call-interactively #'consult-buffer)))

(setup project
  (:load-after consult)
  (:general
   (:states '(normal)
            :keymaps '(override)
            :prefix "SPC"
            ","  '(+project-switch-to-buffer :which-key "Switch to project buffer")
            "p"  '(:ignore t :which-key "project")
            "p!" '(project-shell-command :which-key "Run cmd in project root")
            "p&" '(project-async-shell-command :which-key "Async cmd in project root")
            "pb" '(project-switch-to-buffer :which-key "Switch to project buffer")
            "pc" '(project-compile :which-key "Compile project")
            "pf" '(project-find-file :which-key "Find file in project")
            "pk" '(project-kill-buffers :which-key "Kill project buffers")
            "pp" '(project-switch-project :which-key "Switch project")))
  (customize-set-variable 'project-find-functions
                          '(project-try-vc
                            +project-try-manual)))

(provide 'init-project)
;; init-project.el ends here
