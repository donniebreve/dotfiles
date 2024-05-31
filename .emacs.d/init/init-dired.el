;;; init-dired.el --- Dired configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (setup) (general) (dired))

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
;; Dired set up and configuration. Also adds dired-fl for some nicer highlighting.

;;; Code:

(require 'setup)
(require 'general)

(defvar --dired-omit-mode t)
(defun +dired-omit-mode ()
  "An override for `dired-omit-mode`. Sets `dired-omit-mode` based on 
`--dired-omit-mode` and performs the necessary Dired action."
  (setq dired-omit-mode --dired-omit-mode)
  (if (eq dired-omit-mode t)
      (dired-omit-mode)
    (revert-buffer)))
(defun +dired-omit-mode-toggle ()
  "Toggles `--dired-omit-mode` to keep track of the desired `dired-omit-mode` for 
Dired buffers, and then executes `+dired-omit-mode`."
  (interactive)
  (if (eq --dired-omit-mode t)
      (setq --dired-omit-mode nil)
    (setq --dired-omit-mode t))
  (+dired-omit-mode))

(setup dired
  (:option dired-omit-files "\\`[.]\\|\\`[.]?#\\|\\`[.][.]?\\'"
           dired-kill-when-opening-new-dired-buffer t
           dired-dwim-target t)
  (:general
   (:states '(normal)
            :keymaps '(override)
            :prefix "SPC"
            "fd" '(dired-jump :which-key "Find directory"))
   (:states '(normal)
            :keymaps '(dired-mode-map)
            "<escape>" '(quit-window :which-key "Quit")
            [remap dired-do-compress-to] '(dired-create-empty-file :which-key "Create file") ;; c
            [remap dired-flag-file-deletion] '(dired-create-directory :which-key "Create directory") ;; d
            "h"  '(dired-up-directory :which-key "Go up a directory")
            "j"  '(dired-next-line :which-key "Next line")
            "k"  '(dired-previous-line :which-key "Previous line")
            "l"  '(dired-find-file :which-key "Open file or directory")
            "zh" '(+dired-omit-mode-toggle :which-key "Toggle file hiding")))
  (:hook #'+dired-omit-mode))

(elpaca diredfl
  (setup diredfl
    (:load-after dired)
    (diredfl-global-mode)))

(provide 'init-dired)
;; init-dired.el ends here
