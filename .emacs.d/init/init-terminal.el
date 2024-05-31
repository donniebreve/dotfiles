;;; init-terminal.el --- Terminal configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (setup) (general) (ansi-term))

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
;; Terminal set up and configuration.

;;; Code:

(require 'setup)
(require 'general)

(if env-sys-linux-p (setq shell-file-name "/bin/fish"))
(if env-sys-mac-p (setq shell-file-name "/bin/zsh"))

(defun +ansi-term ()
  "Opens `ansi-term' with a specific shell, no prompt."
  (interactive)
  (ansi-term shell-file-name))

(setup ansi-term
  (:general
   (:states '(normal)
            :keymaps '(override)
            :prefix "SPC"
            "ot" '(+ansi-term :which-key "Open Terminal"))
   (:states '(normal)
            :keymaps '(term-mode-map)
            "q" '(kill-current-buffer :which-key "Quit"))))

(provide 'init-terminal)
;; init-terminal.el ends here
