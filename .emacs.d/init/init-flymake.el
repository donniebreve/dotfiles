;;; init-flymake.el --- Flymake configuration -*- lexical-binding: t; -*-

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
;; Flymake is a universal on-the-fly syntax checker for Emacs.
;; https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html#Top

;;; Code:

(require 'setup)

(defun +flymake-show-error-at-point ()
  "Display the Flymake error mesage for the current line in the minibuffer."
  (interactive)
  (let ((diags (flymake-diagnostics (point))))
    (if diags
        (message "%s" (mapconcat #'flymake-diagnostic-text diags "\n"))
      (message "No Flymake errors at point"))))

(setup flymake
  (:general
   (:states '(normal)
            :keymaps '(override)
            :prefix "SPC"
            "ce" '(+flymake-show-error-at-point :which-key "Show error"))))

(provide 'init-flymake)
;; init-flymake.el ends here
