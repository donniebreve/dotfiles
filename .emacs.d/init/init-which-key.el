;;; init-which-key.el --- Which key initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (elpaca) (setup))

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
;; which-key is a minor mode for Emacs that displays the key bindings
;; following your currently entered incomplete command (a prefix) in a popup.
;; https://github.com/justbur/emacs-which-key

;;; Code:

(require 'elpaca)
(require 'setup)

(elpaca which-key
  (setup which-key
    (:option which-key-idle-delay 1
             which-key-sort-order 'which-key-key-order-alpha
             which-key-sort-uppercase-first nil
             which-key-add-column-padding 1
             which-key-max-display-columns nil
             which-key-min-display-lines 6
             which-key-side-window-slot -10)
    (which-key-mode)))

(provide 'init-which-key)
;;; init-which-key.el ends here
