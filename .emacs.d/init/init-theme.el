;;; init-theme.el --- Theming -*- lexical-binding: t -*-

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
;; Theme setup and customization.
;; https://github.com/doomemacs/themes

;;; Code:

(require 'elpaca)
(require 'setup)

(defvar load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(elpaca doom-themes
  (setup doom-themes
    (load-theme 'doom-nova t)
    ;;(load-theme 'doom-rose-pine t)))
    ;;(load-theme 'doom-rose-pine-moon t)))
    ;;(load-theme 'doom-rose-pine-dawn t)))
    (run-hooks 'load-theme-hook)))

(provide 'init-theme)
;; init-theme.el ends here
