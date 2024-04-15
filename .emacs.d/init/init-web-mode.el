;;; init-web-mode.el --- Web mode initialization -*- lexical-binding: t; -*-

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
;; web-mode.el is an autonomous emacs major-mode for editing web templates.
;; https://web-mode.org

;;; Code:

(elpaca web-mode
  (setup web-mode
    (define-derived-mode vue-mode web-mode "Vue")
    (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))))

(provide 'init-web-mode)
;; init-web-mode.el ends here
