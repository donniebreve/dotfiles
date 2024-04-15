;;; init-visual-fill-column.el --- Visual fill column initialization -*- lexical-binding: t; -*-

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
;; visual-fill-column-mode is a small Emacs minor mode that mimics the effect of fill-column in visual-line-mode
;; https://codeberg.org/joostkremers/visual-fill-column

;;; Code:

(elpaca visual-fill-column
  (setup visual-fill-column
    (:option visual-fill-column-center-text t
             visual-fill-column-width 100)))

(provide 'init-visual-fill-column)
;; init-visual-fill-column.el ends here
