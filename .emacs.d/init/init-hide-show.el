;;; init-hideshow.el --- Hideshow configuration -*- lexical-binding: t; -*-

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
;; Hideshow is a universal on-the-fly syntax checker for Emacs.
;; https://www.gnu.org/software/emacs/manual/html_node/hideshow/index.html#Top

;;; Code:

(require 'setup)

(defun +hs-hide-block (original-function &rest args)
  (interactive)
  (save-excursion
    (end-of-line)
    (apply original-function args)))
(advice-add #'hs-hide-block :around #'+hs-hide-block)

(defun +hs-show-block (original-function &rest args)
  (interactive)
  (save-excursion
    (beginning-of-line)
    (apply original-function args)))
(advice-add #'hs-show-block :around #'+hs-show-block)

(setup hideshow
  (:hook-into prog-mode))

(provide 'init-hideshow)
;; init-hideshow.el ends here
