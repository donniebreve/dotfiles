;;; init-font.el --- Font configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29"))

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
;; Font customzations and helpers.

;;; Code:

(defvar face-attribute-height 120
  "Default font face height when Emacs starts.")

(set-face-attribute 'default nil :font "JetBrains Mono" :height face-attribute-height)
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height face-attribute-height)

(defun face-attribute-height-increase ()
  (interactive)
  (setq face-attribute-height (+ face-attribute-height 5))
  (set-face-attribute 'default nil :height face-attribute-height))

(defun face-attribute-height-decrease ()
  (interactive)
  (setq face-attribute-height (- face-attribute-height 5))
  (set-face-attribute 'default nil :height face-attribute-height))

(define-key global-map (kbd "C-=") 'face-attribute-height-increase)
(define-key global-map (kbd "C--") 'face-attribute-height-decrease)

(provide 'init-font)
;; init-font.el ends here
