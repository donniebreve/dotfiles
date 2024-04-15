;;; init-orderless.el --- Orderless -*- lexical-binding: t -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (elpaca) (setup) (orderless))

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
;; Orderless setup and configuration.
;; Provides better filtering methods.
;; https://github.com/oantolin/orderless

;;; Code:

(require 'elpaca)
(require 'setup)

(elpaca orderless
  (setup orderless
    (:option completion-styles '(orderless basic)
             completion-category-overrides '((file (styles basic partial-completion))))))

(provide 'init-orderless)
;; init-orderless.el ends here
