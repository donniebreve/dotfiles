;;; init-pcre2el.el --- pcre2el initialization -*- lexical-binding: t -*-

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
;; pcre2el: convert between PCRE, Emacs and rx regexp syntax.
;; https://github.com/joddie/pcre2el

;;; Code:

(require 'elpaca)
(require 'setup)

(elpaca pcre2el
  (setup pcre2el))

(provide 'init-pcre2el)
;; init-pcre2el.el ends here
