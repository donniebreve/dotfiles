;;; init-helpful.el --- Helpful initialization -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (elpaca) (setup) (general))

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
;; Helpful is an alternative to the built-in Emacs help that provides much
;; more contextual information.
;; https://github.com/Wilfred/helpful

;;; Code:

(require 'elpaca)
(require 'setup)
(require 'general)

(elpaca helpful
  (setup helpful
    (:general
     (:states '(normal)
              :keymaps '(override)
              :prefix "SPC"
              "hf" '(helpful-callable :which-key "Describe function")
              "hk" '(helpful-key :which-key "Describe key")
              "hv" '(helpful-variable :which-key "Describe variable")
              "hx" '(helpful-command :which-key "Describe command")))))

(provide 'init-helpful)
;; init-helpful.el ends here
