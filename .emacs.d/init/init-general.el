;;; init-general.el --- General initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (elpaca) (setup) (evil))

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
;; general.el provides a more convenient method for binding keys in emacs (for both evil and non-evil users).
;; https://github.com/noctuid/general.el

;;; Code:

(require 'elpaca)
(require 'setup)

(elpaca general
  (setup general
    (setup-define :general
      (lambda (&rest definitions)
        (let (expansions)
          (dolist (definition definitions)
            (push `(general-define-key ,@definition) expansions))
          (macroexp-progn (nreverse expansions))))
      :documentation "Configure keybindings using general.")
    (general-auto-unbind-keys)))
(elpaca-wait) ;; Wait for completion

(provide 'init-general)
;;; init-general.el ends here
