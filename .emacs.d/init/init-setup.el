;;; init-setup.el --- setup.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (elpaca))

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
;; setup.el, a setup macro that simplifies repetitive configuration patterns
;; by providing context-sensitive local macros in setup bodies.
;; https://git.sr.ht/~pkal/setup
;; https://www.emacswiki.org/emacs/SetupEl

;;; Code:

(require 'elpaca)

(elpaca setup
  (require 'setup)
  (setup-define :after-load
    (lambda (&rest expressions)
      (macroexp-progn expressions))
    :documentation "Run EXPRESSIONS after feature is loaded."
    :after-loaded t)
  (setup-define :face
    (lambda (face spec) `(custom-set-faces (quote (,face ,spec))))
    :documentation "Customize FACE to SPEC."
    :signature '(face spec ...)
    :debug '(setup)
    :repeatable t
    :after-loaded t)
  (setup-define :file-match
    (lambda (glob)
      `(add-to-list 'auto-mode-alist (cons ,(wildcard-to-regexp pat) ',(setup-get 'mode))))
    :documentation "Associate the current mode with files that match GLOB."
    :debug '(form)
    :repeatable t)
  (setup-define :load-after
    (lambda (&rest features)
      (let ((body `(require ',(setup-get 'feature))))
        (dolist (feature (nreverse features))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
    :documentation "Load the current feature after FEATURES."))
(elpaca-wait) ;; Wait for completion

(provide 'init-setup)
;;; init-setup.el ends here
