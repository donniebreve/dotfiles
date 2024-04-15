;;; init-consult.el --- Consult initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2024  donniebreve

;; Author: donniebreve <donniebreve@protonmail.com>
;; URL: https://github.com/donniebreve/dotfiles/.emacs.d
;; Version: 0
;; Package-Requires: ((emacs "29") (elpaca) (setup) (general) (evil))

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
;; Consult (search, completion)
;; A suite of useful commands using `completing-read`.
;; https://github.com/minad/consult

;;; Code:

(require 'elpaca)
(require 'setup)
(require 'general)
(require 'evil)

(elpaca consult
  (setup consult
    (:option completion-in-region-function #'consult-completion-in-region ;; the pcomplete-list window is awful! mouse!? X.X
             consult-grep-args '("rg"
                                 "--iglob !.#* --iglob !*.o --iglob !*~ --iglob !*.bin --iglob !*.bak --iglob !*.obj --iglob !*.map --iglob !*.ico --iglob !*.pif --iglob !*.lnk --iglob !*.a --iglob !*.ln --iglob !*.blg --iglob !*.bbl --iglob !*.dll --iglob !*.drv --iglob !*.vxd --iglob !*.386 --iglob !*.elc --iglob !*.lof --iglob !*.glo --iglob !*.idx --iglob !*.lot --iglob !*.fmt --iglob !*.tfm --iglob !*.class --iglob !*.fas --iglob !*.lib --iglob !*.mem --iglob !*.x86f --iglob !*.sparcf --iglob !*.dfsl --iglob !*.pfsl --iglob !*.d64fsl --iglob !*.p64fsl --iglob !*.lx64fsl --iglob !*.lx32fsl --iglob !*.dx64fsl --iglob !*.dx32fsl --iglob !*.fx64fsl --iglob !*.fx32fsl --iglob !*.sx64fsl --iglob !*.sx32fsl --iglob !*.wx64fsl --iglob !*.wx32fsl --iglob !*.fasl --iglob !*.ufsl --iglob !*.fsl --iglob !*.dxl --iglob !*.lo --iglob !*.la --iglob !*.gmo --iglob !*.mo --iglob !*.toc --iglob !*.aux --iglob !*.cp --iglob !*.fn --iglob !*.ky --iglob !*.pg --iglob !*.tp --iglob !*.vr --iglob !*.cps --iglob !*.fns --iglob !*.kys --iglob !*.pgs --iglob !*.tps --iglob !*.vrs --iglob !*.pyc --iglob !*.pyo --iglob !SCCS --iglob !RCS --iglob !CVS --iglob !MCVS --iglob !.src --iglob !.svn --iglob !.git --iglob !.hg --iglob !.bzr --iglob !_MTN --iglob !_darcs --iglob !{arch}"
                                 "--ignore-case --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip --hidden -g !.git -g !.svn -g !.hg"))
    (:general
     (:states '(normal)
              :keymaps '(override)
              :prefix "SPC"
              "."  '(consult-find :which-key "Find file")
              "<"  '(consult-buffer :which-key "Switch buffer")
              "fp" '(consult-find "~/.emacs.d/" :which-key "Open private configuration file")
              "fr" '(consult-recent-file :which-key "Recent files")
              "p/" '(consult-ripgrep :which-key "Find in project")))))

(provide 'init-consult)
;; init-consult.el ends here
