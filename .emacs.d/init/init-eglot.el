;;; init-eglot.el --- Eglot configuration -*- lexical-binding: t; -*-

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
;; Eglot is the Emacs client for the Language Server Protocol (LSP).
;; https://www.gnu.org/software/emacs/manual/html_node/eglot/index.html

;;; Code:

(require 'setup)

(setup eglot
  (:option eglot-events-buffer-size 100000
           eglot-sync-connect nil
           eglot-connect-timeout 60
           eglot-ignored-server-capabilities
           '(;; :hoverProvider ;Documentation on hover
             ;; :completionProvider ;Code completion
             ;; :signatureHelpProvider ;Function signature help (Gives arguments when inside parentheses of function)
             :definitionProvider ;Go to definition
             :typeDefinitionProvider ;Go to type definition
             :implementationProvider ;Go to implementation
             :declarationProvider ;Go to declaration
             :referencesProvider ;Find references
             ;;:documentHighlightProvider    ;Highlight symbols automatically
             ;;:documentSymbolProvider        ;List symbols in buffer
             ;;:workspaceSymbolProvider      ;List symbols in workspace
             :codeActionProvider            ;Execute code actions
             ;;:codeLensProvider              ;Code lens
             :documentFormattingProvider   ;Format buffer
             :documentRangeFormattingProvider ;Format portion of buffer
             :documentOnTypeFormattingProvider ;On-type formatting
             ;; :renameProvider ;Rename symbol
             :documentLinkProvider            ;Highlight links in document
             ;;:colorProvider                   ;Decorate color references
             ;;:foldingRangeProvider            ;Fold regions of buffer
             :executeCommandProvider          ;Execute custom commands
             ;;:inlayHintProvider ;Inlay hints
             ))
  (:after-load
   (add-to-list 'eglot-server-programs
                '(vue-mode . ("vue-language-server" "--stdio" :initializationOptions
                              (:typescript (:tsdk "/usr/lib/node_modules/typescript/lib"
                                                  :languageFeatures (:completion
                                                                     (:defaultTagNameCase "both"
                                                                                          :defaultAttrNameCase "kebabCase"
                                                                                          :getDocumentNameCasesRequest nil
                                                                                          :getDocumentSelectionRequest nil)
                                                                     :diagnostics
                                                                     (:getDocumentVersionRequest nil))
                                                  :documentFeatures (:documentFormatting
                                                                     (:defaultPrintWidth 100
                                                                                         :getDocumentPrintWidthRequest nil)
                                                                     :documentSymbol t
                                                                     :documentColor t))))))
   (add-to-list 'eglot-server-programs
                '(csharp-mode . ("omnisharp" "-lsp")))
   (add-to-list 'eglot-server-programs
                '(csharp-ts-mode . ("omnisharp" "-lsp"))))
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c-ts-mode-hook #'eglot-ensure)
  (add-hook 'csharp-mode-hook #'eglot-ensure)
  (add-hook 'csharp-ts-mode-hook #'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
  (add-hook 'vue-mode-hook #'eglot-ensure))

(provide 'init-eglot)
;; init-eglot.el ends here
