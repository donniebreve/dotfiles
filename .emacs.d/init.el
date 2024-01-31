;; Reference
;; https://github.com/daviwil/emacs-from-scratch/blob/master/init.el

;; Find source code for built in functions
(setq find-function-C-source-directory "~/Projects/emacs-29.1/src/")
;; The default memory setting is too low for lsp/eglot due to the fact that client/server communication generates a lot of memory/garbage. This is 100mb.
(setq gc-cons-threshold (* 100 1000 1000))
;; Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024))
;; Set the window size
(setq initial-frame-alist '((width . (text-pixels . 1600)) (height . (text-pixels . 1050))))

(menu-bar-mode -1)                    ; Disable the menu bar
(tool-bar-mode -1)                    ; Disable the toolbar
(scroll-bar-mode -1)                  ; Disable visible scrollbar
(tooltip-mode -1)                     ; Disable tooltips
(savehist-mode 1)                     ; Save minibuffer history

(column-number-mode)                  ; Show column number in modeline
(global-visual-line-mode 1)           ; Always use visual lines (never logical lines)
(display-time-mode t)                 ; Displays the time in the modeline

(global-display-line-numbers-mode t)  ; Show line numbers everywhere
(setq display-line-numbers-width 3)   ; Use at least 3 columns for line numbers

(setq inhibit-startup-message t)      ; Don't show start up screen
(setq visible-bell nil)               ; Don't flash
(setq ring-bell-function 'ignore)     ; Don't ring
(setopt use-short-answers t)          ; Use y/n instead of yes/no

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq vc-handled-backends '(Git))

;; Sets the margin while scrolling (vim=scrolloff)
(setq scroll-margin 16)
;; Don't jump (https://www.reddit.com/r/emacs/comments/fwmqc8/how_to_stop_emacs_from_half_scrolling_from_bottom/)
(setq scroll-conservatively 101)
;; (Turns this off for specific modes)
(add-hook 'term-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin 0)))
(add-hook 'comint-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin 0)))

;; Center the visual fill column mode
(setq-default visual-fill-column-center-text t)

;; Font
(set-face-attribute 'default nil :font "JetBrains Mono-10")
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono-10")

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; I like ctrl+shift+p to get to the "actions menu"
(global-set-key (kbd "C-S-p") 'execute-extended-command)
;; Text scale
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Quit warnings
;; Taken from DOOM Emacs!
(global-set-key [remap save-buffers-kill-terminal] #'save-buffers-kill-emacs)
(defvar +doom-quit-messages
  `(;; from Doom 1
    "Please don't leave, there's more demons to toast!"
    "Let's beat it -- This is turning into a bloodbath!"
    ,(format "I wouldn't leave if I were you. %s is much worse."
             (if (member system-type '(ms-dos-windows-nt cygwin))
                 "DOS"
               "UNIX"))
    "Don't leave yet -- There's a demon around that corner!"
    "Ya know, next time you come in here I'm gonna toast ya."
    "Go ahead and leave. See if I care."
    "Are you sure you want to quit this great editor?"
    ;; from Portal
    "Thank you for participating in this Aperture Science computer-aided enrichment activity."
    "You can't fire me, I quit!"
    "I don't know what you think you are doing, but I don't like it. I want you to stop."
    "This isn't brave. It's murder. What did I ever do to you?"
    "I'm the man who's going to burn your house down! With the lemons!"
    "Okay, look. We've both said a lot of things you're going to regret..."
    ;; Custom
    "(setq nothing t everything 'permitted)"
    "Emacs will remember that."
    "Emacs, Emacs never changes."
    "Hey! Hey, M-x listen!"
    "It's not like I'll miss you or anything, b-baka!"
    "Wake up, Mr. Stallman. Wake up and smell the ashes."
    "You are *not* prepared!"
    "Please don't go. The drones need you. They look up to you.")
  "A list of quit messages, picked randomly by `+doom-quit'. Taken from
http://doom.wikia.com/wiki/Quit_messages and elsewhere.")
(defun doom-quit-p (&optional prompt)
  "Prompt the user for confirmation when killing Emacs.
Returns t if it is safe to kill this session. Does not prompt if no real buffers
are open."
  (or ;;(not (ignore-errors (doom-real-buffer-list))) ;; we don't have this function
      (yes-or-no-p (format "%s" (or prompt "Really quit Emacs?")))
      (ignore (message "Aborted"))))
(defun +doom-quit-fn (&rest _)
  (doom-quit-p
   (format "%s"
           (propertize (nth (random (length +doom-quit-messages))
                            +doom-quit-messages)
                       'face '(italic default)))))
(setq confirm-kill-emacs #'+doom-quit-fn)

;; Tree sitter
;; Tree sitter is kind of a mess right now, so get the tree-sitter-langs package
;; then copy ~/.emacs.d/straight/build/tree-sitter-langs/bin to ~/.emacs.d/tree-sitter
;; then rename all *.dll to libtree-sitter-*.dll
;; then add modes here to change them from normal to tree-sitter
;; TODO: keep an eye on the state of things and update this section
(add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(csharp-mode . csharp-ts-mode))



;; Packages

;; straight.el (package management)
;; this has to come first since it is the package management backend
;; https://github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; general.el (key bindings)
;; this should be loaded early to allow using the :general keyword
;; https://github.com/noctuid/general.el
(use-package general
  :demand t
  :config
  (general-auto-unbind-keys)
  (general-evil-setup))

;; avy (movement)
;; https://github.com/abo-abo/avy
(use-package avy
  :config
  (avy-setup-default)
  :general
  (:states '(normal)
       "gss" 'evil-avy-goto-char-2
       "gs/" 'evil-avy-goto-char-timer))

;; company (completion)
;; https://github.com/company-mode/company-mode
(use-package company
  :hook
  (prog-mode . company-mode)
  (text-mode . company-mode)
  :general
  (:keymaps '(company-active-map)
    "<escape>" '(lambda () (interactive) (company-abort) (evil-normal-state)))) ;; esc should abort company and insert mode

;; consult (search, completion)
;; suite of useful commands using `completing-read`
;; https://github.com/minad/consult
(use-package consult
  :config
  (setq completion-in-region-function #'consult-completion-in-region) ;; the pcomplete-list window is awful! mouse!? X.X
  (setq consult-grep-args '("rg"
                             "--iglob !.#* --iglob !*.o --iglob !*~ --iglob !*.bin --iglob !*.bak --iglob !*.obj --iglob !*.map --iglob !*.ico --iglob !*.pif --iglob !*.lnk --iglob !*.a --iglob !*.ln --iglob !*.blg --iglob !*.bbl --iglob !*.dll --iglob !*.drv --iglob !*.vxd --iglob !*.386 --iglob !*.elc --iglob !*.lof --iglob !*.glo --iglob !*.idx --iglob !*.lot --iglob !*.fmt --iglob !*.tfm --iglob !*.class --iglob !*.fas --iglob !*.lib --iglob !*.mem --iglob !*.x86f --iglob !*.sparcf --iglob !*.dfsl --iglob !*.pfsl --iglob !*.d64fsl --iglob !*.p64fsl --iglob !*.lx64fsl --iglob !*.lx32fsl --iglob !*.dx64fsl --iglob !*.dx32fsl --iglob !*.fx64fsl --iglob !*.fx32fsl --iglob !*.sx64fsl --iglob !*.sx32fsl --iglob !*.wx64fsl --iglob !*.wx32fsl --iglob !*.fasl --iglob !*.ufsl --iglob !*.fsl --iglob !*.dxl --iglob !*.lo --iglob !*.la --iglob !*.gmo --iglob !*.mo --iglob !*.toc --iglob !*.aux --iglob !*.cp --iglob !*.fn --iglob !*.ky --iglob !*.pg --iglob !*.tp --iglob !*.vr --iglob !*.cps --iglob !*.fns --iglob !*.kys --iglob !*.pgs --iglob !*.tps --iglob !*.vrs --iglob !*.pyc --iglob !*.pyo --iglob !SCCS --iglob !RCS --iglob !CVS --iglob !MCVS --iglob !.src --iglob !.svn --iglob !.git --iglob !.hg --iglob !.bzr --iglob !_MTN --iglob !_darcs --iglob !{arch}"
                             "--ignore-case --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip --hidden -g !.git -g !.svn -g !.hg"))
  :general
  (:states '(normal)
       :keymaps '(override)
       :prefix "SPC"
       "p/" '(consult-ripgrep :which-key "Find in project")))

;; dashboard (ui)
;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :init
  (setq dashboard-startup-banner "~/.emacs.d/banners/sub-zero.txt")
  (setq dashboard-center-content t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-items '((recents  . 5)
                           (projects . 5)
                           (agenda . 5)
                           (bookmarks . 5)))
  :config
  (dashboard-setup-startup-hook))

;; diff-hl (coding)
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :hook
  (prog-mode . diff-hl-mode))

;; Dired
(use-package dired
  :straight
  (:type built-in)
  :init
  (setq dired-omit-files "\\`[.]\\|\\`[.]?#\\|\\`[.][.]?\\'")
  (setq dired-kill-when-opening-new-dired-buffer t)
  :hook
  (dired-mode . dired-omit-mode)
  :general
  (:states '(normal)
	  :keymaps '(override)
	  :prefix "SPC"
	  "fd" '(dired-jump :which-key "Find directory"))
  (:states '(normal)
    :keymaps '(dired-mode-map)
    "h"  '(dired-up-directory :which-key "Go up a directory")
    "j"  '(dired-next-line :which-key "Next line")
    "k"  '(dired-previous-line :which-key "Previous line")
    "l"  '(dired-find-file :which-key "Open file or directory")
    "zh" '(dired-omit-mode :which-key "Toggle file hiding")))

;; dired-fl (appearance)
;; additional highlighting for dired
;; https://github.com/purcell/diredfl
(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode))

;; doom-themes (appearance)
;; https://github.com/doomemacs/themes
(use-package doom-themes
  :init
  (load-theme 'doom-solarized-light t))

;; editorconfig-emacs
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :config
  (editorconfig-mode 1)
  :hook
  (prog-mode . editorconfig-mode))

;; eglot
(use-package eglot
  :straight
  (:type built-in)
  :init
  (setq eglot-events-buffer-size 100000)
  (setq eglot-sync-connect nil)
  (setq eglot-connect-timeout 60)
  (setq eglot-ignored-server-capabilities
    '(
	;; :hoverProvider ;Documentation on hover
	;; :completionProvider ;Code completion
	;; :signatureHelpProvider ;Function signature help (Gives arguments when inside parentheses of function)
	;; :definitionProvider ;Go to definition
	;; :typeDefinitionProvider ;Go to type definition
	;; :implementationProvider ;Go to implementation
	;; :declarationProvider ;Go to declaration
	;; :referencesProvider ;Find references
        :documentHighlightProvider    ;Highlight symbols automatically
        :documentSymbolProvider	      ;List symbols in buffer
        :workspaceSymbolProvider      ;List symbols in workspace
        :codeActionProvider	      ;Execute code actions
        :codeLensProvider	      ;Code lens
        :documentFormattingProvider   ;Format buffer
        :documentRangeFormattingProvider ;Format portion of buffer
        :documentOnTypeFormattingProvider ;On-type formatting
        ;; :renameProvider ;Rename symbol
        :documentLinkProvider		;Highlight links in document
        :colorProvider			;Decorate color references
        :foldingRangeProvider		;Fold regions of buffer
        :executeCommandProvider		;Execute custom commands
        :inlayHintProvider		;Inlay hints
	))
  :config
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
  :hook
  (c-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (csharp-mode . eglot-ensure)
  (csharp-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (vue-mode . eglot-ensure))

;; eldoc
(use-package eldoc
  :straight
  (:type built-in)
  :init
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

;; eshell (shell)
(use-package eshell
  :straight
  (:type built-in)
  :general
  (:states '(insert)
	   :keymaps '(eshell-mode-map)
	   "TAB" 'completion-at-point)) ;; the evil tab keybinding screws up tab completion in eshell

;; eshell-toggle (ui, terminal)
;; https://github.com/4DA/eshell-toggle
(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  :general
  (:states '(normal)
	   :keymaps '(override)
	   :prefix "SPC"
	   "oe" '(eshell-toggle :which-key "Toggle Eshell")))

;; esup (profiling)
;; https://github.com/jschaf/esup
(use-package esup
  :init
  (setq esup-depth 0)) ;; see https://github.com/jschaf/esup/issues/85

;; evil (vi, vim, key bindings)
;; https://github.com/emacs-evil/evil
(use-package evil
  :demand t
  :init
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-u-scroll t)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-move-cursor-back nil)
  (setq evil-move-beyond-eol t)
  (setq evil-cross-lines t)
  (setq evil-ex-substitute-global t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-want-fine-undo t)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1)
  :general
  (:states '(normal)
	   "<escape>" 'evil-ex-nohighlight)
  (:states '(insert)
	   "TAB" 'tab-to-tab-stop)
  (:states '(normal visual)
	   "H" 'evil-beginning-of-line
	   "L" 'evil-end-of-line
	   "U" 'evil-redo)
  (:states '(normal)
	   :keymaps '(override)
	   :prefix "SPC"
	   "qQ" '(evil-quit-all-with-error-code :which-key "Quit Emacs")
	   "w"  '(evil-window-map :which-key "window")
	   "wd" '(evil-window-delete :which-key "Kill window")))
(use-package evil-collection
  :demand t
  :after evil
  :config
  (evil-collection-init))
(use-package evil-commentary
  :demand t
  :after evil
  :config
  (evil-commentary-mode))
(use-package evil-surround
  :demand t
  :after evil
  :config
  (global-evil-surround-mode 1))
(use-package evil-textobj-anyblock
  :demand t
  :after evil
  :config
  ;; Remove quotes from blocks
  (setq evil-textobj-anyblock-blocks '(("(" . ")") ("{" . "}") ("\\[" . "\\]") ("<" . ">"))) 
  ;; Set up quotes separately
  (setq evil-textobj-anyblock-quotes '(("'" . "'") ("\"" . "\"") ("`" . "`") ("“" . "”")))
  (evil-define-text-object evil-textobj-anyblock-inner-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks evil-textobj-anyblock-quotes))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))
  (evil-define-text-object evil-textobj-anyblock-a-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks evil-textobj-anyblock-quotes))
      (evil-textobj-anyblock--make-textobj beg end type count t)))
  :general
  (:keymaps '(evil-inner-text-objects-map)
    "b" 'evil-textobj-anyblock-inner-block
    "q" 'evil-textobj-anyblock-inner-quote)
  (:keymaps '(evil-outer-text-objects-map)
    "b" 'evil-textobj-anyblock-a-block
    "q" 'evil-textobj-anyblock-a-quote))

;; helpful (help)
;; https://github.com/Wilfred/helpful
(use-package helpful
  :general
  (:states '(normal)
     :keymaps '(override)
     :prefix "SPC"
     "hf" '(helpful-callable :which-key "Describe function")
     "hk" '(helpful-key :which-key "Describe key")
     "hv" '(helpful-variable :which-key "Describe variable")
     "hx" '(helpful-command :which-key "Describe command")))

;; hl-todo
(use-package hl-todo
  :config
  (setq hl-todo-highlight-punctuation ":"
    hl-todo-keyword-faces
    '(("TODO" warning bold)
       ("DEPRECATED" font-lock-doc-face bold)
       ("REVIEW" font-lock-keyword-face bold)
       ("NOTE" success bold)
       ("HACK" font-lock-constant-face bold)
	     ("FIXME" error bold)
       ("BUG" error bold)))
  :hook
  (prog-mode . hl-todo-mode)
  (yaml-mode . hl-todo-mode))

;; magit (version control)
;; https://github.com/magit/magit
(use-package magit
  :defer 10
  :general
  (:states '(normal)
	  :keymaps '(override)
	  :prefix "SPC"
    "g/"  '(magit-dispatch :which-key "Magit dispatch")
    "g."  '(magit-file-dispatch :which-key "Magit file dispatch")
    "gb"  '(magit-branch-checkout :which-key "Magit checkout branch")
    "gB"  '(magit-blame-addition :which-key "Magit blame addition")
    "gc"  '(:ignore t :which-key "Magit create")
    "gcc" '(magit-commit-create :which-key "Magit create commit")
    "gcf" '(magit-commit-fixup :which-key "Magit create fixup")
    "gC"  '(magit-clone :which-key "Magit clone")
	  "gD"  '(magit-file-delete :which-key "Magit file delete")
    "gf"  '(:ignore t :which-key "Magit find")
    "gfc" '(magit-show-commit :which-key "Magit find commit")
    "gff" '(magit-find-file :which-key "Magit find file")
    "gfg" '(magit-find-git-config-file :which-key "Magit find gitconfig file")
    "gF"  '(magit-fetch :which-key "Magit fetch")
	  "gg"  '(magit-status :which-key "Magit status")
	  "gG"  '(magit-status-here :which-key "Magit status here")
    "gI"  '(magit-init :which-key "Magit initialize repo")
    "gl"  '(:ignore t :which-key "Magit list")
    "glr" '(magit-list-repositories :which-key "Magit list repositories")
    "gls" '(magit-list-submodules :which-key "Magit list submodules")
    "gL"  '(magit-log-buffer-file :which-key "Magit buffer log")
    "gS"  '(magit-stage-file :which-key "Magit stage file")
    "gU"  '(magit-unstage-file :which-key "Magit unstage file")))

;; magit-todos (comments, version control)
;; https://github.com/alphapapa/magit-todos
(use-package magit-todos
  :after magit
  :init
  (if (eq system-type 'windows-nt)
    (setq magit-todos-nice nil)) ;; don't use `nice` on windows, it doesn't exist T_T
  :config
  ;; https://github.com/alphapapa/magit-todos/issues/156
  (if (eq system-type 'windows-nt)
    (setq magit-todos-scanners nil)
    (magit-todos-defscanner "rg"
			:test (executable-find "rg")
			:directory-form (f-relative directory default-directory) ;; revert
			:allow-exit-codes (0 1)
			:command (list "rg" "--no-heading" "--line-number"
					       (when depth
					         (list "--maxdepth" (1+ depth)))
					       (when magit-todos-ignore-case
					         "--ignore-case")
					       (when magit-todos-exclude-globs
					         (--map (list "--glob" (concat "!" it))
						         magit-todos-exclude-globs))
					       (unless magit-todos-submodule-list
					         (--map (list "--glob" (concat "!" it))
						         (magit-list-module-paths)))
					       extra-args search-regexp-pcre directory)))
  :general
  (:states '(normal)
	  :keymaps '(override)
	  :prefix "SPC"
	  "pt" '(magit-todos-list :which-key "List project todos")))

;; marginalia.el (completion, annotations) 
;; provides annotations for completion candidates
;; https://github.com/minad/marginalia
(use-package marginalia
  :init
  (marginalia-mode))

;; markdown (coding, writing)
;; https://jblevins.org/projects/markdown-mode
(use-package markdown-mode
  :defer t
  :mode
  ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "multimarkdown"))

;; mood-line
;; https://gitlab.com/jessieh/mood-line
(use-package mood-line
  :config
  (set-face-attribute 'mood-line-unimportant nil
    :foreground "#839496")
  (mood-line-mode)
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

;; orderless (completion, filtering)
;; better filtering methods
;; https://github.com/oantolin/orderless
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; org (notes)
;; built-in
(use-package org
  :straight
  (:type built-in)
  :config
  (setq org-directory "~/Documents/Notes")
  (setq org-agenda-files '("~/Documents/Notes"))
  :general
  (:keymaps '(org-agenda-mode-map)
    "j" 'evil-next-line
    "k" 'evil-previous-line))

;; persp-mode (buffers)
;; https://github.com/Bad-ptr/persp-mode.el
(use-package persp-mode
  :init
  (setq persp-auto-save-opt 0))

;; prettier.el (formatting, coding)
;; prettier code formatting
;; https://github.com/jscheid/prettier.el
(use-package prettier
  :defer t
  :hook
  (prog-mode . prettier-mode))

;; projectile.el (formatting, coding)
;; allows for separate project spaces
;; https://github.com/bbatsov/projectile
(use-package projectile
  :init
  (projectile-mode +1)
  :general
  (:states '(normal)
     :keymaps '(override)
	   :prefix "SPC"
	   "p"  '(:ignore t :which-key "project")
	   ;;"p." '(browse-project :which-key "Browse project")
	   ;;"p>" '(browse-in-other-project :which-key "Browse other project")
	   "p!" '(projectile-run-shell-command-in-root :which-key "Run cmd in project root")
	   "p&" '(projectile-run-async-shell-command-in-root :which-key "Async cmd in project root")
	   "pa" '(projectile-add-known-project :which-key "Add new project")
	   "pb" '(projectile-switch-to-buffer :which-key "Switch to project buffer")
	   "pc" '(projectile-compile-project :which-key "Compile in project")
	   "pC" '(projectile-repeat-last-command :which-key "Repeat last command")
	   "pd" '(projectile-remove-known-project :which-key "Remove known project")
	   "pe" '(projectile-edit-dir-locals :which-key "Edit project .dir-locals")
	   "pf" '(projectile-find-file :which-key "Find file in project")
	   "pg" '(projectile-configure-project :which-key "Configure project")
	   "pi" '(projectile-invalidate-cache :which-key "Invalidate project cache")
	   "pk" '(projectile-kill-buffers :which-key "Kill project buffers")
	   "po" '(projectile-find-other-file :which-key "Find other file")
	   "pp" '(projectile-switch-project :which-key "Switch project")
	   "pr" '(projectile-recentf :which-key "Find recent project files")
	   "pR" '(projectile-run-project :which-key "Run project")
	   "ps" '(projectile-save-project-buffers :which-key "Save project files")
	   "pT" '(projectile-test-project :which-key "Test project")))

;; transient
;; used by magit
;; https://github.com/magit/transient
(use-package transient
  :general
  (:keymaps '(transient-map)
	  "<escape>" 'transient-quit-one))

;; tree-sitter-langs
;; precompiled tree sitter languages
;; https://github.com/emacs-tree-sitter/tree-sitter-langs
(use-package tree-sitter-langs
  :defer t)

;; vertico (completion)
;; vertical completion user interface
;; https://github.com/minad/vertico
(use-package vertico
  :init
  (vertico-mode)
  :general
  (:states '(normal)
	   :keymaps '(override)
	   :prefix "SPC"
	   "'" '(vertico-repeat :which-key "Resume last search")))

;; web-mode (coding)
;; https://github.com/fxbois/web-mode
(use-package web-mode
  :init
  (define-derived-mode vue-mode web-mode "Vue")
  :mode
  ("\\.vue\\'" . vue-mode))

;; which-key.el (key bindings)
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :init
  (setq which-key-idle-delay 1)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-sort-uppercase-first nil)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-display-columns nil)
  (setq which-key-min-display-lines 6)
  (setq which-key-side-window-slot -10)
  :config
  (which-key-mode))



;; Global keybindings
(general-define-key
 :states '(normal)
 :keymaps '(override)
 :prefix "SPC"
 ;; Global
 ";" '(pp-eval-expression :which-key "Eval expression")
 ":" '(execute-extended-command :which-key "M-x")
 "x" '(scratch-buffer :which-key "Switch to scratch buffer")
 "X" '(org-capture :which-key "Org capture")
 "u" '(universal-argument :which-key "Universal argument")
 "." '(find-file :which-key "Find file")
 "," '(switch-to-buffer :which-key "Switch buffer")
 ;; Buffer
 "b" '(:ignore t :which-key "buffer")
 "b[" '(previous-buffer :which-key "Previous buffer")
 "b]" '(next-buffer :which-key "Next buffer")
 "bd" '(kill-current-buffer :which-key "Kill buffer")
 ;; File
 "f" '(:ignore t :which-key "file")
 "fp" '(find-file "~/.emacs.d/" :which-key "Open private configuration file")
 "fr" '(recentf-open :which-key "Recent files")
 "fs" '(save-buffer :which-key "Save file")
 "fS" '(write-file :which-key "Save file as...")
 ;; Git
 "g"  '(:ignore t :which-key "git")
 ;; Help
 "h"  '(:ignore t :which-key "help")
 "h'" '(describe-char :which-key "Describe char")
 ;; Open
 "o"  '(:ignore t :which-key "open")
 ;; Quit
 "q"  '(:ignore t :which-key "quit")
 "qq" '(save-buffers-kill-emacs :which-key "Quit Emacs"))
