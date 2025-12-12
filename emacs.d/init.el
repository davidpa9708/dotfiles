;; emacs options
(setopt
 frame-resize-pixelwise t
 inhibit-startup-screen t
 ring-bell-function nil
 custom-file "~/.emacs.d/emacs-custom.el"
 create-lockfiles nil
 make-backup-files nil
 use-short-answers t
 auto-save-default nil
 tab-width 4
 warning-minimum-level :emergency
 ;; search-whitespace-regexp ".*"
 ;; project-mode-line t
 speedbar-show-unknown-files t

 scroll-preserve-screen-position nil
 mouse-wheel-follow-mouse t
 pixel-scroll-precision-use-momentum t
 fast-but-imprecise-scrolling t
 ;; jit-lock-defer-time 0
 isearch-wrap-pause 'no
 column-number-mode t
 project-mode-line t
 enable-recursive-minibuffers t

 minibuffer-follows-selected-frame nil ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Basic-Minibuffer.html

 ;; org
 org-support-shift-select t
 )

;; (cua-mode) ;; C-x to cut on selection https://www.emacswiki.org/emacs/CuaMode
;; (transient-mark-mode nil)
(global-display-line-numbers-mode) ;; display line numbers
;; (electric-pair-mode)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode t)
(delete-selection-mode 1)
(global-auto-revert-mode)

;; (recent-mode)

(add-to-list 'default-frame-alist
             '(font . "SourceCodePro-12"))


;; straight https://github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t) ;; always use straight

;; https://www.gnu.org/software/emacs/manual/html_mono/use-package.html#Install-package
;; go fetch packages
;; might not need it with straight
(require 'use-package-ensure)

;; (use-package fast-scroll
;;   :defer 1
;;   :hook
;;   (fast-scroll-start . (lambda () (flycheck-mode -1)))
;;   (fast-scroll-end . (lambda () (flycheck-mode 1)))
;;   :config
;;   (fast-scroll-config)
;;   (fast-scroll-mode 1))

(defun my/text-scale-increase ()
  "Increase font size."
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ old-face-attribute 10))))

(defun my/text-scale-decrease ()
  "Decrease font size."
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- old-face-attribute 10))))


(defun my/next ()
  "Scroll down and recenter."
  (interactive "^")
  (forward-line (/ (window-total-height) 2))
  (back-to-indentation)
  (recenter)
  )

(defun my/prior ()
  "Scroll up and recenter."
  (interactive "^")
  (forward-line (- (/ (window-total-height) 2)))
  (back-to-indentation)
  (recenter)
  )

(defun my/copy-path-file ()
  "Copy path file name."
  (interactive)
  (kill-new buffer-file-name)
  )

;; https://www.emacswiki.org/emacs/NavigatingParentheses#h5o-3
(defun my/goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis. Else go to the
   opening parenthesis one level up."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))
                  ))))))

;; https://stackoverflow.com/a/51445691
(defun my/get-selected-text ()
  (interactive)
  (if (use-region-p)
      (let ((regionp (buffer-substring (region-beginning) (region-end))))
        (deactivate-mark)
        regionp)))

(defun my/goto-line (arg)
  "Go to other window and go to line."
  (interactive "p")
  (call-interactively 'other-window)
  (forward-line (-  (my/get-selected-text) (string-to-number (format-mode-line "%l"))))
  )

(defun my/view-buffer-other-frame (arg)
  "Go to other window and go to line."
  (interactive "p")
  (view-buffer-other-frame (current-buffer))
  )

(defun my/right-word (arg)
  "Right word."
  (interactive "^p")
  (forward-same-syntax arg)
  ;; (re-search-forward
  ;;  (if (> arg 0)
  ;; 	   "\\S-\\s-"
  ;; 	 "\\s-\\S-")
  ;;  nil t arg)
  ;; (if (not (eolp))
  ;; (backward-char arg)
  ;; )
  )


(defun my/left-word (arg)
  "Left word"
  (interactive "^p")
  (my/right-word (* -1 arg)))

(defun my/kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-region (point) (progn (my/right-word arg) (point))))

(defun my/backward-kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (my/kill-word (* -1 arg)))

(defun my/tab-bar-move-next ()
  ""
  (interactive)
  (tab-bar-move-tab 1))

(defun my/tab-bar-move-previous ()
  ""
  (interactive)
  (tab-bar-move-tab -1))


;; keybindings
(bind-keys
 ;; navigation
 ;; ("C-<right>" .  my/right-word)
 ;; ("C-<left>" .  my/left-word)
 ;; ("C-<delete>" . my/kill-word)
 ;; ("C-<backspace>" . my/backward-kill-word)
 ("<next>" . my/next)
 ("<prior>" . my/prior)
 ("<home>" . back-to-indentation) ;; https://stackoverflow.com/a/12346740
 ("C-e" . next-error)
 ("C-S-E" . previous-error)
 ("C-|" . my/goto-match-paren)
 ("C-\"" . my/goto-match-paren)
 ;; font / theme
 ("C--" . my/text-scale-decrease)
 ("C-=" . my/text-scale-increase)
 ("<f1>" . execute-extended-command)
 ("<f6>" . load-theme)
 ;; others
 ("C-." . completion-at-point)
 ("C-f" . isearch-forward)
 ("C-S-f" . project-search)
 ("C-s" . save-buffer)
 ("C-z" . undo)
 ("C-S-z" . undo-redo)
 ("<mouse-3>" . context-menu-open)
 ("S-<down-mouse-1>" . mouse-set-mark)
 ("C-b" . switch-to-buffer)
 ("C-p" . project-find-file)
 ("C-S-p" . project-switch-project)
 ("<mouse-3>" . context-menu-open)
 ("C-o" . find-file)
 ;; ("C-i" . imenu)	
 ("C-l" . nil) ; lsp-prefix
 ;; ("<escape>" . keyboard-escape-quit)
 ("<end>" . move-end-of-line)
 ;; ("TAB" . self-insert-command)
 ("M-?" . goto-line)
 ("C-<escape>" . keyboard-quit)
 ("C-o" . other-window)
 ("C-:" . my/view-buffer-other-frame)
 ;; ("C-1" . delete-other-windows)
 ;; ("C-2" . split-window-below)
 ;; ("C-3" . split-window-right)
 ;; ("C-0" . delete-window)
 ;; ("C-;" . query-replace-regexp)
 ("C-S-SPC" . exchange-point-and-mark)
 ;; ("C-SPC" . execute-extended-command)
 ("C-S-w" . tab-close)
 ("C-S-t" . tab-new)
 ("C-<next>" . tab-next)
 ("C-<prior>" . tab-previous)
 ("C-S-<next>" .  my/tab-bar-move-next)
 ("C-S-<prior>" .  my/tab-bar-move-previous)
 ("C-!" .  hs-hide-level)
 ("C-@" .  hs-show-all)
 ("C-#" .  hs-toggle-hiding)
 )

(use-package undo-fu
  :init
  ;; https://github.com/emacsmirror/undo-fu?tab=readme-ov-file#undo-limits
  (setq undo-limit 67108864) ; 64mb.
  (setq undo-strong-limit 100663296) ; 96mb.
  (setq undo-outer-limit 1006632960) ; 960mb.
  :config
  (bind-keys
   ("C-z" . undo-fu-only-undo)
   ("C-S-z" . undo-fu-only-redo)))

;; (use-package minimap
;;   :init
;;   (setq minimap-window-location 'right)
;;   (setq minimap-width-fraction 0.15)
;;   (setq minimap-update-delay 0)
;;   :config
;;   (minimap-mode))


;; (use-package dashboard
;;   :init
;;   (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
;;   :config
;;   (dashboard-setup-startup-hook))

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-odd-face-perc 50)
  (setq highlight-indent-guides-auto-even-face-perc 50)
  (setq highlight-indent-guides-auto-character-face-perc 80)
  :hook
  (prog-mode-hook . highlight-indent-guides-mode)
  )

;; (use-package line-reminder
;;   :init
;;   (setq line-reminder-show-option 'indicators)
;;   :config
;;   (global-line-reminder-mode t))

;; (use-package focus
;;   :config (focus-mode))


(use-package dimmer
  :init
  ;; (setq dimmer-adjustment-mode 'both)
  (setq dimmer-fraction 0.4)
  
  :config (dimmer-mode))


(windmove-default-keybindings '(meta))
(windmove-swap-states-default-keybindings '(meta shift))

(use-package golden-ratio
  :config
  (golden-ratio-mode))

(use-package buffer-name-relative
  :config
  (buffer-name-relative-mode)
  )

;; (use-package sideline
;;   :init
;;   (setq sideline-backends-left-skip-current-line t   ; don't display on current line (left)
;; 		sideline-backends-right-skip-current-line t  ; don't display on current line (right)
;; 		sideline-order-left 'down                    ; or 'up
;; 		sideline-order-right 'up                     ; or 'down
;; 		sideline-format-left "%s   "                 ; format for left aligment
;; 		sideline-format-right "   %s"                ; format for right aligment
;; 		sideline-priority 100                        ; overlays' priority
;; 		sideline-display-backend-name t)             ; display the backend name
;;   :hook (flycheck-mode . sideline-mode)
;;   )

;; (use-package sideline-flycheck :init (setq sideline-backends-right '(sideline-flycheck)) :hook (flycheck-mode . sideline-flycheck-setup))
;; (use-package sideline-lsp :init (setq sideline-backends-right '(sideline-lsp)))
;; (use-package sideline-blame :init (setq sideline-backends-left '((sideline-blame . up))))

(use-package avy
  :config
  (bind-keys
   ("M-/" .  avy-goto-char)
   )
  )


(use-package jump-char
  :config
  (bind-keys
   ("C-/" .  jump-char-forward)
   ("C-?" .  jump-char-backward)
   ))


;; https://github.com/minad/vertico vertical completion ui
(use-package vertico
  :init
  (vertico-mode))

;; https://github.com/minad/corfu completion in region
;; similar to company
(use-package corfu
  :config
  (global-corfu-mode))

;; https://github.com/oantolin/orderless
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :config

  (defun my/project-search ()
    "Search in project."
    (interactive)
    (consult-ripgrep nil (my/get-selected-text))
    )
  (defun my/file-search ()
    "Search in file."
    (interactive)
    (consult-line (my/get-selected-text))
    )
  (bind-keys
   ;; ([remap isearch-forward] . my/file-search)
   ([remap load-theme] . consult-theme)
   ([remap goto-line] . consult-goto-line)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap project-search] .  my/project-search)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap project-find-file] . consult-fd)
   ("C-S-v" . consult-yank-from-kill-ring)
   ;; ;; ("C-/" . consult-global-mark)
   ([remap imenu] . consult-imenu)
   ([remap Info-search] . consult-info)
   ))

(use-package rg)

(use-package which-key
  :config (which-key-mode))

(use-package ef-themes
  :config
  (load-theme 'ef-owl t)
  )

(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  ;; (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  ;; (load-theme 'doom-bluloco-dark t)
  )

;; (use-package solaire-mode
;;   :config
;; (solaire-global-mode +1))

;; (use-package  spacemacs-theme
;;   :config
;;   (load-theme 'spacemacs-dark)
;;   )


;; commenting due to some formatting issues
(use-package apheleia
  :config
  (apheleia-global-mode)
  ;; https://github.com/radian-software/apheleia/issues/30#issuecomment-778150037
  ;; apheleia by default only run formatter under current directory, this fix
  ;; makes it run the command under project root.
  (defun shou/fix-apheleia-project-dir (orig-fn &rest args)
    (let ((project (project-current)))
	  (if (not (null project))
		  (let ((default-directory (project-root project))) (apply orig-fn args))
        (apply orig-fn args))))
  (advice-add 'apheleia-format-buffer :around #'shou/fix-apheleia-project-dir)
  )

(use-package magit
  :bind
  ;; ("M-g" .  magit-file-dispatch)		
  ("M-G" .  magit-status)
  )

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  ;; (diff-hl-flydiff-mode)
  ;; (diff-hl-margin-mode)
  ;; (use-package highlight-indent-guides
  ;;   :config
  ;;   (setq   highlight-indent-guides-method 'character)
  ;;   (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

  :hook
  (
   magit-pre-refresh-hook diff-hl-magit-pre-refresh
   magit-post-refresh-hook diff-hl-magit-post-refresh
   ))

(use-package doom-modeline
  :config
  (doom-modeline-mode))


;; (straight-use-package
;;  '(awesome-tray :type git :host github :repo "manateelazycat/awesome-tray.git")
;;  )

;; (add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)

(use-package envrc
  :config
  (envrc-global-mode))

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

;; (use-package gdscript-mode
;;   :straight (gdscript-mode
;;       :type git
;;       :host github
;;       :repo "godotengine/emacs-gdscript-mode"))

(use-package gdscript-mode)

(use-package typescript-mode)

(use-package lsp-mode
  :preface
  ;; (defun lsp-booster--advice-json-parse (old-fn &rest args)
  ;;   "Try to parse bytecode instead of json."
  ;;   (or
  ;;    (when (equal (following-char) ?#)
  ;;      (let ((bytecode (read (current-buffer))))
  ;;        (when (byte-code-function-p bytecode)
  ;;          (funcall bytecode))))
  ;;    (apply old-fn args)))
  ;; (advice-add (if (progn (require 'json)
  ;;                        (fboundp 'json-parse-buffer))
  ;;                 'json-parse-buffer
  ;;               'json-read)
  ;;             :around
  ;;             #'lsp-booster--advice-json-parse)

  ;; (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  ;;   "Prepend emacs-lsp-booster command to lsp CMD."
  ;;   (let ((orig-result (funcall old-fn cmd test?)))
  ;;     (if (and (not test?)                             ;; for check lsp-server-present?
  ;;              (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
  ;;              lsp-use-plists
  ;;              (not (functionp 'json-rpc-connection))  ;; native json-rpc
  ;;              (executable-find "emacs-lsp-booster"))
  ;;         (progn
  ;;           (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
  ;;             (setcar orig-result command-from-exec-path))
  ;;           (message "Using emacs-lsp-booster for %s!" orig-result)
  ;;           (cons "emacs-lsp-booster" orig-result))
  ;;       orig-result)))
  :init
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-keymap-prefix "C-c l")
  ;; (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  :custom
  (lsp-inlay-hint-enable nil)
  (lsp-javascript-display-enum-member-value-hints t)
  (lsp-javascript-display-parameter-name-hints "all")
  (lsp-javascript-display-parameter-name-hints-when-argument-matches-name t)
  (lsp-javascript-display-parameter-type-hints t)
  (lsp-javascript-display-property-declaration-type-hints t)
  (lsp-javascript-display-return-type-hints t)
  (lsp-javascript-display-variable-type-hints t)
  (lsp-ui-doc-show-with-cursor nil)
  ;;(lsp-ui-sideline-show-code-actions nil)
  (lsp-completion-enable t)
  :hook (
         ((;; js modes
		   typescript-mode
		   ;; emacs-lisp-mode
		   js-ts-mode tsx-ts-mode typescript-ts-mode
		   ;; config files modes
		   json-ts-mode yaml-ts-mode
		   ;; css modes
		   scss-mode css-ts-mode
		   ;; godot
		   gdscript-ts-mode gdscript-mode
		   ;; others
		   bash-ts-mode
		   gfm-mode markdown-mode
		   dockerfile-ts-mode terraform-mode
		   lua-mode python-ts-mode nix-mode) . lsp-deferred)
		 ;; (lsp-mode . sideline-mode)
         (lsp-mode . lsp-enable-which-key-integration)
		 )
  :commands (lsp lsp-deferred)
  )

(use-package lsp-ui
  ;; :init (setq lsp-ui-sideline-enable nil)
  :commands lsp-ui-mode
  )

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (bind-keys ([remap next-error] . flycheck-next-error))
  (bind-keys ([remap previous-error] . flycheck-previous-error)))

(use-package consult-flycheck
  :config
  (bind-keys ([remap consult-flymake] . consult-flycheck))
  )

(use-package embark-consult)

(use-package embark
  :bind
  (("C-," . embark-act)         ;; pick some comfortable binding
   ;;   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;; (use-package app-launcher
;;   :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))

;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :if (display-graphic-p))


;; (use-package dirvish
;;   :init
;;   (setq dirvish-attributes
;; 		(append
;; 		 ;; The order of these attributes is insignificant, they are always
;; 		 ;; displayed in the same position.
;; 		 '(vc-state subtree-state nerd-icons collapse)
;; 		 ;; Other attributes are displayed in the order they appear in this list.
;; 		 '(git-msg file-modes file-time file-size)))
;;   (setq dired-listing-switches
;;         "-l --almost-all --human-readable --group-directories-first --no-group")
;;   ;; this command is useful when you want to close the window of `dirvish-side'
;;   ;; automatically when opening a file
;;   (put 'dired-find-alternate-file 'disabled nil)
;;   :config
;;   (dirvish-override-dired-mode)
;;   (dirvish-side-follow-mode)
;;   :bind
;;   (
;;    ("C-S-t" . dirvish-side)
;;    :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
;;    (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
;;    ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
;;    ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
;;    ("f"   . dirvish-file-info-menu)    ; [f]ile info
;;    ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
;;    ("s"   . dirvish-quicksort)         ; [s]ort flie list
;;    ("r"   . dirvish-history-jump)      ; [r]ecent visited
;;    ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
;;    ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
;;    ("*"   . dirvish-mark-menu)
;;    ("y"   . dirvish-yank-menu)
;;    ("N"   . dirvish-narrow)
;;    ("^"   . dirvish-history-last)
;;    ("TAB" . dirvish-subtree-toggle)
;;    ("M-f" . dirvish-history-go-forward)
;;    ("M-b" . dirvish-history-go-backward)
;;    ("M-e" . dirvish-emerge-menu)
;;    )
;;   )

(use-package all-the-icons-dired
  :hook
  (dired-mode-hook . all-the-icons-dired-mode)
  )

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package perfect-margin
  :init
  (setq perfect-margin-only-set-left-margin t)
  :config (perfect-margin-mode 1))


;; new tab for each project:
;; (use-package otpp
;;   :straight t
;;   :after project
;;   :init
;;   ;; Enable `otpp-mode` globally
;;   (otpp-mode 1)
;;   ;; If you want to advice the commands in `otpp-override-commands`
;;   ;; to be run in the current's tab (so, current project's) root directory
;;   (otpp-override-mode

;; (set-frame-parameter nil 'alpha-background 80)
;; (add-to-list 'default-frame-alist '(alpha-background . 80))

(use-package kanata-kbd-mode
  :straight '(kanata-kbd-mode :host github :repo "chmouel/kanata-kbd-mode"))
