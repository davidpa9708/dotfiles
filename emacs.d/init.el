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
 )

;; (cua-mode) ;; C-x to cut on selection https://www.emacswiki.org/emacs/CuaMode
;; (transient-mark-mode nil)
(global-display-line-numbers-mode) ;; display line numbers
;; (electric-pair-mode)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; (recentf-mode)

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
  (interactive)
  (forward-line (/ (window-total-height) 2))
  (recenter)
  )

(defun my/prior ()
  "Scroll up and recenter."
  (interactive)
  (forward-line (- (/ (window-total-height) 2)))
  (recenter)
  )

(defun my/next-copy ()
  "Scroll down and recenter."
  (interactive)
  (if (not (region-active-p))
      (call-interactively 'push-mark-command))
  (forward-line (/ (window-total-height) 2))
  (recenter)
  )

(defun my/prior-copy ()
  "Scroll up and recenter."
  (interactive)
  (if (not (region-active-p))
      (call-interactively 'push-mark-command))
  (forward-line (- (/ (window-total-height) 2)))
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
  "go to other window and go to line"
  (interactive "p")
  (call-interactively 'other-window)
  (forward-line (-  (my/get-selected-text) (string-to-number (format-mode-line "%l"))))
  )

(defun my/view-buffer-other-frame (arg)
  "go to other window and go to line"
  (interactive "p")
  (view-buffer-other-frame (current-buffer))
  )


(bind-keys
 ("C--" . my/text-scale-decrease)
 ("C-=" . my/text-scale-increase)
 ("<f1>" . execute-extended-command)
 ("<f6>" . load-theme)
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
 ;; ("C-o" . find-file)
 ("C-i" . imenu)
 ;; ("C-l" . nil) ; lsp-prefix
 ("<escape>" . keyboard-escape-quit)
 ("<next>" . my/next)
 ("<prior>" . my/prior)
 ;; ("S-<next>" . my/next-copy)
 ;; ("S-<prior>" . my/prior-copy)
 ("<home>" . back-to-indentation) ;; https://stackoverflow.com/a/12346740
 ;; ("<end>" . move-end-of-line)
 ("TAB" . self-insert-command)
 ;; ("M-?" . goto-line)
 ;; ("C-<escape>" . keyboard-quit)
 ;; ("C-o" . other-window)
 ;; ("C-:" . my/view-buffer-other-frame)
 ;; ("C-1" . delete-other-windows)
 ;; ("C-2" . split-window-below)
 ;; ("C-3" . split-window-right)
 ;; ("C-0" . delete-window)
 ;; ("C-;" . query-replace-regexp)
 ;; ("C-S-SPC" . exchange-point-and-mark)
 ("C-|" . my/goto-match-paren)
 ("C-\"" . my/goto-match-paren)
 ;; ("C-SPC" . execute-extended-command)
 )


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
   ;; b			 ("C-/" . consult-global-mark)
   ("C-e" . consult-flymake)
   ([remap imenu] . consult-imenu)
   ([remap Info-search] . consult-info)
   ))



(use-package which-key
  :config (which-key-mode))

(use-package ef-themes
  :config
  (load-theme 'ef-owl t))

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
  ("M-g" .  magit-file-dispatch)
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

;; (use-package doom-modeline
;;   :config
;; (doom-modeline-mode))

;; (straight-use-package
;;  '(awesome-tray :type git :host github :repo "manateelazycat/awesome-tray.git")
;;  )

;; (use-package dirvish
;;   :config
;;   (dirvish-override-dired-mode))

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
  :init
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-inlay-hint-enable nil)
  (lsp-javascript-display-enum-member-value-hints t)
  (lsp-javascript-display-parameter-name-hints "all")
  (lsp-javascript-display-parameter-name-hints-when-argument-matches-name t)
  (lsp-javascript-display-parameter-type-hints t)
  (lsp-javascript-display-property-declaration-type-hints t)
  (lsp-javascript-display-return-type-hints t)
  (lsp-javascript-display-variable-type-hints t)
  ( lsp-ui-doc-show-with-cursor t)
  ( lsp-ui-sideline-show-code-actions nil)
  :config
  (lsp-completion-enable)
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
         (lsp-mode . lsp-enable-which-key-integration))
  )

(use-package lsp-ui :commands lsp-ui-mode)

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

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
