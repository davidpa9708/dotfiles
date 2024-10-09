

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
 )

(cua-mode) ;; C-x to cut on selection https://www.emacswiki.org/emacs/CuaMode
;; (global-display-line-numbers-mode) ;; display line numbers
(electric-pair-mode)
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


(bind-keys
 ("C--" . my/text-scale-decrease)
 ("C-=" . my/text-scale-increase)
 ("<f1>" . execute-extended-command)
 ("<f6>" . load-theme)
 ("C-<SPC>" . completion-at-point)
 ("C-f" . isearch-forward)
 ("C-S-f" . project-search)
 ("C-s" . save-buffer)
 ("C-S-z" . undo-redo)
 ("<mouse-3>" . context-menu-open)
 ("S-<down-mouse-1>" . mouse-set-mark)
 ("C-b" . switch-to-buffer)
 ("C-p" . project-find-file)
 ("C-S-p" . project-switch-project)
 ("C-o" . find-file)
 ("C-i" . imenu)
 ;; ("C-l" . nil) ; lsp-prefix
 ("<next>" . my/next)
 ("<prior>" . my/prior)
 )

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

  ;; https://stackoverflow.com/a/51445691
  (defun my/get-selected-text ()
    (interactive)
    (if (use-region-p)
	(let ((regionp (buffer-substring (region-beginning) (region-end))))
	  (deactivate-mark)
          regionp)))
  
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
  (bind-keys ([remap isearch-forward] . my/file-search)
	     ([remap load-theme] . consult-theme)
	     ([remap goto-line] . consult-goto-line)
	     ([remap switch-to-buffer] . consult-buffer)
	     ([remap project-search] .  my/project-search)
	     ([remap project-switch-to-buffer] . consult-project-buffer)
	     ([remap project-find-file] . consult-fd)
	     ("C-S-v" . consult-yank-from-kill-ring)
	     ("C-/" . consult-global-mark)
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

(use-package magit)

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

(use-package dirvish
  :config
  (dirvish-override-dired-mode))

(use-package envrc
  :config
  (envrc-global-mode))

;; (straight-use-package 'tree-sitter)
;; (straight-use-package 'tree-sitter-langs)

;; (use-package gdscript-mode
;;   :straight (gdscript-mode
;; 	     :type git
;; 	     :host github
;; 	     :repo "godotengine/emacs-gdscript-mode"))

(use-package gdscript-mode)

(use-package typescript-mode)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c C-l")
  :config
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-sideline-show-code-actions t)
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
  :commands lsp
  :bind
  (("C-." .    lsp-execute-code-action)
   ))

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
   ("C-;" . embark-dwim)        ;; good alternative: M-.
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
