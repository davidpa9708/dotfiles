;; (package-initialize)

;; (add-to-list 'package-archives rs'("melpa" . "https://melpa.org/packages/"))

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

(setopt
 frame-resize-pixelwise t
 ;;  inhibit-startup-screen t
 ring-bell-function nil
 custom-file "~/.emacs.d/emacs-custom.el"
 create-lockfiles nil
 make-backup-files nil
 use-short-answers t
 auto-save-default nil)
;; (global-display-line-numbers-mode)
;; (electric-pair-mode)
;; (recentf-mode)

(cua-mode)

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


;; https://stackoverflow.com/a/51445691
(defun my/get-selected-text ()
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring (region-beginning) (region-end))))
        (message regionp))))

(defun my/project-search ()
  "Search in project."
  (interactive)
  (consult-grep nil (my/get-selected-text))
  )

(defun my/file-search ()
  "Search in file."
  (interactive)
  (consult-line (my/get-selected-text))
  )

(bind-keys
 ("C--" . my/text-scale-decrease)
 ("C-=" . my/text-scale-increase)
 ("<f1>" . execute-extended-command)
 ("<f6>" . load-theme)
 ("C-<SPC>" . completion-at-point)
 ("C-f" . isearch-forward)
 ("C-S-f" .  project-search)
 ("C-s" . save-buffer)
 ("C-S-z" . undo-redo)
 ("<mouse-3>" . context-menu-open)
 ("S-<down-mouse-1>" . mouse-set-mark)
 ("C-/" . consult-global-mark)
 ("C-b" . consult-buffer)
 ("C-p" . project-find-file)
 ("C-l" . nil))

(require 'use-package-ensure)

(use-package vertico
  :init
  (vertico-mode))

(use-package corfu
  :config
  (global-corfu-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :config
  (bind-keys ([remap isearch-forward] . my/file-search)
	     ([remap load-theme] . consult-theme)
	     ([remap goto-line] . consult-goto-line)
	     ([remap switch-to-buffer] . consult-buffer)
	     ([remap project-search] .  my/project-search)))

(use-package which-key
  :config (which-key-mode))

(use-package ef-themes
  :config
  (load-theme 'ef-elea-dark t))

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


;; (use-package git)

(use-package magit)

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

(use-package gdscript-mode
  :straight (gdscript-mode
	     :type git
	     :host github
	     :repo "godotengine/emacs-gdscript-mode"))


(use-package typescript-mode)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :hook (
	 ((;; js modes
	   typescript-mode
	   ;;emacs-lisp-mode
	   js-ts-mode tsx-ts-mode typescript-ts-mode
	   ;; config files modes
	   json-ts-mode yaml-ts-mode
	   ;; css modes
	   scss-mode css-ts-mode
	   ;; godot
	   gdscript-ts-mode
	   ;; others
	   bash-ts-mode
	   gfm-mode markdown-mode
	   dockerfile-ts-mode terraform-mode
	   lua-mode python-ts-mode nix-mode). lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
