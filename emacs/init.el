;; (package-initialize)

;; (add-to-list 'package-archives rs'("melpa" . "https://melpa.org/packages/"))

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

(bind-keys
 ("C--" . my/text-scale-decrease)
 ("C-=" . my/text-scale-increase)
 ("<f1>" . execute-extended-command)
 ("<f6>" . load-theme)
 ("C-<SPC>" . completion-at-point)
 ("C-f" . isearch-forward)
 ("C-S-f" . project-or-external-find-regexp)
 ("C-s" . save-buffer)
 ("C-S-z" . undo-redo)
 ("<mouse-3>" . nil)
 ("S-<down-mouse-1>" . mouse-set-mark))

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
  (bind-keys ([remap isearch-forward] . consult-line)
	     ([remap load-theme] . consult-theme)
	     ([remap goto-line] . consult-goto-line)
	     ([remap switch-to-buffer] . consult-buffer)))

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

;; (use-package tree-sitter-langs
;;   :ensure t)

(use-package lsp-mode
  :config
  (lsp-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package typescript-mode)
