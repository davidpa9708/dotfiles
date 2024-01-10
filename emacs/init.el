(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setopt
 frame-resize-pixelwise t
 inhibit-startup-screen t
 ring-bell-function nil
 custom-file "~/.emacs.d/emacs-custom.el"
 create-lockfiles nil
 make-backup-files nil
 use-short-answers t
 auto-save-default nil)

(global-display-line-numbers-mode)
(electric-pair-mode)
(recentf-mode)

(cua-mode)

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :config
  (bind-keys ([remap isearch-forward] . consult-line)
	     ([remap load-theme] . consult-theme)
	     ([remap switch-to-buffer] . consult-buffer)))

(use-package corfu
  :ensure t
  :config
  (global-corfu-mode)
  )

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package magit
  :ensure t)

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-elea-dark t))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode))

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
 ("<f6>" . load-theme)
 ("C-<SPC>" . completion-at-point)
 ("C-f" . isearch-forward)
 ("C-s" . save-buffer)
 ("C-S-z" . undo-redo))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode))
