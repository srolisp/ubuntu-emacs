(defun xr-org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name (concat default-directory "settings.org")))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)
      ;; FIX: remove tangling for reamde.org repeatedly
      (copy-file (buffer-name) (concat user-emacs-directory "readme.org") t))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'xr-org-babel-tangle-config)))

(setq inhibit-startup-message t)

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)

  (column-number-mode)
  (display-battery-mode 1)
  (global-display-line-numbers-mode t)
  ;; TODO: add for pdf something mode. can't remember that mode.
  (dolist (mode '(org-mode-hook
		  term-mode-hook
		  shell-mode-hook
		  treemacs-mode-hook
		  eshell-mode-hook
		  vterm-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  (set-background-color "#002b00")
  (set-foreground-color "#ffe97a")

  (set-face-attribute 'default nil 
		      :family "fixed"
		      :height 98)

(set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
(set-face-font 'default ":antialias=false")

  (when window-system
    (set-frame-parameter (selected-frame) 'alpha (list 80 80))
    (add-to-list 'default-frame-alist `(alpha . 80)))

  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory ".saves"))))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ivy
  :bind (("C-s" . swiper))
  :init
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ;; ("C-x b" . counsel-ibuffer)
	 ("C-x C-b" . counsel-switch-buffer)
	 ;; :map minibuffer-local-map
	 ("C-c r" . counsel-minibuffer-history)
	 )
  :config
  (counsel-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package vterm
  :ensure t)

(use-package magit
  :bind (("C-c m g" . magit-status)))
