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
(display-time-mode 1)

(global-display-line-numbers-mode t)
;; TODO: add for pdf something mode. can't remember that mode.
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-to-list 'default-frame-alist '(foreground-color . "#ffe97a"))
(add-to-list 'default-frame-alist '(background-color . "#002b00"))
(add-to-list 'default-frame-alist   '(cursor-color . "red3"))


;; 다음에 써보자. ttf 아님 "-xos4-terminus-medium-r-normal--12-140-*-*-*-*-*-*"
(set-face-attribute 'default nil 
		    :family "fixed"
		    :height 98)


(set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
(set-face-font 'default ":antialias=false")

(when window-system
  (set-frame-parameter (selected-frame) 'alpha (list 80 80))
  (add-to-list 'default-frame-alist '(alpha . 80)))

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
  :ensure t
  :config
  ;; (setq vterm-term-environment-variable "eterm-color")
  (set-face-attribute 'vterm-color-blue nil :foreground "skyblue"))

(use-package magit
  :bind (("C-c m g" . magit-status)))

(defun wm-xmodmap()
  (interactive)
  (call-process "xmodmap" nil (get-buffer-create "wm") nil
		(expand-file-name "~/.emacs.d/exwm/xmodmap_original")))
(add-to-list 'after-init-hook 'wm-xmodmap)
;; (Wm-xmodmap)
(use-package exwm
  :config
  (require 'exwm)
  (require 'exwm-config)
  ;; (exwm-config-default)
  ;;   ;;     ;; (require 'exwm-randr)
  ;;   ;;     ;; (exwm-randr-enable)
  ;;   ;;     ;; These keys should always pass through to Emacs
  (setq exwm-input-global-keys
	`(([?\s-r] . exwm-reset)
	  ([?\s-w] . exwm-workspace-switch)
	  ,@(mapcar (lambda (i)
		      `(,(kbd (format "s-%d" i)) .
			(lambda ()
			  (interactive)
			  (exwm-workspace-switch-create ,i))))
		    (number-sequence 0 9))
	  ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))
	  (,(kbd "<f8>") . toggle-korean-input-method)
	  (,(kbd "<XF86AudioLowerVolume>") . (lambda () (interactive) (shell-command "amixer -D pulse -q sset Master 5%-")))
	  (,(kbd "<XF86AudioRaiseVolume>") . (lambda () (interactive) (shell-command "amixer -D pulse -q sset Master 5%+")))
	  (,(kbd "<XF86AudioMute>") . (lambda () (interactive) (shell-command "amixer -D pulse -q sset Master 1+ toggle")))))
  ;;   (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)
  ;; (define-key exwm-mode-map (kbd "S-SPC")  #'toggle-korean-input-method)

  ;;   (setq exwm-input-prefix-keys
  ;; 	'(?\C-x
  ;; 	  ?\C-u
  ;; 	  ?\C-h
  ;; 	  ?\M-x
  ;; 	  ?\M-`
  ;; 	  ?\M-&
  ;; 	  ?\M-:
  ;; 	  ?\C-\M-j  ;; Buffer list
  ;; 	  ?\C-\ ))
  ;; toggle-korean-input-method
  (setq exwm-input-simulation-keys
	'(
	  ;; movement
	  ([?\C-b] . [left])
	  ([?\M-b] . [C-left])
	  ([?\C-f] . [right])
	  ([?\M-f] . [C-right])
	  ([?\C-p] . [up])
	  ([?\C-n] . [down])
	  ([?\C-a] . [home])
	  ([?\C-e] . [end])
	  ([?\M-v] . [prior])
	  ([?\C-v] . [next])
	  ([?\C-d] . [delete])
	  ([?\C-k] . [S-end delete])
	  ;; cut/paste.
	  ;; ([?\C-w] . [?\C-x])
	  ([?\M-w] . [?\C-c])
	  ([?\C-y] . [?\C-v])
	  ;; search
	  ([?\C-s] . [?\C-f])
	  ;; ([?\C-b] . [left])
	  ))
  ;;   (setq exwm-workspace-number 4)
  ;;   (require 'exwm-randr)
  ;;   (setq exwm-randr-workspace-output-plist '(0 "VGA1"))
  ;;   (add-hook 'exwm-randr-screen-change-hook
  ;; 	    (lambda ()
  ;; 	      (start-process-shell-command
  ;; 	       "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
  ;;   (exwm-randr-enable)
  ;;   (defun exwm-change-screen-hook ()
  ;;     (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
  ;; 	  default-output)
  ;;       (with-temp-buffer
  ;; 	(call-process "xrandr" nil t nil)
  ;; 	(goto-char (point-min))
  ;; 	(re-search-forward xrandr-output-regexp nil 'noerror)
  ;; 	(setq default-output (match-string 1))
  ;; 	(forward-line)
  ;; 	(if (not (re-search-forward xrandr-output-regexp nil 'noerror))
  ;; 	    (call-process "xrandr" nil nil nil "--output" default-output "--auto")
  ;; 	  (call-process
  ;; 	   "xrandr" nil nil nil
  ;; 	   "--output" (match-string 1) "--primary" "--auto"
  ;; 	   "--output" default-output "--off")
  ;; 	  (setq exwm-randr-workspace-output-plist (list 0 (match-string 1)))))))

  ;;   (setq exwm-workspace-number 4)
  (exwm-enable))

(use-package exwm-systemtray
:ensure nil
:config
(setq exwm-systemtray-height 24)
(exwm-systemtray-enable))

(setq org-hide-leading-stars t)

(use-package eterm-256color
:ensure t)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))
