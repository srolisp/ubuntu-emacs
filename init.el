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

(set-background-color "#002b00")
(set-foreground-color "#ffe97a")

(set-face-attribute 'default nil 
		    :family "fixed"
		    :height 98)

(when window-system
  (set-frame-parameter (selected-frame) 'alpha (list 80 80))
  (add-to-list 'default-frame-alist `(alpha . 80)))
