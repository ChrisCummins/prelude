(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dockerfile-mode super-save hl-todo zop-to-char zenburn-theme which-key volatile-highlights undo-tree smartrep smartparens smart-mode-line projectile ov operate-on-number move-text magit imenu-anywhere guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region exec-path-from-shell editorconfig easy-kill discover-my-major diminish diff-hl crux browse-kill-ring beacon anzu ace-window)))

;; Set the font.
(set-face-attribute 'default nil :height 125)
(add-to-list 'default-frame-alist '(font . "InconsolataGo-15"))

;; Set up cursor.
(blink-cursor-mode 1)
(setq-default cursor-type '(bar . 4))
(set-cursor-color "#f00")

;; Keyboard shortcut.
(global-set-key (kbd "C-c c") 'recompile)
(global-set-key (kbd "C-c C") 'compile)

(global-set-key (kbd "C-<return>") 'recompile)
(global-set-key (kbd "S-<return>") 'compile)


(add-to-list 'load-path "~/.emacs.d/personal/")
(require 'protobuf-mode)
