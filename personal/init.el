;;; init.el --- Personal config for general Emacs usage

;; XTerm mouse mode enables mouse commands within the terminal,
;; e.g. click to set cursor at mouse point, etc.
(xterm-mouse-mode 1)

;; Prompt to quit before closing Emacs.
(setq kill-emacs-query-functions
      (cons (lambda ()
              (yes-or-no-p "Exit Emacs?"))
            kill-emacs-query-functions))

;; Fuck off guru mode!
(defun disable-guru-mode ()
  (guru-mode -1))
(add-hook 'prelude-prog-mode-hook 'disable-guru-mode t)

;; Set a prettier line highlight.
(global-hl-line-mode 1)
(set-face-background 'hl-line "#2f2f2f")
(set-face-foreground 'highlight nil)

;; Start an emacs server, if one is not already running.
(require 'server)
(or (server-running-p)
    (server-start))
