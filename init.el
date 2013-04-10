;; init.el --- Emacs configuration file
;;
;; Copyright (C) 2012 Christopher E. Cummins
;;
;; Author: Christopher. E. Cummins <chrisc.101@gmail.com>
;; Created: 28 October 2012
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;    Emacs configuration file.
;;
;;; Issues and to-do list:
;;
;;    See the GitHub issue tracker for this repository, available at:
;;    <https://github.com/ChrisCummins/.emacs/issues>
;;
;;; Code:

;; Directories and files:
(setq root-dir      (concat (getenv "HOME") "/emacs/"))
(setq site-lisp-dir (concat root-dir "site-lisp/"))
(setq themes-dir    (concat root-dir "themes/"))

;;; Bootstrap.
;;  Put early-boot proceedures here.
;;; ==========================================================================

;; Assemble our load-path.
(dolist (d (directory-files site-lisp-dir))
  (add-to-list 'load-path (concat site-lisp-dir d)))

;; Assemble our theme load-path.
(dolist (d (directory-files themes-dir))
  (add-to-list 'custom-theme-load-path (concat themes-dir d)))

;; Hide the visual clutter.
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Load a color theme.
(load-theme 'ir-black t)

;;; General Settings.
;;; ==========================================================================

;; Load system packages.
(require 'comint)
(require 'flymake)
(require 'linum)
(require 'package)
(require 'server)
(require 'tabify)

;; Emacs Customize.
(setq custom-file (concat root-dir "custom.el"))
(load-file custom-file)

;; Emacs package manager.
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Inhibit the startup buffer.
(setq inhibit-startup-message t)

;; Inhibits display of buffer list when more than 2 files are loaded.
(setq inhibit-startup-buffer-menu t)

;; Add Emacs close confirmation.
(setq kill-emacs-query-functions
      (cons (lambda ()
              (yes-or-no-p "Close Emacs?"))
            kill-emacs-query-functions))

;; Don't prompt to kill active processes on close.
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; Case insensitive list sorting.
(setq sort-fold-case t)

;; Use abbreviations.
(read-abbrev-file (concat root-dir "abbrevs"))
(setq-default abbrev-mode t)
(setq save-abbrevs t)

;; Set the follow to non-nil to Isearch in file names only.
;;
;; If t, Isearch in Dired always matches only file names.
;; If `dwim', Isearch matches file names when initial point position is on a
;; file name.
;; Otherwise, it searches the whole buffer without restrictions.
(setq dired-isearch-filenames t)

;; Allow wdired to edit file permissions.
(setq wdired-allow-to-change-permissions t)

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "f" 'dired-find-file)))

;; Define a key-binding for convenient gdb'ing.
(global-set-key (kbd "C-c d") 'run-gdb)

;; Define key-bindings for window control.
(global-set-key [f1] 'delete-other-windows)
(global-set-key [f2] 'split-window-below)
(global-set-key [f3] 'split-window-right)
(global-set-key [f4] 'delete-window)

;; Define key-bindings for macros.
(global-set-key [f5] 'kmacro-start-macro)
(global-set-key [f6] 'kmacro-end-macro)
(global-set-key [f7] 'kmacro-)

;;; Appearance, text and input.
;;; ==========================================================================

;; Setting this to non-nil means mouse commands use dialog boxes to ask
;; questions. File selection dialogs are also enabled if this is non-nil.
(setq use-dialog-box nil)

;; Set the default font.
(set-default-font "Inconsolata-11")
(set-face-bold-p 'bold nil)

;; Toggle visualization of matching parens (Show Paren mode).  With a prefix
;; argument ARG, enable Show Paren mode if ARG is positive, and disable it
;; otherwise.  If called from Lisp, enable the mode if ARG is omitted or nil.
(show-paren-mode 1)

;; Toggle highlighting changes in this buffer (Highlight Changes mode).  With a
;; prefix argument ARG, enable Highlight Changes mode if ARG is positive, and
;; disable it otherwise.  If called from Lisp, enable the mode if ARG is omitted
;; or nil.
(highlight-changes-mode t)

;; Toggle Font-Lock mode in all buffers.  With prefix ARG, enable
;; Global-Font-Lock mode if ARG is positive; otherwise, disable it.  If called
;; from Lisp, enable the mode if ARG is omitted or nil.
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

;; "y"/"n" prompt instead of "yes"/"no".
(defalias 'yes-or-no-p 'y-or-n-p)

;; Cursor to use when this buffer is in the selected window.
(set-default 'cursor-type 'bar)

;; Set a cursor color.
(setq default-frame-alist
      '((cursor-color . "red")))

;; Toggle XTerm mouse mode.
(xterm-mouse-mode 1)

;; Toggle Linum mode in all buffers. Enable Global-Linum mode if ARG is positive
;; otherwise, disable it.
(global-linum-mode -1)

;;; Files, buffers and windows.
;;; ==========================================================================

(defun system-move-file-to-trash (filename)
  ;; Play nicely with the system recycling bin, requires trash-cli.
  (shell-command (concat "trash-put " (shell-quote-argument filename))))

;; Specifies whether to use the system's trash can.
(setq delete-by-moving-to-trash t)

;; Always remove trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Global Auto Revert mode is a global minor mode that reverts any buffer
;; associated with a file when the file changes on disk.
(global-auto-revert-mode t)

;; Revert a buffer through a key binding.
(global-set-key (kbd "M-R") 'revert-buffer)

;; Non-nil means make a backup of a file the first time it is saved.
(setq make-backup-files nil)

;; Non-nil says by default do auto-saving of every file-visiting buffer.
(setq auto-save-default nil)

;; Toggle Auto Compression mode. When enabled, compressed files are
;; automatically uncompressed for reading, and compressed when writing.
(auto-compression-mode 1)

;; Define a key binding for quick grep'ing.
(global-set-key (kbd "C-c g") 'grep)

;; Define a key binding for quick git-grep'ing.
(global-set-key (kbd "M-/") 'git-grep)

;; Enables/disables Flymake GUI warnings.
(setq flymake-gui-warnings-enabled nil)

;; Load flymake on file open.
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Define a key binding for quick grep'ing.
(global-set-key (kbd "C-c C") 'compile)
(global-set-key (kbd "C-c c") 'quick-compile)

;; Scroll the output of compilation windows.
(setq compilation-scroll-output t)

;; Automatically save changes before compiling.
(setq compilation-ask-about-save nil)

;; Register Description of a Project (.doap) files for xml-mode.
(setq auto-mode-alist
      (append '(("\\.doap$" . xml-mode)) auto-mode-alist))

;; Automatically set mail mode for mutt files.
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;; Associate zsh files with sh-mode.
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme\\'" . sh-mode))

;; Set text-mode for normal plaintext files.
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("INSTALL" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("LICENSE" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("COPYING" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt$" . text-mode) auto-mode-alist))

;; PHP files load in html-mode (why not?).
(setq auto-mode-alist (cons '("\\.php$" . html-mode) auto-mode-alist))

(defvar c++-source-extension-list '("c" "cc" "C" "cpp" "c++"))
(defvar c++-header-extension-list '("h" "hh" "H" "hpp"))
(defvar c++-default-header-ext "h")
(defvar c++-default-source-ext "cpp")
(defvar c++-header-ext-regexp "\\.\\(hpp\\|h\\|\hh\\|H\\)$")
(defvar c++-source-ext-regexp "\\.\\(cpp\\|c\\|\cc\\|C\\)$")

;; Override switch-to-buffer with ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; Configure ido-mode.
(ido-mode 1)
(setq ido-eveywhere t)
(setq ido-case-fold nil)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)

;; The number of lines to try scrolling a window by when point moves out.
;; If that fails to bring point back on frame, point is centered instead.
;; If this is zero, point is always centered after it moves off frame.
(setq scroll-step 1)

;; Use wind move.
(when (fboundp 'windmove-default-keybindings)
  (global-set-key (kbd "<select>") 'windmove-up)
  (windmove-default-keybindings))

;; Define a key to jump to line.
(global-set-key (kbd "C-l") 'goto-line)

;; List of directories to search for Info documentation files.
(setq Info-directory-list Info-default-directory-list)

;; Replace the default (set-fill-column) assignment for C-x f with the
;; find-file-at-pointer function.
(global-set-key (kbd "C-x f") 'ffap)

;; Flip the default bindings of search and regexp search so that we default to
;; making regexp searches.
(global-set-key (kbd "C-s")   'isearch-forward-regexp)
(global-set-key (kbd "C-r")   'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-s") 'isearch-backward)

;; Turn off visual line wrap.
(setq default-truncate-lines 1)

;; Wrap lines to 80 characters.
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 80)

;; Define a key binding for quickly formatting paragraphs.
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Define a key for quickly killing lines.
(global-set-key (kbd "C-c k") 'kill-start-of-line)

;; Use up and down keys in shells for command history
(define-key comint-mode-map (kbd "M-") 'comint-next-input)
(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

;; Stop ^M's from displaying in system shell window.
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;; Def
(global-set-key (kbd "C-c u") 'untabify)

;; Define key bindings for quickly starting shells.
(global-set-key (kbd "C-c a") 'ansi-term)
(global-set-key (kbd "C-c s") 'shell)

;;; C-Indentation styles.
;;; ==========================================================================

(defconst gnu-no-tabs
  '("gnu"
    (c-tab-always-indent . nil)
    (indent-tabs-mode . nil)
    (pedant+-mode . 1))
  "GNU style without tabs")

(c-add-style "gnu-no-tabs" gnu-no-tabs)

;;; Mode Hooks.
;;  An alphabetically arranged list of hooks.
;;; ==========================================================================

;; Highlight FIXME: | TODO: | BUG: tags.
(defun enable-todo-highlighting ()
  (progn
    (font-lock-add-keywords
     nil '(("\\<\\(\\(FIXME\\|TODO\\|BUG\\):\\)" 1 font-lock-warning-face t)))))

(defun programming-common-hook ()
  (interactive)
  (progn
    (auto-fill-mode -1)
    (enable-todo-highlighting)
    (idle-highlight-mode t)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (programming-common-hook)
            (c-set-style "linux")
            (c-toggle-auto-state 1)
            (c-toggle-hungry-state 1)
            (electric-indent-mode 1)
            (electric-layout-mode 1)
            (electric-pair-mode 1)
            (setq default-truncate-lines 1)
            (setq indent-tabs-mode 1)
            (subword-mode 1)
            (which-func-mode)))

(add-hook 'calendar-load-hook
          (lambda ()
            (setq mark-holidays-in-calendar t)
            (define-key calendar-mode-map (kbd "<") 'scroll-calendar-left)
            (define-key calendar-mode-map (kbd ">") 'scroll-calendar-right)
            (define-key calendar-mode-map (kbd "C-x <") 'scroll-calendar-left)
            (define-key calendar-mode-map (kbd "C-x >") 'scroll-calendar-right)))

(add-hook 'compilation-mode-hook
          (lambda ()
            ;; TODO: Auto-tail new buffer.
            (passive-text-mode)))

(add-hook 'compilation-start-hook
          (lambda (process)
            ;; Don't prompt for running process on quit.
            (set-process-query-on-exit-flag process nil)) nil t)

(add-hook 'completion-list-mode-hook
          (lambda ()
            (passive-text-mode)))

(add-hook 'css-mode-common-hook
          (lambda ()
            (programming-common-hook)
            ;; Define the normal key binding for comment region.
            (global-set-key (kbd "C-c C-c") 'comment-region)
            ;; Indent by 2 spaces.
            (setq indent-tabs-mode nil)
            (setq-default tab-width 2)
            (setq indent-line-function 'insert-tab)))

(add-hook 'diff-mode-hook
          (lambda ()
            (passive-text-mode)))

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

(add-hook 'dired-mode-hook
          (lambda ()
            (setq mode-name "dired")
            (passive-text-mode)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (programming-common-hook)
;;            (enable-paredit-mode) FIXME:
            (setq indent-tabs-mode nil)
            (setq mode-name "el")
            (global-set-key (kbd "C-c C-c") 'comment-region)
            (turn-on-eldoc-mode)
            (auto-fill-mode -1)))

(add-hook 'gdb-inferior-io-mode-hook
          (lambda ()
            (passive-text-mode)))

(add-hook 'gud-mode-hook
          (lambda ()
            (passive-text-mode)
            ;; Lock the main gud window in place.
            (window-lock-mode)
            (gdb-many-windows 6)))

(add-hook 'help-mode-hook
          (lambda ()
            (passive-text-mode)))

(add-hook 'html-mode-common-hook
          (lambda ()
            (programming-common-hook)
            ;; Define the normal key binding for comment region.
            (global-set-key (kbd "C-c C-c") 'comment-region)
            ;; Indent by 2 spaces.
            (setq indent-tabs-mode nil)
            (setq-default tab-width 2)
            (setq indent-line-function 'insert-tab)))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (passive-text-mode)
            ;; Do not wrap lines. This ensures each entry in the buffer list
            ;; occupies only a single line.
            (setq truncate-lines t)))

(add-hook 'image-mode-hook
          (lambda ()
            (linum-mode -1)))

(add-hook 'Info-mode-hook
          (lambda ()
            (passive-text-mode)
            (linum-mode 1)))

(add-hook 'js2-mode-hook
          (lambda ()
            (programming-common-hook)
            ;; Lint.
            (lintnode-hook)
            ;; Folding.
            (imenu-add-menubar-index)
            (hs-minor-mode t)
            ;; Auto-complete mode.
            (auto-complete-mode 1)
            (set-variable 'indent-tabs-mode nil)))

(add-hook 'lisp-interaction-mode
          (lambda ()
            (setq mode-name "lisp")))

(add-hook 'mail-mode-hook
          (lambda ()
            ;; Move point past headers and insert newline.
            (forward-paragraph)
            (newline)
            (flyspell-mode 1)
            ;; C-c C-c to close.
            (define-key mail-mode-map [(control c) (control c)]
              (lambda ()
                (interactive)
                (save-buffer)
                (server-edit)))))

(add-hook 'makefile-mode-hook
          (lambda ()
            (programming-common-hook)
            (setq mode-name "make")
            (highlight-tabs-mode -1)))

(add-hook 'Man-mode-hook
          (lambda ()
            (passive-text-mode)))

(add-hook 'python-mode-hook
          (lambda ()
            (programming-common-hook)
            (setq python-indent 4)
            (setq indent-tabs-mode t)
            (setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
            (column-marker-1 80)
            (lambda-mode)
            (global-set-key (kbd "C-c c") 'py-execute-buffer)
            (define-key python-mode-map "C-c C-c" 'comment-region)
            (define-key python-mode-map "\"" 'electric-pair)
            (define-key python-mode-map "\'" 'electric-pair)
            (define-key python-mode-map "(" 'electric-pair)
            (define-key python-mode-map "[" 'electric-pair)
            (define-key python-mode-map "{" 'electric-pair)
            (define-key python-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'scheme-mode-hook
          (lambda ()
            (programming-common-hook)
;;            (enable-paredit-mode) FIXME:
            (setq indent-tabs-mode nil)
            (global-set-key (kbd "C-c C-c") 'comment-region)
            (global-set-key (kbd "C-c i") 'scheme-send-last-sexp)
            (global-set-key (kbd "C-c r") 'eval-region)
            (global-set-key (kbd "C-c b") 'eval-buffer)
            (auto-fill-mode -1)))

(add-hook 'sh-mode-hook
          (lambda ()
            (programming-common-hook)
            (setq mode-name "sh")
            (global-set-key (kbd "C-c C-c") 'comment-region)
            (setq indent-tabs-mode nil)
            (setq fill-column 80)
            (auto-fill-mode -1)))

(add-hook 'shell-mode-hook
          (lambda ()
            (passive-text-mode)))

(add-hook 'vala-mode-hook
          (lambda ()
            (programming-common-hook)
            (setq mode-name "vala")))

(add-hook 'wdired-mode-hook
          (lambda ()
            (setq mode-name "wdired")))

;;; Modeline.
;;; ==========================================================================

(defvar mode-line-warning-column 80)
(defvar mode-line-special-buffer-regexp "^\*.*\*$")
(defvar mode-line-file-string-length 35)

(defun truncate-dir-path (dir length)
  "Show up to `length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(setq-default
 mode-line-format
 '(" "
   ;; Directory and buffer/file name, unless buffer-name is '*...*' type
   (:propertize (:eval (if (not (string-match mode-line-special-buffer-regexp
                                              (buffer-name)))
                           (truncate-dir-path default-directory
                                              (- mode-line-file-string-length
                                                 (length (buffer-name))))))
                face mode-line-directory-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ;; TODO: Insert whitespace to pad out to fixed length all the time
   "  "
   ;; Position, including warning for 80 columns
   (:propertize "%l" face mode-line-position-face)
   ":"
   (:eval (propertize "%c" 'face
                      (if (>= (current-column) mode-line-warning-column)
                          'mode-line-80col-face
                        'mode-line-position-face))q)
   (:propertize " (%p)" face mode-line-minor-mode-face)
   modae-line-client " "
   ;; If not a special buffer, show read-only/edited status.
   (:eval
    (if (not (string-match mode-line-special-buffer-regexp (buffer-name)))
        (cond (buffer-read-only
               (propertize "RO" 'face 'mode-line-read-only-face))
              ((buffer-modified-p)
               (propertize "**" 'face 'mode-line-modified-face))
              (t "  "))))
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process,
   ;;                  global
   " %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (vc-mode vc-mode)
   (global-mode-string global-mode-string)))

;; Extra mode line faces.
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-directory-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
                    :foreground "gray60" :background "gray15"
                    :inverse-video nil
                    :box '(:line-width 3 :color "gray15" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :foreground "gray80" :background "gray30"
                    :inverse-video nil
                    :box '(:line-width 3 :color "gray30" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#4271ae"
                    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#c82829"
                    :background "#ffffff"
                    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-directory-face nil
                    :inherit 'mode-line-face
                    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :foreground "#eab700"
                    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    :family "Menlo"
                    :height 130)
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "gray40"
                    :height 110)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "black"
                    :background "#eab700")

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (passive-text-mode)))

;;; 3rd Party Packages.
;;; ==========================================================================

;;; auto-complete.el --- Auto Completion for GNU Emacs
;;; --------------------------------------------------------------------------

(require 'auto-complete-config)

;; Auto-Complete mode in all buffers.
(global-auto-complete-mode 1)

;; Make auto-complete case sensitive.
(setq ac-ignore-case nil)

;; Set default key.
(ac-set-trigger-key "TAB")

;; Set sources for auto-completions.
(setq ac-sources '(ac-source-symbols
                   ac-source-semantic
                   ac-source-words-in-same-mode-buffers
                   ac-source-dictionary
                   ac-source-filename
                   ac-source-functions
                   ac-new-yas-source
                   ac-source-yasnippet
                   ac-source-variables
                   ac-source-features
                   ac-source-abbrev))

;;; column-marker.el --- Highlight certain character columns
;;; --------------------------------------------------------------------------

(require 'column-marker)

;; Define a kery to enable column marker.
(global-set-key (kbd "C-x t m") 'column-marker-2)

;;; diminish.el --- Diminished modes are minor modes with no modeline display
;;; --------------------------------------------------------------------------

(require 'diminish)

;; Minor mode diminishes.
(eval-after-load "company" '(diminish 'company-mode "Cmp"))
(eval-after-load "abbrev" '(diminish 'abbrev-mode "Ab"))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode " Y"))
(eval-after-load "eldoc" '(diminish 'eldoc-mode "Doc"))

(diminish 'visual-line-mode "")

;;; doremi.el --- Do Re Mi: Incremental change using arrow keys or mouse wheel.
;;; --------------------------------------------------------------------------

(require 'doremi)
(require 'doremi-cmd)
(require 'doremi-frm)

;;; etags-select.el --- Select from multiple tags
;;; --------------------------------------------------------------------------

(require 'etags-select)

(global-set-key (kbd "C-c T") 'tags-search)
(global-set-key (kbd "C-c %") 'tags-query-replace)

;;; find-file-in-project.el --- Find files in a project quickly.
;;; --------------------------------------------------------------------------

(require 'find-file-in-project)

;;; flymake-cursor.el --- displays flymake error msg in minibuffer after delay
;;; --------------------------------------------------------------------------

(require 'flymake-cursor)

;;; grep-buffers.el --- grep through buffers (a la 'moccur')
;;; --------------------------------------------------------------------------

(require 'grep-buffers)

(global-set-key (kbd "C-c G") 'grep-buffers)

;;; highlight-tabs.el --- highlight tabs in buffer
;;; --------------------------------------------------------------------------

(require 'highlight-tabs)

;;; idle-highlight-mode.el --- highlight the word the point is on
;;; --------------------------------------------------------------------------

(require 'idle-highlight-mode)

;;; ido-ubiquitous.el --- Use ido (nearly) everywhere
;;; --------------------------------------------------------------------------

(require 'ido-ubiquitous)

;; Turn on ido-mode (almost) everywhere.
(ido-ubiquitous-mode)

;;; info+.el --- Extensions to `info.el'.
;;; --------------------------------------------------------------------------

(require 'info+)

;;; jslint.el --- flymake integration for jslint
;;; --------------------------------------------------------------------------

(require 'flymake-jslint)

(add-hook 'javascript-mode-hook
          (lambda ()
            (lintnode-hook)))

;;; magit.el --- control Git from Emacs
;;; --------------------------------------------------------------------------

(require 'magit)

(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (setq mode-name "git-msg")
            (flyspell-mode 1)
            (setq fill-column 72)
            (column-marker-2 72)))

(add-hook 'magit-mode-hook
          (lambda ()
            (setq mode-name "git")
            (passive-text-mode)))

;; TODO: Define a COMMIT_EDITMSG hook, something like this:

;; (setq auto-mode-alist (cons
;;                     '("COMMIT_EDITMSG$" . magit-log-edit-mode)
;;                     auto-mode-alist))

;; Open magit status with a key stroke.
(global-set-key (kbd "C-x g") 'magit-status)

;;; minimap.el --- Minimap sidebar for Emacs.
;;; --------------------------------------------------------------------------

(require 'minimap)

(global-set-key (kbd "s-.") 'minimap-create)

(setq minimap-width-fraction 0.15)

;; Update the minimap instantaneously (may slow down scrolling).
(setq minimap-update-delay 0)

;;; mo-git-blame.el --- An interactive, iterative 'git blame' mode for Emacs
;;; --------------------------------------------------------------------------

(require 'mo-git-blame)

(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;;; pager.el --- windows-scroll commands
;;; --------------------------------------------------------------------------

(require 'pager)

(global-set-key "\C-v" 'pager-page-down)
(global-set-key [next] 'pager-page-down)
(global-set-key "\ev" 'pager-page-up)
(global-set-key [prior] 'pager-page-up)
(global-set-key '[M-up] 'pager-row-up)
(global-set-key '[M-kp-8] 'pager-row-up)
(global-set-key '[M-down] 'pager-row-down)
(global-set-key '[M-kp-2] 'pager-row-down)

;; Bind a key to enable window resizing.
(global-set-key (kbd "C-x t w") 'doremi-window-height+)

;;; paredit.el --- minor mode for editing parentheses
;;; --------------------------------------------------------------------------

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)

(require 'paredit)

;;; pedant+.el --- highlight all that naughty formatting.
;;; --------------------------------------------------------------------------

(require 'pedant+)

;; Toggle Pedant+ mode in all buffers. With prefix ARG, enable Global-Pedant+
;; mode if ARG is positive; otherwise, disable it.
(global-pedant+-mode -1)

;; Toggle Pedant+ mode with a key binding.
(global-set-key (kbd "C-c n") 'pedant+-mode)

;;; pylookup.el --- emacs mode for searching python documents with convenience
;;; --------------------------------------------------------------------------

(setq pylookup-dir (concat site-lisp-dir "pylookup/"))

(require 'pylookup)

(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
(setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

;;; pymacs.el --- Interface between Emacs Lisp and Python
;;; --------------------------------------------------------------------------

(require 'pymacs)

(autoload 'pymacs-apply    "pymacs")
(autoload 'pymacs-call     "pymacs")
(autoload 'pymacs-eval     "pymacs" nil t)
(autoload 'pymacs-exec     "pymacs" nil t)
(autoload 'pymacs-load     "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

(add-to-list 'pymacs-load-path (concat site-lisp-dir "python/"))
(pymacs-load "ropemacs" "rope-")

;;; python-mode.el --- Towards an Python-IDE in Emacs
;;; --------------------------------------------------------------------------

(setq python-mode-dir (concat site-lisp-dir "python-mode/"))

(setq py-install-directory python-mode-dir)

(require 'python-mode)

;;; quack.el --- enhanced support for editing and running Scheme code
;;; --------------------------------------------------------------------------

(require 'quack)

;; Quack configuration.
(setq quack-fontify-style 'emacs
      quack-default-program "guile"
      quack-newline-behavior 'newline)

;;; slime.el --- Superior Lisp Interaction Mode for Emacs
;;; --------------------------------------------------------------------------

(require 'slime)

(slime-setup)

;; Program name for invoking an inferior Lisp with for Inferior Lisp mode.
(setq inferior-lisp-program "sbcl")

;; Program invoked by the `run-scheme' command.
(setq scheme-program-name "guile")

;;; smex.el --- M-x interface with Ido-style fuzzy matching.
;;; --------------------------------------------------------------------------

(require 'smex)

;; Can be omitted. This might cause a (minimal) delay when Smex is
;; auto-initialized on its first run.
(smex-initialize)

(setq smex-save-file (concat root-dir "smex-items"))

;; Replace default M-x binding with smex.
(global-set-key (kbd "M-x") 'smex)

;; Binding for smex major modes.
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;; tinyprocmail.el --- Emacs procmail minor mode. Lint code checker.
;;; --------------------------------------------------------------------------

(setq tinyprocmail--procmail-version "v3.22")

(add-hook 'tinyprocmail--load-hook 'tinyprocmail-install)

(require 'tinyprocmail)

;; Auto-load tinyprocmail mode for procmailrc files.
(add-to-list 'auto-mode-alist '("\\.procmailrc\\|pm-.*\\.rc$"
                                . turn-on-tinyprocmail-mode))

;;; vala-mode.el --- Vala mode derived mode
;;; --------------------------------------------------------------------------

(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)

;; Register .vala extension for vala-mode.
(setq auto-mode-alist
      (append '(("\\.vala$" . vala-mode)) auto-mode-alist))

;;; valgrind.el --- Based on compile.el included with Emacs
;;; --------------------------------------------------------------------------

(require 'valgrind)

;;; window-lock.el --- Lock an Emacs window.
;;; --------------------------------------------------------------------------

(require 'window-lock)

;; Toggle Window lock mode with key binding.
(global-set-key (kbd "C-x t l") 'window-lock-mode)

;;; xclip.el --- Emacs Interface to XClip.
;;; --------------------------------------------------------------------------

(require 'xclip)

(xclip-mode 1)

;;; Proceedures and variables.
;;; ==========================================================================

(defface trailing-whitespace
  '((((class color) (min-colors 88) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 88) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 16) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 16) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 8))
     :background "green" :foreground "black")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'basic-faces)

(defun layout-ide ()
  "Set up a simple IDE-like environment."
  (interactive)
  (split-window-vertically -8)
  (other-window 1)
  (shell)
  (window-lock-mode)
  (other-window 1)
  (split-window-horizontally)
  ;; Make sure we can get back here if our setup somehow gets messed up (use 'C-x r j i')
  (window-configuration-to-register ?i)
  (message "IDE initialized. Use 'C-x r j i' to return to this window configuration."))

(defun dired-find-file (&optional arg)
  "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
  (interactive "P")
  (let* ((fn-list (dired-get-marked-files nil arg)))
    (mapc 'find-file fn-list)))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

(defun git-grep ()
  (interactive)
  (setq grep-command "git --no-pager grep -nH ")
  (call-interactively 'grep))

(defun electric-pair ()
  "Insert character pair without sournding spaces."
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defun bounce-matching-paren ()
  "Will bounce between matching parens just like % in vi"
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (error "%s" "Not on a paren, brace, or bracket")))))

(defun clean-buffer ()
  "Untabify the whole buffer and remove trailing whitespace."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)
    (message "Cleaned code.")))

(defun format-buffer ()
  "Untabify the whole buffer, remove trailing whitespace and auto-indent."
  (interactive)
  (clean-buffer)
  (save-excursion
    (mark-whole-buffer)
    (indent-for-tab-command)
    (message "Formatted code.")))

(defun set-compile-command ()
  "Set the compile-command variable."
  (interactive)
  (let ((new-compile-command (read-from-minibuffer
                              "Compile command: "
                              compile-command)))
    (setq compile-command new-compile-command)))

(defun quick-compile ()
  "Run the compile command (no prompt)."
  (interactive)
  (compile compile-command))

(defun run-gdb ()
  "Run GDB session"
  (interactive)
  ;; TODO: Make this prompt auto-complete file paths.
  (let ((gdb-command (read-from-minibuffer
                      "Run gdb (like this): "
                      "gdb -i=mi ")))
    (gdb gdb-command)))

(defun beginning-of-next-line()
  "Moves cursor to the beginning of the next line"
  (interactive)
  (end-of-line)
  (if (not (eobp))
      (forward-char 1)))

(defun kill-start-of-line ()
  "Kill from point to start of line."
  (interactive)
  (kill-line 0))

(defun passive-text-mode ()
  "Disable all text editing niceities"
  (interactive)
  (pedant+-mode -1)
  (linum-mode -1)
  (abbrev-mode -1)
  (auto-fill-mode -1)
  (visual-line-mode 1))

(defun guile-doc ()
  "Browse the Guile Manual"
  (interactive)
  (browse-url "http://www.gnu.org/software/guile/manual/guile.html"))

(defun toggle-source-header()
  "Switches to the source buffer if currently in the header buffer and vice versa."
  (interactive)
  (let ((buf (current-buffer))
        (name (file-name-nondirectory (buffer-file-name)))
        file
        offs)
    (setq offs (string-match c++-header-ext-regexp name))
    (if offs
        (let ((lst c++-source-extension-list)
              (ok nil)
              ext)
          (setq file (substring name 0 offs))
          (while (and lst (not ok))
            (setq ext (car lst))
            (if (file-exists-p (concat file "." ext))
                (setq ok t))
            (setq lst (cdr lst)))
          (if ok
              (find-file (concat file "." ext))))
      (let ()
        (setq offs (string-match c++-source-ext-regexp name))
        (if offs
            (let ((lst c++-header-extension-list)
                  (ok nil)
                  ext)
              (setq file (substring name 0 offs))
              (while (and lst (not ok))
                (setq ext (car lst))
                (if (file-exists-p (concat file "." ext))
                    (setq ok t))
                (setq lst (cdr lst)))
              (if ok
                  (find-file (concat file "." ext)))))))))

(defun latex-insert-itemize ()
  "Insert new itemized list at point."
  (interactive)
  (insert "\\begin{itemize}
")
  (indent-for-tab-command)
  (insert "\\item ")
  (save-excursion
    (insert "
\\end{itemize}
")))

(defun latex-insert-enumerate ()
  "Insert new enumerated list at point."
  (interactive)
  (insert "\\begin{enumerate}
")
  (indent-for-tab-command)
  (insert "\\item ")
  (save-excursion
    (insert "
\\end{enumerate}
")))

(defun latex-format-verbatim ()
  "Begin verbatim text block."
  (interactive)
  (insert "\\begin{verbatim}
")
  (save-excursion
    (insert "
\\end{verbatim}
")))

(defun latex-format-texttt ()
  "Wrap selected region with monospace font tags."
  (interactive)
  (insert "\\texttt{")
  (save-excursion
    (insert "} ")))

(defun latex-format-textit ()
  "Wrap selected region with monospace font tags."
  (interactive)
  (insert "\\textit{")
  (save-excursion
    (insert "} ")))

(defun latex-format-textbf ()
  "Wrap selected region with monospace font tags."
  (interactive)
  (insert "\\textbf{")
  (save-excursion
    (insert "} ")))

;;; Emacs server.
;;; ==========================================================================

;; Start an emacs server, if one is not already running.
(or (server-running-p)
    (server-start))
