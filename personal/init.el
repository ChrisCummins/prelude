;;; init.el --- Emacs configuration file
;;
;; Copyright (C) 2012, 2013 Christopher E. Cummins
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
;;    Personal configuration for Emacs with the prelude package.
;;
;;; Table of Contents:
;;
;;    Procedures.
;;    General settings.
;;    Appearance, text and input.
;;    Files, buffers and windows.
;;    Terminal integration.
;;    Mode-specific configurations.
;;    Modeline.
;;
;;; Issues and to-do list:
;;
;;    None.
;;
;;; Code:

;;; Procedures.
;;; ==========================================================================

(defun compile-command-set ()
  "Set the compile-command variable."
  (interactive)
  (let ((new-compile-command (read-from-minibuffer
                              "Compile command: "
                              compile-command)))
    (setq compile-command new-compile-command)))

(defun c-toggle-header-file()
  "Switches a .c source buffer to the .h header buffer and vice versa."
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

(defun open-in-vim ()
  "Opens the current file in vim."
  (interactive)
  (if (not (equal buffer-file-name nil))
      (let ((vim-prefix "gnome-terminal -e 'vim") (vim-postfix "'"))
        (save-buffer) ;; First, write changes to disk
        (recenter) ;; Vim opens files with the view centred
        (shell-command-to-string (concat vim-prefix " "
                                         (shell-quote-argument buffer-file-name)
                                         " +"
                                         (number-to-string (line-number-at-pos))
                                         vim-postfix " &>/dev/null &")))
    ;; Not all buffers are associated with files
    (message "Not a real file")))


;;; General settings.
;;; ==========================================================================

;; Prompt to quit before closing Emacs.
(setq kill-emacs-query-functions
      (cons (lambda ()
              (yes-or-no-p "Exit Emacs?"))
            kill-emacs-query-functions))

;; Fuck off guru mode!
(defun disable-guru-mode ()
  (guru-mode -1))

(add-hook 'prelude-prog-mode-hook 'disable-guru-mode t)

;; Case insensitive list sorting.
(setq sort-fold-case t)

;; Setup dictionary.
(setq ispell-dictionary "british")

;; Revert a buffer through a key binding.
(global-set-key (kbd "M-R") 'revert-buffer)

;; Set text-mode for known plaintext files.
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist)
(setq auto-mode-alist (cons '("INSTALL" . text-mode) auto-mode-alist)))
(setq auto-mode-alist (cons '("LICENSE" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("COPYING" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt$" . text-mode) auto-mode-alist))


;;; Appearance, text and input.
;;; ==========================================================================

;; Set a prettier line highlight.
(global-hl-line-mode 1)
(set-face-background 'hl-line "#2f2f2f")
(set-face-foreground 'highlight nil)

;; Cursor to use when this buffer is in the selected window.
(set-default 'cursor-type 'bar)

;; Set a cursor color.
(setq default-frame-alist
      '((cursor-color . "red")))


;;; Terminal integration.
;;; ==========================================================================

;; XTerm mouse mode enables mouse commands within the terminal,
;; e.g. click to set cursor at mouse point, etc.
(xterm-mouse-mode 1)


;;; Mode-specific configurations.
;;; ==========================================================================

;; Allow wdired to edit file permissions.
(setq wdired-allow-to-change-permissions t)

(setq js-indent-level 2)


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