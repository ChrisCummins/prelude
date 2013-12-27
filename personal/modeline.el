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
