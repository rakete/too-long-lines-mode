;;; -*- lexical-binding: t -*-
;;; too-long-lines-mode.el --- Minor mode for hiding too long lines

;; Copyright (C) 2017 Andreas Raster

;; Author: Andreas Raster <lazor@affenbande.org>
;; Version: 0
;; Keywords: convenience files maint
;; Package-Requires: ((emacs "24.2"))
;; URL: https://github.com/rakete/too-long-lines-mode

;; This file is part of too-long-lines-mode.

;; too-long-lines-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; too-long-lines-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with too-long-lines-mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs has a problem when displaying very long lines: it becomes unusable slow.
;; This global minor mode works around this problem by hiding very long lines and
;; replacing them with only a few of their first characters and a little info
;; blurp about how many characters were hidden.

;; It works adding the function too-long-lines-hide into the find-file-hook so that
;; it is called as soon as a file is opened.

;; too-long-lines-hide goes through all lines in a buffer, checks if the line
;; is longer then too-long-lines-threshold and if it is, it creates an overlay
;; with the lines replacement in its 'display property that is then displayed
;; instead of the very long line.

;; To use this mode just require this file, configure too-long-lines-threshold
;; and too-long-lines-show-number-of-characters to your pleasing and call
;; too-long-lines-mode to enable the mode globally.

;;; Code:
(defvar too-long-lines-threshold 10000
  "The threshold after which `too-long-lines-hide' cuts of a line and hides the rest.")

(defvar too-long-lines-show-number-of-characters 30
  "How many characters of a line remain shown after it is hidden.")

(defvar too-long-lines-hide-current-timer nil
  "The current timer for `too-long-lines-run-with-idle-timer-in-special-buffers'.

Do not set this manually, use `too-long-lines-idle-seconds' to specify the
idle seconds after which `too-long-lines-run-with-idle-timer-in-special-buffers'
runs `too-long-lines-hide'.")

(defvar too-long-lines-idle-seconds 3
  "Set this to how many seconds emacs should be idle before
`too-long-lines-run-with-idle-timer-in-special-buffers' runs `too-long-lines-hide'.")

(defvar too-long-lines-special-buffer-modes '(shell-mode)
  "The modes which `too-long-lines-run-with-idle-timer-in-special-buffers' recognizes
as special buffers in which `too-long-lines-hide' should be run periodically.")

(defvar too-long-lines-toggle-key [(tab)])

(defun too-long-lines-hide (&optional beg end len)
  "Hides lines that are longer then `too-long-lines-threshold'.

It replaces too long lines with the first N number characters of the line as
configured in variable `too-long-lines-show-number-of-characters', and a little
info blurp about how many characters were hidden.

See also `too-long-lines-threshold', `too-long-lines-show-number-of-characters',
`too-long-lines-hide-in-buffers' and `too-long-lines-show'."
  (interactive)
  (save-excursion
    (goto-char (or beg (point-min)))
    (let ((done nil))
      (while (not done)
        (let ((line-beg (line-beginning-position))
              (line-end (line-end-position)))
          (setq done (>= line-end (or end (point-max))))
          (let ((line-length (- line-end line-beg))
                (already-hidden nil)
                (cur-too-long-line t)
                (too-long-lines-overlay-display
                 (lambda (o)
                   (if (eq (overlay-get o 'too-long-line) 'shown)
                       (progn (overlay-put o 'display nil)
                              (overlay-put o 'face '(:box (:line-width -1 :color "#ff0066"))))
                     (overlay-put o 'display (concat "... " (prin1-to-string (overlay-get o 'num-hidden)) " hidden characters"))
                     (overlay-put o 'face '(:background "#ff0066"))))))
            (when (> line-length too-long-lines-threshold)
              (message "should hide %S" line-length)
              (overlay-recenter line-end)
              (dolist (ov (overlays-in line-beg line-end))
                (if (overlay-get ov 'too-long-line)
                    (if already-hidden
                        (delete-overlay ov)
                      (setq already-hidden ov))))
              (if already-hidden
                  (move-overlay already-hidden (+ line-beg too-long-lines-show-number-of-characters) line-end)
                (lexical-let (
                      (ov (make-overlay (+ line-beg too-long-lines-show-number-of-characters) line-end (current-buffer)))
                      (too-long-keymap (make-sparse-keymap))
                      (line-length line-length)
                      (too-long-lines-overlay-display too-long-lines-overlay-display)
                      )
                  (setq already-hidden ov)
                  (define-key too-long-keymap too-long-lines-toggle-key
                    (lambda () (interactive)
                      (overlay-put ov 'too-long-line (if (eq (overlay-get ov 'too-long-line) 'shown) t 'shown))
                      (funcall too-long-lines-overlay-display ov)))
                  (overlay-put ov 'keymap too-long-keymap)
                  (overlay-put ov 'ovaporate t)
                  (overlay-put ov 'too-long-line cur-too-long-line)))
              (overlay-put already-hidden 'num-hidden (- line-length too-long-lines-show-number-of-characters))
              (funcall too-long-lines-overlay-display already-hidden))))
        (when (eq (point) (goto-char (line-beginning-position 2)))
          (setq done t))))))

(defun too-long-lines-hide-after-change-in-special-buffer (&optional beg end len)
  "Run `too-long-lines-hide' after changes in current buffer

But only if its mode is in `too-long-lines-special-buffer-modes'. Arguments
BEG, END and LEN are passed to `too-long-lines-hide'."
  (when (cl-some (lambda (x) (eq major-mode x)) too-long-lines-special-buffer-modes)
    (too-long-lines-hide beg end len)))

(defun too-long-lines-hide-in-buffers (&optional buffers)
  "Run `too-long-lines-hide' in multiple buffers.

Argument BUFFERS is a list of buffers in which this function looks for too
long lines to hide, if it is not specified this function will look in all
visible buffers for too long lines to hide.

See also `too-long-lines-threshold', `too-long-lines-show-number-of-characters',
`too-long-lines-hide' and `too-long-lines-show'."
  (interactive)
  (let ((buffers (or buffers
                     (cl-mapcan (lambda (buf)
                                  (when (get-buffer-window buf)
                                    (list buf)))
                                (buffer-list)))))
    (cl-pushnew (current-buffer) buffers)
    (dolist (buf buffers)
      (unless (window-minibuffer-p (get-buffer-window buf))
        (with-current-buffer buf
          (too-long-lines-hide (point-min) (point-max)))))))

(defun too-long-lines-run-with-idle-timer-in-special-buffers ()
  "Periodically run `too-long-lines-hide' in buffers that are in a major-mode from
`loo-long-lines-special-buffer-modes'.

See also `too-long-lines-idle-seconds'."
  (when (not (eq too-long-lines-hide-current-timer nil))
    (cancel-timer too-long-lines-hide-current-timer)
    (setq too-long-lines-hide-current-timer nil))
  (setq too-long-lines-hide-current-timer
        (run-with-idle-timer too-long-lines-idle-seconds t
                             (lambda ()
                               (let ((special-buffers '()))
                                 (dolist (buffer (buffer-list))
                                   (when (cl-some (lambda (x) (eq (with-current-buffer buffer major-mode) x)) too-long-lines-special-buffer-modes)
                                     (setq special-buffers (append special-buffers (list buffer)))))
                                 (too-long-lines-hide-in-buffers special-buffers))))))

(defun too-long-lines-show ()
  "Restore all lines previously hidden by `too-long-lines-hide' in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((done nil))
      (while (not done)
        (setq done (>= (line-end-position) (point-max)))
        (dolist (ov (overlays-in (line-beginning-position) (line-end-position)))
          (when (overlay-get ov 'too-long-line)
            (delete-overlay ov)))
        (when (eq (point) (goto-char (line-beginning-position 2)))
          (setq done t))))))

;;;###autoload
(define-minor-mode too-long-lines-mode
  "A minor that hides lines that are longer then a configurable threshold.

See also `too-long-lines-hide'."
  :global t
  :init-value nil
  :lighter " tll"
  :keymap '()
  (if too-long-lines-mode
      (progn
        (too-long-lines-hide)
        (add-hook 'find-file-hook 'too-long-lines-hide)
        (add-hook 'after-change-functions 'too-long-lines-hide-after-change-in-special-buffer))
    (progn
      (remove-hook 'find-file-hook 'too-long-lines-hide)
      (remove-hook 'after-change-functions 'too-long-lines-hide-after-change-in-special-buffer)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (too-long-lines-show)))
      (when (timerp too-long-lines-hide-current-timer)
        (cancel-timer too-long-lines-hide-current-timer)
        (setq too-long-lines-hide-current-timer nil)))))

(provide 'too-long-lines-mode)
;;; too-long-lines-mode.el ends here
