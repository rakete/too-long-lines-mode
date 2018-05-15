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

;; It works adding the function too-long-lines-hide into the find-file-hook and
;; into after-change-function so that it is called as soon as a file is opened
;; or whenever is something inserted into a buffer.

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

(defvar too-long-lines-hide-current-timer nil)

(defvar too-long-lines-idle-seconds 1)

(defun too-long-lines-hide (&optional beg end len)
  "Hides lines that are longer then `too-long-lines-threshold'.

It replaces them by the first number characters of the line as configured in
variable `too-long-lines-show-number-of-characters', and a little info blurp
about how many characters were hidden.

BEG and END arguments can be used to narrow the region in which this function
looks for too long lines. LEN is only there so this function can be added
to `after-change-functions'.

See also `too-long-lines-threshold', `too-long-lines-show-number-of-characters',
and `too-long-lines-show'."
  (interactive)
  (unless (window-minibuffer-p)
    (save-excursion
      (goto-char (or beg (point-min)))
      (let ((done nil))
        (while (not done)
          (setq done (>= (line-end-position) (or end (point-max))))
          (let ((line-length (- (line-end-position) (line-beginning-position)))
                (already-hidden nil))
            (when (> line-length too-long-lines-threshold)
              (dolist (ov (overlays-in (line-end-position) (line-beginning-position)))
                (when (overlay-get ov 'too-long-line)
                  (setq already-hidden t)))
              (unless already-hidden
                (let ((ov (make-overlay (+ (line-beginning-position) too-long-lines-show-number-of-characters) (line-end-position) (current-buffer))))
                  (overlay-put ov 'too-long-line t)
                  (overlay-put ov 'display (concat "... " (prin1-to-string (- line-length too-long-lines-show-number-of-characters)) " hidden characters"))
                  (overlay-put ov 'face '(:background "#ff0066"))
                  )
                )))
          (when (eq (point) (goto-char (line-beginning-position 2)))
            (setq done t)))))))

(defun too-long-lines-run-with-idle-timer (&optional beg end len)
  (interactive)
  (when too-long-lines-hide-current-timer
    (cancel-timer too-long-lines-hide-current-timer)
    (setq too-long-lines-hide-current-timer nil))
  (lexical-let ((lexical-beg (or beg (point-min)))
                (lexical-end (or end (point-max))))
    (setq too-long-lines-hide-current-timer
          (run-with-idle-timer too-long-lines-idle-seconds nil 'too-long-lines-hide lexical-beg lexical-end))))

(defun too-long-lines-show ()
  "Restore all lines previously hidden by `too-long-lines-hide' in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((done nil))
      (while (not done)
        (setq done (>= (line-end-position) (or end (point-max))))
        (dolist (ov (overlays-in (line-beginning-position) (line-end-position)))
          (when (overlay-get ov 'too-long-line)
            (delete-overlay ov)))
        (when (eq (point) (goto-char (line-beginning-position 2)))
          (setq done t))))))

;;;###autoload
(define-minor-mode too-long-lines-mode
  "A minor that hides lines that are longer then a configurable threshold.

See also `too-long-lines-hide'."
  nil
  " tll"
  '()
  (if too-long-lines-mode
      (progn
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (too-long-lines-hide)))
        (add-hook 'find-file-hook 'too-long-lines-hide)
        (add-hook 'post-command-hook 'too-long-lines-run-with-idle-timer)
        )
    (progn
      (remove-hook 'find-file-hook 'too-long-lines-hide)
      (remove-hook 'post-command-hook 'too-long-lines-run-with-idle-timer)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (too-long-lines-show))))))

(provide 'too-long-lines-mode)
;;; too-long-lines-mode.el ends here
