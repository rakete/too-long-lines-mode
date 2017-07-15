;;; too-long-lines-mode.el --- Minor mode for hiding too long lines

;; Copyright (C) 2017 Andreas Raster

;; Author: Andreas Raster <lazor@affenbande.org>
;; Version: 0
;; Keywords: workarounds display
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


;;; Code:
(defvar too-long-lines-threshold 10000)

(defvar too-long-lines-show-characters 30)

(defvar too-long-lines-mode-enabled nil)

(defun too-long-lines-hide (&optional beg end len)
  (interactive)
  (when (and (buffer-file-name (current-buffer)) (file-exists-p (buffer-file-name (current-buffer))))
    (save-excursion
      (goto-char (or beg (point-min)))
      (while (not (>= (point) (or end (point-max))))
        (let ((line-length (- (point-at-eol) (point-at-bol)))
              (already-hidden nil))
          (when (> line-length too-long-lines-threshold)
            (dolist (ov (overlays-in (point-at-bol) (point-at-eol)))
              (when (overlay-get ov 'too-long-line)
                (setq already-hidden t)))
            (unless already-hidden
              (let ((ov (make-overlay (+ (point-at-bol) too-long-lines-show-characters) (point-at-eol) (current-buffer))))
                (overlay-put ov 'too-long-line t)
                (overlay-put ov 'display (concat "... " (prin1-to-string (- line-length too-long-lines-show-characters)) " hidden characters"))
                )
              )))
        (forward-line 1)))))

(defun too-long-lines-show ()
  (interactive)
  (when (and (buffer-file-name (current-buffer)) (file-exists-p (buffer-file-name (current-buffer))))
    (save-excursion
      (goto-char (point-min))
      (while (not (= (point) (point-max)))
        (dolist (ov (overlays-in (point-at-bol) (point-at-eol)))
          (when (overlay-get ov 'too-long-line)
            (delete-overlay ov)))
        (forward-line 1)))))

;;;###autoload
(define-minor-mode too-long-lines-mode
  ""
  nil
  " tll"
  '()
  :global t
  (if (not too-long-lines-mode-enabled)
      (progn
        (setq too-long-lines-mode-enabled t)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (too-long-lines-hide)))
        (add-hook 'find-file-hook 'too-long-lines-hide)
        (add-hook 'after-change-functions 'too-long-lines-hide))
    (progn
      (remove-hook 'find-file-hook 'too-long-lines-hide)
      (remove-hook 'after-change-functions 'too-long-lines-hide)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (too-long-lines-show)))
      (setq too-long-lines-mode-enabled nil))))

(provide 'too-long-lines-mode)
