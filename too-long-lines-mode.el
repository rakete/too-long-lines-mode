
(defvar too-long-lines-threshold 10000)

(defvar too-long-lines-mode-enabled t)

(defun too-long-lines-hide ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (= (point) (point-max)))
      (let ((line-length (- (point-at-eol) (point-at-bol)))
            (already-hidden nil))
        (when (> line-length too-long-lines-threshold)
          (dolist (ov (overlays-in (point-at-bol) (point-at-eol)))
            (when (overlay-get ov 'too-long-line)
              (setq already-hidden t)))
          (unless already-hidden
            (let ((ov (make-overlay (+ (point-at-bol) 30) (point-at-eol) (current-buffer))))
              (overlay-put ov 'too-long-line t)
              (overlay-put ov 'display (concat "... " (prin1-to-string (- line-length 30)) " hidden characters"))
              ;;   (overlay-put ov 'before-string (concat "... " (prin1-to-string (- line-length 30)) " hidden characters"))
              )
            )))
      (forward-line 1))))

(defun too-long-lines-show ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (= (point) (point-max)))
      (dolist (ov (overlays-in (point-at-bol) (point-at-eol)))
        (when (overlay-get ov 'too-long-line)
          (delete-overlay ov)))
      (forward-line 1))))

(defadvice set-auto-mode (before too-long-lines-hide-before-set-auto-mode disable)
  (too-long-lines-hide))

;;;###autoload
(defun too-long-lines-mode-on ()
  "Enable the too-long-lines-mode functionality."
  (interactive)
  (ad-enable-advice 'set-auto-mode 'before 'too-long-lines-hide-before-set-auto-mode)
  (ad-activate 'set-auto-mode)
  (setq too-long-lines-mode-enabled t))

(defun too-long-lines-mode-off ()
  "Disable the too-long-lines-mode functionality."
  (interactive)
  (ad-disable-advice 'set-auto-mode 'before 'too-long-lines-hide-before-set-auto-mode)
  (ad-activate 'set-auto-mode)
  (setq too-long-lines-mode-enabled nil))

(ad-activate 'set-auto-mode)

(provide 'too-long-lines-mode)
