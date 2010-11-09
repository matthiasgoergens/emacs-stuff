;; A command to save a point to a ring. We can then cycle the ring,
;; activate the head, and pop the head.

(defvar point-ring-size 16 "The default size of the point-ring.")

(defvar point-ring (make-ring point-ring-size) "The point ring.")

(defun clear-point-ring ()
  (interactive)
  (message "Point ring cleared")
  (setq point-ring (make-ring point-ring-size)))

(defun push-point-ring ()
  (interactive)
  (message "Current point pushed to point ring")
  (ring-insert ;-at-beginning
   point-ring
   (cons (current-buffer) (point))))

(defun ring-rotate (r)
  (let ((h (ring-remove r)))
    (ring-insert r h)))

;; should this rotate ring first?
;; no! we might not have pushed the current point!
(defun set-point-rotate-ring ()
  (interactive)
  (message "Rotating point ring")
  ;; (let ((p (ring-rotate point-ring)))
  (ring-rotate point-ring)
  (let ((p (ring-ref point-ring 0)))
    (switch-to-buffer (car p))
    (goto-char (cdr p))))

;; TODO: this isn't working right!
(defun pop-point-ring ()
  (interactive)
  (message "Current point popped from point ring")
  (ring-remove point-ring (ring-length point-ring)))
  ;(ring-remove point-ring))

;; displays contents of point-ring in message buffer
(defun show-point-ring ()
  (interactive)
  (let ((r (ring-elements point-ring)))
    (message
     (concat "Contents of point ring: " (mapcar (lambda (p) (format "%s:%d " (car p) (cdr p))) r)))))

(global-set-key (kbd "C-x p") 'push-point-ring)
(global-set-key (kbd "C-x M-p") 'pop-point-ring)
(global-set-key (kbd "C-x C-x") 'set-point-rotate-ring)
