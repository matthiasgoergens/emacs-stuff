; (***********************************************************************)
; (*                                                                     *)
; (*                            OCamlSpotter                             *)
; (*                                                                     *)
; (*                             Jun FURUSE                              *)
; (*                                                                     *)
; (*   Copyright 2008, 2009 Jun Furuse. All rights reserved.             *)
; (*   This file is distributed under the terms of the GNU Library       *)
; (*   General Public License, with the special exception on linking     *)
; (*   described in file LICENSE.                                        *)
; (*                                                                     *)
; (***********************************************************************)

; How-to-use
; 
; Write the following to your .emacs
;
; ; load-path
; (setq load-path (cons "WHERE-YOU-HAVE-INSTALLED-THE-ELISP" load-path))
;
; ; set the path of the ocamlspot binary
; (setq ocamlspot-path "WHERE-YOU-HAVE-INSTALLED-THE-BINARIES/ocamlspot")
;
; ; autoload
; (autoload 'ocamlspot-query "ocamlspot" "OCamlSpot")
;
; ; tuareg mode hook (use caml-mode-hook instead if you use caml-mode)
;   (add-hook 'tuareg-mode-hook 
;   	  '(lambda ()
; 	     (local-set-key "\C-c;" 'ocamlspot-query)
; 	     (local-set-key "\C-c\C-t" 'ocamlspot-type)
;            (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
; 	     (local-set-key "\C-c\C-u" 'ocamlspot-use)
; 	     (local-set-key "\C-ct" 'caml-types-show-type)))
 
(defvar ocamlspot-buffer "*ocamlspot*")

(defvar ocamlspot-path "OCAML-SOURCE-TREE/ocamlspot/ocamlspot"
  "ocamlspot program path")

;; debug

(defvar ocamlspot-debug nil "turn on debug output")

(defvar ocamlspot-debug-buffer "*ocamlspot-debug*")
(defun ocamlspot-debug-message (s)
  (save-excursion
    (if (get-buffer ocamlspot-debug-buffer) ()
      (generate-new-buffer ocamlspot-debug-buffer))
    (set-buffer ocamlspot-debug-buffer)
    (insert s)
    (insert "\n")))

;; the spot overlay
(defvar ocamlspot-spot-overlay (make-overlay 1 1))
(make-face 'ocamlspot-spot-face)
(set-face-doc-string 'ocamlspot-spot-face "face for ocamlspot spot highlight")
(set-face-background 'ocamlspot-spot-face "#88FF44")
(overlay-put ocamlspot-spot-overlay 'face 'ocamlspot-spot-face)

;; the tree overlay
(defvar ocamlspot-tree-overlay (make-overlay 1 1))
(make-face 'ocamlspot-tree-face)
(set-face-doc-string 'ocamlspot-tree-face "face for ocamlspot tree highlight")
(set-face-background 'ocamlspot-tree-face "#FF88FF")
(overlay-put ocamlspot-tree-overlay 'face 'ocamlspot-tree-face)

(defun ocamlspot-delete-overlays-now ()
  (interactive)
  (delete-overlay ocamlspot-tree-overlay)
  (delete-overlay ocamlspot-spot-overlay))

(defun ocamlspot-delete-overlays ()
  (unwind-protect
      (sit-for 10)
    (ocamlspot-delete-overlays-now)))

; (defun ocamlspot-cnum-of-point () (1- (point)))
; (defun ocamlspot-char-of-cnum (n) (1+ n))

(defun ocamlspot-find-file-existing (path)
  (if (file-exists-p path)
      (find-file-other-window path)
    (message "ERROR: source file %s was not found" path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; column chars => column bytes

(defun ocamlspot-pos-beginning-of-line ()
  (save-excursion
    (beginning-of-line)
    (point)))
 
(defun ocamlspot-string-of-line-to-point ()
  (buffer-substring-no-properties 
   (ocamlspot-pos-beginning-of-line) (point)))

(defun ocamlspot-bytes-of-line-to-point ()
  (length 
   (encode-coding-string 
    (ocamlspot-string-of-line-to-point) buffer-file-coding-system)))

(defun ocamlspot-lines-of-point ()
  (count-lines (point-min) (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; column bytes => column chars  

; goto-line set mark and we see the result in the minibuffer
(defun ocamlspot-goto-line (line)
  (goto-char (point-min))
  (forward-line (- line 1)))

;; get the string at line
(defun ocamlspot-buffer-substring-at-line (line)
  ; no need of save-excursion
  (ocamlspot-goto-line line)
  (beginning-of-line)
  (let ((start (point)))
    (end-of-line)
    (buffer-substring-no-properties start (point))))

(defun ocamlspot-chars-of-bytes-of-string (str bytes)
  (length 
   (decode-coding-string 
    (substring (encode-coding-string str buffer-file-coding-system)
               0 bytes)
    buffer-file-coding-system)))

(defun ocamlspot-pos-of-bytes-at-line (line bytes)
  ; no need of save-excursion
  (ocamlspot-goto-line line)
  (let ((pos-at-beginning-of-line (ocamlspot-pos-beginning-of-line))
        (chars-from-beginning-of-line 
         (ocamlspot-chars-of-bytes-of-string 
          (ocamlspot-buffer-substring-at-line line) bytes)))
    (+ pos-at-beginning-of-line chars-from-beginning-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; location parser

; parses lxxxcxxxbxxx and returns the triplet
(defun ocamlspot-parse-location (s)
  (if (string-match "^l\\([\-0-9]+\\)c\\([\-0-9]+\\)b\\([\-0-9]+\\)$" s)
      (let ((line (string-to-int 
                   (substring s (match-beginning 1) (match-end 1))))
            (colbytes (string-to-int
                       (substring s (match-beginning 2) (match-end 2))))
            (bytes (string-to-int
                       (substring s (match-beginning 3) (match-end 3)))))
        (list line colbytes bytes))
    ; older version
    (if (string-match "^\\([\-0-9]+\\)$" s)
        (let ((line -1)
              (colbytes -1)
              (bytes (string-to-int
                      (substring s (match-beginning 1) (match-end 1)))))
          (list line colbytes bytes))
      nil)))

(defun ocamlspot-pos-of-location (buffer s)
  (destructuring-bind (line colbytes bytes) (ocamlspot-parse-location s)
    (if (= line -1) bytes
      (save-excursion
        (set-buffer buffer)
        (ocamlspot-pos-of-bytes-at-line line colbytes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; overlay handling

; (defun ocamlspot-display-overlay (filename position overlay)
;   (if (string-match "\.cm[ioxa]$" filename)
;       ;; It is not an .ml or .mli. Packed module.
;       (progn 
;         (message "Packed module: %s" filename)
;         ;; CR jfuruse: opening a binary file is not good
;         (setq target-buffer (ocamlspot-find-file-existing filename)))
;     (progn
;       (setq target-buffer (ocamlspot-find-file-existing filename))
;       (if (string-match "^\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[\-0-9]+\\):\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[\-0-9]+\\)$" position)
;           (let ((start (substring position (match-beginning 1) (match-end 1)))
;                 (end   (substring position (match-beginning 2) (match-end 2))))
;             (let ((start (ocamlspot-pos-of-location target-buffer start))
;                   (end   (ocamlspot-pos-of-location target-buffer end)))
;               ;; display the result
;               (save-excursion
;                 (set-buffer target-buffer)
;                 (goto-char start)
;                 (move-overlay overlay start end target-buffer))))))))

(defun ocamlspot-display-overlay (buffer position overlay)
  (if (string-match "^\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[\-0-9]+\\):\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[\-0-9]+\\)$" position)
      (let ((start (substring position (match-beginning 1) (match-end 1)))
            (end   (substring position (match-beginning 2) (match-end 2))))
        (let ((start (ocamlspot-pos-of-location buffer start))
              (end   (ocamlspot-pos-of-location buffer end)))
          ;; display the result
          (set-buffer buffer)
          (goto-char start)
          (move-overlay overlay start end buffer)))
    ; this should be all
    (progn
      (display-buffer buffer)
      (move-overlay overlay (point-min) (point-max) buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ocamlspot-warning ()
  (if (re-search-forward "^\\(Warning: .*\\)$" nil t)
      (buffer-substring-no-properties (match-beginning 1) (match-end 1))
    nil))

(defun ocamlspot-warnings-rev (lst)
  (let ((warning (ocamlspot-warning)))
    (if warning (ocamlspot-warnings-rev (concat lst warning "\n"))
      lst)))

(defun ocamlspot-warnings ()
  (goto-char (point-min))
  (ocamlspot-warnings-rev ""))

; launch ocamlspot 
; result is stored in the buffer "ocamlspot-buffer"
; the current buffer is stored in source_buffer
(defun ocamlspot-gen-query (extra_args)
  (interactive)
  (save-excursion
    (ocamlspot-delete-overlays-now)
    ;; arguments
    (let ((file-name (buffer-file-name))
          (arg
           (format "%s:l%dc%d"
                   (buffer-file-name)
                   (ocamlspot-lines-of-point)
                   (ocamlspot-bytes-of-line-to-point))
           ))
      ;; ocamlspot buffer
      (setq source-buffer (current-buffer))
      (save-current-buffer
        (set-buffer (get-buffer-create ocamlspot-buffer))
        (erase-buffer)
        (let* ((debug
  	      ; (if ocamlspot-debug '("--debug") nil))
                (if ocamlspot-debug '("-debug") nil))
  	     (command 
  	      (append '(call-process ocamlspot-path nil ocamlspot-buffer nil)
                        ; '("--version") ; it's new
  		      debug
  		      extra_args
  		      '(arg))))
          ;; chdir is required
          (cd (file-name-directory file-name))
  	(eval command))
        ;; search the found tree element
        (goto-char (point-min))
        (if (re-search-forward "^Tree: \\(l[\-0-9]+c[\-0-9]+b[\-0-9]+:l[\-0-9]+c[\-0-9]+b[\-0-9]+\\)$" 
  			     nil t)
            (let ((pos (buffer-substring (match-beginning 1) (match-end 1))))
              ;; display the result
              (save-current-buffer
                (ocamlspot-display-overlay source-buffer pos ocamlspot-tree-overlay))
              (message (ocamlspot-warnings))
              t)
          (progn 
            (if (re-search-forward "^\\(Error: .*\\)" nil t)
                (message (buffer-substring (match-beginning 1) (match-end 1)))
              ;; display debug info
              (message "ERROR: no tree node found there"))
            nil))))))

(defun ocamlspot-jump-to-spot (filename position)
  (if (string-match "\.cm[ioxa]$" filename)
      ;; It is not an .ml or .mli. Packed module.
      (progn 
        (message "Packed module: %s" filename)
        ;; CR jfuruse: opening a binary file is not good
        )
    (ocamlspot-display-overlay 
     (ocamlspot-find-file-existing filename)
     position ocamlspot-spot-overlay)))

(defun ocamlspot-find-type-in-buffer (&optional to-kill)
  (set-buffer (get-buffer-create ocamlspot-buffer))
  (goto-char (point-min))
  (if (re-search-forward "^Type: \\(.*\\(\n +.*\\)*\\)" nil t)
      (let ((type (buffer-substring (match-beginning 1) (match-end 1))))
	(if to-kill (kill-new type))
	(message "Type: %s" type)
	type)
    (progn 
      (message "no type found here")
      nil)))

(defun ocamlspot-find-spot-in-buffer ()
  (set-buffer (get-buffer-create ocamlspot-buffer))
  (goto-char (point-min))
  (if (re-search-forward "^Spot: \\(.*\\):\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+:l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|all\\)$" 
			 nil t)
      (let ((filename (buffer-substring (match-beginning 1) 
					(match-end 1)))
	    (position (buffer-substring (match-beginning 2)
					(match-end 2))))
	;; display the result
	(let ((type (ocamlspot-find-type-in-buffer)))
	  (ocamlspot-jump-to-spot filename position)
	  (message "Type: %s" type)))
    (progn
      (if (re-search-forward "^Spot: \\(.*\\)" nil t)
	  (message (buffer-substring (match-beginning 1) (match-end 1)))
	(if (re-search-forward "^\\(Error: .*\\)" nil t)
	    (message (buffer-substring (match-beginning 1) (match-end 1)))
	  (progn
	    ;; display debug info
	    (message "No spot found there") 
	    (ocamlspot-find-type-in-buffer)
	    ;; (display-buffer ocamlspot-buffer)
	    ))))))

(defun ocamlspot-query ()
  (interactive)
  (let ((sel-window (selected-window)))
  (save-selected-window
    (if (ocamlspot-gen-query nil)
        (progn ;save-excursion
          ;; search the result
          (ocamlspot-find-spot-in-buffer)))
    (ocamlspot-delete-overlays)) ; CR jfuruse: it depends on one's taste
  ; I dunno why but we need the following line to list-buffers work nicely
  (select-window sel-window)))

(defun ocamlspot-type (&optional to-kill)
  (interactive)
  (if (ocamlspot-gen-query '("-n"))
      (save-current-buffer 
	(ocamlspot-find-type-in-buffer to-kill)))
  (ocamlspot-delete-overlays))

(defun ocamlspot-type-and-copy ()
  (interactive)
  (ocamlspot-type t))

; CR can be shared with ocamlspot-type
(defun ocamlspot-use ()
  (interactive)
  (if (ocamlspot-gen-query '("-n"))
      (save-current-buffer 
	(set-buffer (get-buffer-create ocamlspot-buffer))
	(goto-char (point-min))
	(if (re-search-forward "^Use: \\(.*\\(\n +.*\\)*\\)" nil t)
	    (let ((type (buffer-substring (match-beginning 1) (match-end 1))))
	      (message type))
	  (message "no use information found here"))))
  (ocamlspot-delete-overlays))

(provide 'ocamlspot)
