;;; zimpl-mode.el --- Major mode for editing ZIMPL programs

(defconst zi-version "$Revision: 1.00 $"
  "`zimpl-mode' version number.")

;;; Code:

(require 'comint)
(require 'custom)
(require 'cl)
(require 'compile)

;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defgroup zimpl nil
  "Support for the ZIMPL programming language, <http://zimpl.zib.de/>"
  :group 'languages
  :prefix "zi-")

(defcustom zi-block-comment-prefix "##"
  "*String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
`...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'zimpl)

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defconst zi-emacs-features
  (let (features)
   features)
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, with different levels of
support for features needed by `zimpl-mode'.")

;; Face for None, True, False, self, and Ellipsis
(defvar zi-pseudo-keyword-face 'zi-pseudo-keyword-face
  "Face for pseudo keywords in ZIMPL mode, like self, True, False, Ellipsis.")
(make-face 'zi-pseudo-keyword-face)

;; Face for builtins
(defvar zi-builtins-face 'zi-builtins-face
  "Face for builtins like TypeError, object, open, and exec.")
(make-face 'zi-builtins-face)

(defun zi-font-lock-mode-hook ()
  (or (face-differs-from-default-p 'zi-pseudo-keyword-face)
      (copy-face 'font-lock-keyword-face 'zi-pseudo-keyword-face))
  (or (face-differs-from-default-p 'zi-builtins-face)
      (copy-face 'font-lock-keyword-face 'zi-builtins-face))
  )
(add-hook 'font-lock-mode-hook 'zi-font-lock-mode-hook)

(defvar zimpl-font-lock-keywords
  (let ((kw1 (mapconcat 'identity
			'("and" "as" "default" "do" "end" "else" "forall" "if" "in"
			  "infinity" "or" "then" "to" "with" "without" "xor" 
			  "abs" "argmax" "argmin" "binary" "card" "ceil" "cross" "comment" "exp" "floor" 
			  "implicit" "indexsets" "integer" "inter" "length" "ln" "log"
			  "max" "min" "mod" "ord" "print" "priority" "powerset" "prod" "proj" "random" 
			  "read" "real" "sgn" "skip" "sqrt" "startval" "subsets" "sum" "symdiff" "union" "use"
			  "vabs" "vif"
			  )
			"\\|"))
	(kw3 (mapconcat 'identity
			;; Don't include True, False, None, or
			;; Ellipsis in this list, since they are
			;; already defined as pseudo keywords.
			'("maximize" "minimize" "param" "set" "subto" "var" )
			"\\|"))
	)
    (list
     ;; keywords
     (cons (concat "\\b\\(" kw1 "\\)\\b[ \n\t(]") 1)
     ;; builtins when they don't appear as object attributes
     (list (concat "\\([^. \t]\\|^\\)[ \t]*\\b\\(" kw3 "\\)\\b[ \n\t(]") 2
	   'zi-builtins-face)
     ;; block introducing keywords with immediately following colons.
     ;; Yes "except" is in both lists.
     '("[ \t]*\\(\\bfrom\\b.*\\)?\\bimport\\b.*\\b\\(as\\)\\b" . 2)
     ;; classes
     '("\\bclass[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-type-face)
     ;; functions
     '("\\bdef[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-function-name-face)
     ;; pseudo-keywords
     '("\\b\\(self\\|None\\|True\\|False\\|Ellipsis\\)\\b"
       1 zi-pseudo-keyword-face)
     ))
  "Additional expressions to highlight in ZIMPL mode.")
(put 'zimpl-mode 'font-lock-defaults '(zimpl-font-lock-keywords))

;; Constants

(defconst zi-stringlit-re
  (concat
   ;; These fail if backslash-quote ends the string (not worth
   ;; fixing?).  They precede the short versions so that the first two
   ;; quotes don't look like an empty short string.
   ;;
   ;; (maybe raw), long single quoted triple quoted strings (SQTQ),
   ;; with potential embedded single quotes
   "[rR]?'''[^']*\\(\\('[^']\\|''[^']\\)[^']*\\)*'''"
   "\\|"
   ;; (maybe raw), long double quoted triple quoted strings (DQTQ),
   ;; with potential embedded double quotes
   "[rR]?\"\"\"[^\"]*\\(\\(\"[^\"]\\|\"\"[^\"]\\)[^\"]*\\)*\"\"\""
   "\\|"
   "[rR]?'\\([^'\n\\]\\|\\\\.\\)*'"	; single-quoted
   "\\|"				; or
   "[rR]?\"\\([^\"\n\\]\\|\\\\.\\)*\""	; double-quoted
   )
  "Regular expression matching a ZIMPL string literal.")

(defconst zi-continued-re
  ;; This is tricky because a trailing backslash does not mean
  ;; continuation if it's in a comment
  (concat
   "\\(" "[^#'\"\n\\]" "\\|" zi-stringlit-re "\\)*"
   "\\\\$")
  "Regular expression matching ZIMPL backslash continuation lines.")
  
(defconst zi-blank-or-comment-re "[ \t]*\\($\\|#\\)"
  "Regular expression matching a blank or comment line.")

;; Major mode boilerplate

;; define a mode-specific abbrev table for those who use such things
(defvar zimpl-mode-abbrev-table nil
  "Abbrev table in use in `zimpl-mode' buffers.")
(define-abbrev-table 'zimpl-mode-abbrev-table nil)

(defvar zimpl-mode-hook nil
  "*Hook called by `zimpl-mode'.")

(defvar zi-mode-map ()
  "Keymap used in `zimpl-mode' buffers.")
(if zi-mode-map
    nil
  (setq zi-mode-map (make-sparse-keymap))
  ;; electric keys
  (define-key zi-mode-map "\C-c#"     'zi-comment-region)
  )

(defvar zi-mode-syntax-table nil
  "Syntax table used in `zimpl-mode' buffers.")
(when (not zi-mode-syntax-table)
  (setq zi-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" zi-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" zi-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" zi-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" zi-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" zi-mode-syntax-table)
  (modify-syntax-entry ?\} "){" zi-mode-syntax-table)
  ;; Add operator symbols misassigned in the std table
  (modify-syntax-entry ?\$ "."  zi-mode-syntax-table)
  (modify-syntax-entry ?\% "."  zi-mode-syntax-table)
  (modify-syntax-entry ?\& "."  zi-mode-syntax-table)
  (modify-syntax-entry ?\* "."  zi-mode-syntax-table)
  (modify-syntax-entry ?\+ "."  zi-mode-syntax-table)
  (modify-syntax-entry ?\- "."  zi-mode-syntax-table)
  (modify-syntax-entry ?\/ "."  zi-mode-syntax-table)
  (modify-syntax-entry ?\< "."  zi-mode-syntax-table)
  (modify-syntax-entry ?\= "."  zi-mode-syntax-table)
  (modify-syntax-entry ?\> "."  zi-mode-syntax-table)
  (modify-syntax-entry ?\| "."  zi-mode-syntax-table)
  ;; For historical reasons, underscore is word class instead of
  ;; symbol class.  GNU conventions say it should be symbol class, but
  ;; there's a natural conflict between what major mode authors want
  ;; and what users expect from `forward-word' and `backward-word'.
  ;; Guido and I have hashed this out and have decided to keep
  ;; underscore in word class.  If you're tempted to change it, try
  ;; binding M-f and M-b to zi-forward-into-nomenclature and
  ;; zi-backward-into-nomenclature instead.  This doesn't help in all
  ;; situations where you'd want the different behavior
  ;; (e.g. backward-kill-word).
  (modify-syntax-entry ?\_ "w"  zi-mode-syntax-table)
  ;; Both single quote and double quote are string delimiters
  (modify-syntax-entry ?\' "\"" zi-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" zi-mode-syntax-table)
  ;; backquote is open and close paren
  (modify-syntax-entry ?\` "$"  zi-mode-syntax-table)
  ;; comment delimiters
  (modify-syntax-entry ?\# "<"  zi-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  zi-mode-syntax-table)
  )

;; An auxiliary syntax table which places underscore and dot in the
;; symbol class for simplicity
(defvar zi-dotted-expression-syntax-table nil
  "Syntax table used to identify ZIMPL dotted expressions.")
(when (not zi-dotted-expression-syntax-table)
  (setq zi-dotted-expression-syntax-table
	(copy-syntax-table zi-mode-syntax-table))
  (modify-syntax-entry ?_ "_" zi-dotted-expression-syntax-table)
  (modify-syntax-entry ?. "_" zi-dotted-expression-syntax-table))



;; Utilities
(defmacro zi-safe (&rest body)
  "Safely execute BODY, return nil if an error occurred."
  (` (condition-case nil
	 (progn (,@ body))
       (error nil))))

(defsubst zi-keep-region-active ()
  "Keep the region active in XEmacs."
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs 19 does it differently; its policy doesn't require us
  ;; to take explicit action.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defsubst zi-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol  -- beginning of line
  eol  -- end of line
  bod  -- beginning of def or class
  eod  -- end of def or class
  bob  -- beginning of buffer
  eob  -- end of buffer
  boi  -- back to indentation
  bos  -- beginning of statement

This function does not modify point or mark."
  (let ((here (point)))
    (cond
     ((eq position 'bol) (beginning-of-line))
     ((eq position 'eol) (end-of-line))
     ((eq position 'bod) (zi-beginning-of-def-or-class 'either))
     ((eq position 'eod) (zi-end-of-def-or-class 'either))
     ;; Kind of funny, I know, but useful for zi-up-exception.
     ((eq position 'bob) (beginning-of-buffer))
     ((eq position 'eob) (end-of-buffer))
     ((eq position 'boi) (back-to-indentation))
     ((eq position 'bos) (zi-goto-initial-line))
     (t (error "Unknown buffer position requested: %s" position))
     )
    (prog1
	(point)
      (goto-char here))))

(defsubst zi-highlight-line (from to file line)
  (cond
   ((fboundp 'make-extent)
    ;; XEmacs
    (let ((e (make-extent from to)))
      (set-extent-property e 'mouse-face 'highlight)
      (set-extent-property e 'zi-exc-info (cons file line))
      (set-extent-property e 'keymap zi-mode-output-map)))
   (t
    ;; Emacs -- Please port this!
    )
   ))

(defun zi-in-literal (&optional lim)
  "Return non-nil if point is in a ZIMPL literal (a comment or string).
Optional argument LIM indicates the beginning of the containing form,
i.e. the limit on how far back to scan."
  ;; This is the version used for non-XEmacs, which has a nicer
  ;; interface.
  ;;
  ;; WARNING: Watch out for infinite recursion.
  (let* ((lim (or lim (zi-point 'bod)))
	 (state (parse-partial-sexp lim (point))))
    (cond
     ((nth 3 state) 'string)
     ((nth 4 state) 'comment)
     (t nil))))

;; XEmacs has a built-in function that should make this much quicker.
;; In this case, lim is ignored
(defun zi-fast-in-literal (&optional lim)
  "Fast version of `zi-in-literal', used only by XEmacs.
Optional LIM is ignored."
  ;; don't have to worry about context == 'block-comment
  (buffer-syntactic-context))

(if (fboundp 'buffer-syntactic-context)
    (defalias 'zi-in-literal 'zi-fast-in-literal))



;; Menu definitions, only relevent if you have the easymenu.el package
;; (standard in the latest Emacs 19 and XEmacs 19 distributions).
(defvar zi-menu nil
  "Menu for ZIMPL Mode.
This menu will get created automatically if you have the `easymenu'
package.  Note that the latest X/Emacs releases contain this package.")

(and (zi-safe (require 'easymenu) t)
     (easy-menu-define
      zi-menu zi-mode-map "ZIMPL Mode menu"
      '("ZIMPL"
	["Comment Out Region"   zi-comment-region  (mark)]
	["Uncomment Region"     (zi-comment-region (point) (mark) '(4)) (mark)]
	)))

;;;###autoload
(defun zimpl-mode ()
  (interactive)
  ;; set up local variables
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'add-log-current-defun-function)
  (make-local-variable 'fill-paragraph-function)
  ;;
  (set-syntax-table zi-mode-syntax-table)
  (setq major-mode              'zimpl-mode
	mode-name               "ZIMPL"
	local-abbrev-table      zimpl-mode-abbrev-table
	font-lock-defaults      '(zimpl-font-lock-keywords)
	paragraph-separate      "^[ \t]*$"
	paragraph-start         "^[ \t]*$"
	require-final-newline   t
	comment-start           "# "
	comment-end             ""
	comment-start-skip      "# *"
	comment-column          40
	)
  (use-local-map zi-mode-map)
  ;; add the menu
  (if zi-menu
      (easy-menu-add zi-menu))
  ;; Emacs 19 requires this
  (if (boundp 'comment-multi-line)
      (setq comment-multi-line nil))
  ;; Run the mode hook.  Note that zi-mode-hook is deprecated.
  (if zimpl-mode-hook
      (run-hooks 'zimpl-mode-hook)
    (run-hooks 'zi-mode-hook))
  ;; Now do the automagical guessing
  ;; Set the default shell if not already set
  )


;;;###autoload
(when (not (or (rassq 'zimpl-mode auto-mode-alist)))
  (push '("\\.zpl$" . zimpl-mode) auto-mode-alist))

;;; Subprocess commands

(defun zi-comment-region (beg end &optional arg)
  "Like `comment-region' but uses double hash (`#') comment starter."
  (interactive "r\nP")
  (let ((comment-start zi-block-comment-prefix))
    (comment-region beg end arg)))

(defun zi-version ()
  "Echo the current version of `zimpl-mode' in the minibuffer."
  (interactive)
  (message "Using `zimpl-mode' version %s" zi-version)
  (zi-keep-region-active))

(provide 'zimpl-mode)
;;; zimpl-mode.el ends here
