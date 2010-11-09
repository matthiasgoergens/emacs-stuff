(set-background-color "black")
(set-foreground-color "green")
(server-start)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex")
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-command "pdftex")
 '(TeX-output-view-style (quote (("^pdf$" "." "evince %o %(outpage)") ("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^dvi$" ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$") "%(o?)xdvi %dS -paper a4r -s 0 %d") ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d") ("^dvi$" ("^a5\\(?:comb\\|paper\\)$" "^landscape$") "%(o?)xdvi %dS -paper a5r -s 0 %d") ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^dvi$" "." "%(o?)xdvi %dS %d") ("^html?$" "." "netscape %o"))))
 '(TeX-parse-self t)
 '(auto-revert-interval 1)
 '(completion-auto-help (quote lazy))
 '(contentswitch-max-files-from-history 60)
 '(default-input-method "german-postfix")
 '(enable-recursive-minibuffers t)
 '(exec-path (quote ("~/bin" "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/usr/games" "/usr/lib/emacs/23.1/x86_64-linux-gnu")))
 '(global-whitespace-mode nil)
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
 '(mouse-autoselect-window t)
 '(org-read-date-prefer-future nil)
 '(pop-up-frames nil)
 '(preview-auto-cache-preamble t)
 '(preview-preserve-counters t)
 '(tab-width 4)
 '(tuareg-comment-end-extra-indent 1)
 '(tuareg-default-indent 4)
 '(tuareg-electric-indent t)
 '(tuareg-in-indent 0)
 '(tuareg-indent-comments t)
 '(tuareg-indent-leading-comments t)
 '(tuareg-lazy-= t)
 '(tuareg-lazy-paren t)
 '(tuareg-leading-star-in-doc t)
 '(tuareg-sig-struct-align t)
 '(tuareg-sig-struct-indent 4)
 '(tuareg-use-abbrev-mode nil)
 '(whitespace-action nil)
 '(whitespace-line-column 80))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "#559944"))))
 '(diff-context ((t nil)))
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-function ((t (:foreground "#00bbdd"))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-hunk-header ((t (:foreground "#fbde2d"))))
 '(diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))))
 '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042"))))
 '(diff-removed ((t (:foreground "#de1923")))))

; may be interesting:



;(load-file "/home/matthias/emacs/graphviz-dot-mode.el")
;(load-file "~/.emacs.d/elisp/zimpl-mode.el")
(load-file "~/.emacs.d/elisp/contentswitch.el")

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	(let* ((my-lisp-dir "~/.emacs.d/elisp/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;(tex-pdf-mode t)



;(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) ;turn on pdf-mode.  AUCTeX
;                                         ;will call pdflatex to
;                                          ;compile instead of latex.

;(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode) ;turn on math-mode by
;                                             ;default

;(add-hook 'LaTeX-mode-hook 'reftex-mode) ;turn on REFTeX mode by
;                                         ;default

(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(defun pdfevince ()
   (add-to-list 'TeX-output-view-style
                    (quote ("^pdf$" "." "evince %o %(outpage)")))
)

(add-hook  'LaTeX-mode-hook  'pdfevince  t) ; AUCTeX LaTeX mode
(add-hook  'TeX-mode-hook  'pdfevince  t) ; AUCTeX LaTeX mode

(defun count nil "wc on buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc"))



(show-paren-mode 1)
(scroll-bar-mode)
(menu-bar-mode 0)
(tool-bar-mode 0)
(column-number-mode t)
(setq visible-bell t)
(toggle-input-method)


(setq tuareg-in-indent 0)

(put 'narrow-to-region 'disabled nil)

;                   ; set the path of the ocamlspot binary
(setq ocamlspot-path "/usr/local/bin/ocamlspot")

;                   ; autoload
;(autoload 'ocamlspot-query "ocamlspot" "OCamlSpot")

;                   ; tuareg mode hook (use caml-mode-hook instead if you use caml-mode)
;(add-hook 'tuareg-mode-hook
;          '(lambda ()
;            (local-s;et-key "\C-c;" 'ocamlspot-query)
;            (local-set-key "\C-c\C-t" 'ocamlspot-type)
;             (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
;            (local-set-key "\C-c\C-u" 'ocamlspot-use)
;            (local-set-key "\C-ct" 'caml-types-show-type)))

;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))


(global-set-key (kbd "C-x C-b") 'contentswitch)
;(global-set-key (kbd "C-c C-b") 'switch-to-buffer)

(add-to-list 'load-path "~/.emacs.d/elisp/tuareg-mode")
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
  "Configuration of imenu for tuareg" t)
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

;(load-file "~/elisp/tuareg-mode/tuareg.el")

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)