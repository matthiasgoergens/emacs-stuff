;; http://www.emacswiki.org/emacs/FlymakeOcaml
(require 'flymake)

(defun flymake-ocaml-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-ocaml-cmdline))

(defun flymake-get-ocaml-cmdline (source base-dir)
  (list "ocaml_flycheck"
        (list source base-dir)))

(push '(".+\\.ml[yilp]?$" flymake-ocaml-init flymake-simple-java-cleanup)
      flymake-allowed-file-name-masks)
(push
 '("^\\(\.+\.ml[yilp]?\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
   1 2 3 4) flymake-err-line-patterns)

;; optional setting
;; if you want to use flymake always, then add the following hook.
;; (add-hook 'tuareg-mode-hook
;;           '(lambda ()
;;              (if (not (null buffer-file-name)) (flymake-mode))))
