(use-package slime
  :ensure t
  :defer t
  :init
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy slime-quicklisp slime-asdf)
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-net-coding-system 'utf-8-unix)
  :config
  (setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/HyperSpec/"
        common-lisp-hyperspec-symbol-table (concat common-lisp-hyperspec-root "Data/Map_Sym.txt")
        common-lisp-hyperspec-issuex-table (concat common-lisp-hyperspec-root "Data/Map_IssX.txt"))
  :mode (("\\.lisp\\'" . slime-mode)
         ("\\.lisp\\'" . lisp-mode)))

(load (expand-file-name "~/quicklisp/slime-helper.el"))
