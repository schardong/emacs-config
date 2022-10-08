(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-refactor-mode)))
