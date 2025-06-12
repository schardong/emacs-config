(use-package tex
  :ensure auctex
  :defer 4
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t
        reftex-cite-format 'bibtex
        reftex-plug-into-AUCTeX t)
  (setq-default TeX-master 'shared
                TeX-command-extra-options "-shell-escape")
  :hook
  (LaTeX-mode . reftex-mode))

(use-package reftex
  :ensure t
  :after (auctex)
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-default-bibliography '("~/Documents/zotero-lib.bib")
        reftex-enable-partial-scanning t))
