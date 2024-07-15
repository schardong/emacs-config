(add-hook 'latex-mode-hook 'reftex-mode)

(use-package tex
  :ensure auctex
  :defer 4
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t
        reftex-cite-format 'bibtex
        reftex-plug-into-AUCTeX t)
  (setq-default TeX-master nil
                TeX-command-extra-options "-shell-escape")
  :hook
  (LaTeX-mode . reftex-mode)
  ;; (LaTeX-mode . TeX-PDF-mode)
  ;; (LaTeX-mode . company-mode)
  ;; (LaTeX-mode . flyspell-mode)
  ;; (LaTeX-mode . flycheck-mode)
  ;; (LaTeX-mode . LaTeX-math-mode)
  ;; (LaTeX-mode . rainbow-delimiters)
  ;; (LaTeX-mode . smartparens)
  ;; (LaTeX-mode . reftex-isearch-minor)
  )

(use-package reftex
  :ensure t
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-enable-partial-scanning t))

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :bind ("C-c C-g" . pdf-sync-forward-search)
  :defer t
  :config
  (setq mouse-wheel-follow-mouse t
        pdf-view-resize-factor 1.10))
