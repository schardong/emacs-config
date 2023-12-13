(use-package latex
  :ensure auctex
  :defer t
  ;:mode ("\\.tex\\'" . latex-mode)
  :config
  (setq-default TeX-master nil
                TeX-auto-save t
                TeX-save-query nil
                TeX-parse-self t
                reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-PDF-mode t)
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t)))
  :hook
  (LaTeX-mode . TeX-PDF-mode)
  (LaTeX-mode . company-mode)
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . flycheck-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . turn-on-cdlatex)
  (LaTeX-mode . rainbow-delimiters)
  (LaTeX-mode . smartparens)
  (LaTeX-mode . reftex-isearch-minor))

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
           #'TeX-revert-document-buffer)

;; to use pdfview with auctex
(add-hook 'LaTeX-mode-hook 'pdf-tools-install)

;; to use pdfview with auctex
(setq TeX-view-program-selection '((output-pdf "pdf-tools"))
       TeX-source-correlate-start-server t)
(setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))

(use-package reftex
  :ensure t
  :after (auctex)
  :commands turn-on-reftex
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-cite-prompt-optional-args t))

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :bind ("C-c C-g" . pdf-sync-forward-search)
  :defer t
  :config
  (setq mouse-wheel-follow-mouse t
        pdf-view-resize-factor 1.10))
