(add-hook 'latex-mode-hook 'reftex-mode)

(use-package tex
  :ensure auctex
  :defer 4
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t
        TeX-view-program-list '((Okular "okular" "okular %b"))
        TeX-view-program-selection '((output-pdf "Okular"))
        reftex-cite-format 'bibtex
        reftex-plug-into-AUCTeX t))
  (setq-default TeX-master 'shared
                TeX-command-extra-options "-shell-escape")
  :hook
  (LaTeX-mode . reftex-mode)

(use-package reftex
  :ensure t
  :after (auctex)
  :config
  (setq reftex-plug-into-AUCTeX t
        ;; reftex-default-bibliography '("~/my-bib-file.bib")
        reftex-enable-partial-scanning t))


;;(use-package tex-site
;;  :ensure auctex
;;  :defer t
;;  :mode ("\\.tex\\'" . latex-mode))
  ;; :config
  ;; (setq-default TeX-master nil
  ;;               TeX-auto-save t
  ;;               TeX-save-query nil
  ;;               TeX-parse-self t
  ;;               reftex-plug-into-AUCTeX t)
  ;; (add-hook 'LaTeX-mode-hook
  ;;           (lambda ()
  ;;             (setq TeX-PDF-mode t)
  ;;             (setq TeX-source-correlate-method 'synctex)
  ;;             (setq TeX-source-correlate-start-server t)))
  ;; :hook
  ;; (LaTeX-mode . TeX-PDF-mode)
  ;; (LaTeX-mode . company-mode)
  ;; (LaTeX-mode . flyspell-mode)
  ;; (LaTeX-mode . flycheck-mode)
  ;; (LaTeX-mode . LaTeX-math-mode)
  ;; (LaTeX-mode . rainbow-delimiters)
  ;; (LaTeX-mode . smartparens)
  ;; (LaTeX-mode . reftex-isearch-minor))

;; (use-package pdf-tools
;;   :ensure t
;;   :mode ("\\.pdf\\'" . pdf-tools-install)
;;   :bind ("C-c C-g" . pdf-sync-forward-search)
;;   :defer t
;;   :config
;;   (setq mouse-wheel-follow-mouse t
;;         pdf-view-resize-factor 1.10))
