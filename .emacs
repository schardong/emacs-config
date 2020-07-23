;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package managers and installing use-package if needed.
(setq package-enable-at-startup nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installing all packages

(setq *python-pkgs* '(elpy pip-requirements py-isort pyenv-mode py-autopep8 py-import-check sphinx-doc sphinx-frontend sphinx-mode flymake-python-pyflakes importmagic jedi))

(setq *julia-pkgs* '(flycheck-julia julia-mode julia-repl julia-shell))

(setq *js-pkgs* '(js2-mode js2-refactor xref-js2))

(setq *misc-packages* '(auctex ess lua-mode plan9-theme cmake-mode exec-path-from-shell magit markdown-mode markdown-mode+ use-package org-bullets))

(setq *my-packages* (append *python-pkgs* *julia-pkgs* *js-packages* *misc-packages*))

(package-refresh-contents)

(defun process-pkg (p)
  "Installs a package if not already installed."
  (if (not (null p))
      (unless (package-installed-p p)
    (package-install p)))
  (package-installed-p p))

(mapcar 'process-pkg *my-packages*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc. options
(setq visible-bell t)
(display-time)
(when (version<= "26.1" emacs-version)
  (global-display-line-numbers-mode))

;; (tool-bar-mode -1)
;; (menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode +1)

(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

(setq-default line-spacing 3)
(setq-default indent-tabs-mode nil
              tab-width 4)

(load-theme 'plan9 t)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better window splitting
(use-package "window"
  :ensure nil
  :config
  (defun ggs/split-and-follow-horizontally ()
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun ggs/split-and-follow-vertically ()
    (interactive)
    (split-window-right)
    (other-window 1))
  (global-set-key (kbd "C-x 2") 'ggs/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") 'ggs/split-and-follow-vertically))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autorevert buffers changed outside emacs
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        auto-revert-verbose nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use ELDOC only in prog-mode
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (global-eldoc-mode -1)
  (add-hook 'prog-mode-hook 'eldoc-mode)
  (setq eldoc-idle-delay 0.4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paren mode and autocomplete parens
(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0)
  (show-paren-mode))

(use-package elec-pair
  :ensure nil
  :config
  (add-hook 'prog-mode-hook 'electric-pair-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove whitespaces and empty lines
(use-package whitespace
  :ensure nil
  :config (add-hook 'before-save-hook 'whitespace-cleanup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
(use-package magit
  :bind
  ("C-x g s" . magit-status)
  ("C-x g m" . magit-merge)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push))

(use-package magit-popup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE
(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-<tab>") nil)
    (define-key org-mode-map "C-c l" 'org-store-link)
    (define-key org-mode-map "C-c a" 'org-agenda))
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))
  (setq org-log-done t)
  (setq org-agenda-files (list
                          "~/Google Drive/org/home.org"
                          "~/Google Drive/org/research.org"
                          "~/Google Drive/org/simpad.org"
                          "~/Google Drive/org/d2s.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; READING PATH FROM SHELL
(use-package exec-path-from-shell
  :config
  (when (or (memq window-system '(mac ns x))
            (memq window-system 'ns))
    (exec-path-from-shell-initialize)))

(use-package markdown-mode :hook (markdown-mode . visual-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python and ELPY
(use-package python
  :defer 10
  :hook python-mode-hook
  :init (setq-default indent-tabs-mode nil)
  :mode
  ("\\.py\\'" . python-mode)
  ("\\.wsgi$" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq python-indent-offset 4))

(use-package elpy
  :after (company python)
  :init (elpy-enable)
  :config
  (setq elpy-rpc-backend "jedi")
  (setq py-autopep8-options '("--ignore E402"))
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  (add-hook 'elpy-mode-hook 'hs-minor-mode))

(use-package pyenv-mode
  :if
  (executable-find "pyenv")
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sort python import statements before saving the buffer
(use-package py-isort
  :ensure nil
  :config (add-hook 'before-save-hook 'py-isort-before-save))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUCTEX
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (rainbow-delimiters-mode)
              (company-mode)
              (smartparens-mode)
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (reftex-isearch-minor-mode)
              (setq TeX-PDF-mode t)
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t))))

;; Update PDF buffers after successful LaTeX runs
;;(add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
;;           #'TeX-revert-document-buffer)

;; to use pdfview with auctex
;;(add-hook 'LaTeX-mode-hook 'pdf-tools-install)

;; to use pdfview with auctex
;;(setq TeX-view-program-selection '((output-pdf "pdf-tools"))
;;       TeX-source-correlate-start-server t)
;;(setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESS
(require 'ess-site)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lua-mode
(use-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode))

;; (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS2-mode
(use-package js2-mode
  :ensure nil
  :mode ("\\.js\\'" . js2-mode)
  :hook ((js2-mode-hook . js2-imenu-extras-mode)
         (js2-mode-hook . js2-refactor-mode)))
