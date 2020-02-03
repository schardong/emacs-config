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

(setq *misc-packages* '(auctex ess lua-mode plan9-theme cmake-mode exec-path-from-shell magit js2-mode js2-refactor xref-js2 markdown-mode markdown-mode+ use-package))

(setq *my-packages* (append *python-pkgs* *julia-pkgs* *misc-packages*))

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
;; (use-package json-mode)

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
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS2-mode
(use-package js2-mode
  :ensure nil
  :mode ("\\.js\\'" . js2-mode)
  :hook ((js2-mode-hook . js2-imenu-extras-mode)
         (js2-mode-hook . js2-refactor-mode)))

;;(require 'js2-mode)
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
;;(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
;;(add-hook 'js2-mode-hook #'js2-refactor-mode)
;;(js2r-add-keybindings-with-prefix "C-c C-r")
;;(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
;;(define-key js-mode-map (kbd "M-.") nil)

;;(add-hook 'js2-mode-hook (lambda ()
;;  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (plan9)))
 '(custom-safe-themes
   (quote
    ("30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c" "c9e02bc73b027c25da6d5d3eee642f7892bb409a32acecd2ae8c7b5df52c068f" "ef1e992ef341e86397b39ee6b41c1368e1b33d45b0848feac6a8e8d5753daa67" default)))
 '(elpy-rpc-python-command "/Users/gschardong/.pyenv/shims/python3")
 '(elpy-syntax-check-command "/usr/local/bin/flake8")
 '(elpy-test-discover-runner-command (quote ("python3" "-m" "unittest")))
 '(fci-rule-color "#f8fce8")
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#40883f" "#0287c8" "#b85c57")))
 '(org-agenda-files
   (quote
    ("~/Google Drive/org/home.org" "~/Google Drive/org/research.org" "~/Google Drive/org/simpad.org")))
 '(package-selected-packages
   (quote
    (pip-requirements magit-popup py-isort pyenv-mode pyenv-mode-auto python-docstring company-auctex company-bibtex company-lua company-jedi json-mode markdown-mode+ js2-refactor glsl-mode go-autocomplete go-direx go-mode ob-ipython org-babel-eval-in-repl nil flycheck-julia julia-shell ein-mumamo plan9-theme flycheck-swift swift-mode sphinx-doc sphinx-frontend sphinx-mode flymake-python-pyflakes importmagic cmake-mode py-autopep8 py-import-check magithub)))
 '(python-flymake-command (quote ("\"flake8\" \"-\"")))
 '(python-shell-interpreter "/Users/gschardong/.pyenv/shims/python3")
 '(safe-local-variable-values (quote ((TeX-master . thesis\.tex))))
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
