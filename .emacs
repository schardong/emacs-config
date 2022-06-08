;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package managers and installing use-package if needed.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Configure and bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installing all packages
(setq *cpp-pkgs* '(clang-format cmake-mode))

(setq *go-pkgs* '(company-go flycheck-golangci-lint go-mode go-scratch go-snippets))

(setq *python-pkgs* '(elpy pip-requirements py-isort pyenv-mode py-autopep8 py-import-check flymake-python-pyflakes importmagic jedi ein))

(setq *julia-pkgs* '(flycheck-julia julia-mode julia-repl julia-shell))

(setq *js-pkgs* '(js2-mode js2-refactor xref-js2))

(setq *lisp-pkgs* '(slime slime-company))

(setq *docker-pkgs* '(dockerfile-mode docker-compose-mode))

(setq *misc-pkgs* '(auctex plan9-theme eink-theme exec-path-from-shell graphviz-dot-mode magit markdown-mode org-bullets yaml-mode))

(setq *my-pkgs* (append *cpp-pkgs* *python-pkgs* *misc-pkgs*))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'org)
  (package-install 'org))

(defun process-pkg (p)
  "Installs a package if not already installed."
  (if (not (null p))
      (unless (package-installed-p p)
    (package-install p)))
  (package-installed-p p))

(mapcar 'process-pkg *my-pkgs*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc. options
(setq visible-bell t)
(display-time)
(when (version<= "26.1" emacs-version)
  (global-display-line-numbers-mode))

(unless (display-graphic-p)
  (menu-bar-mode -1))
(when (display-graphic-p)
  (load-theme 'plan9 t))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode +1)

(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

(setq-default line-spacing 3)
(setq-default indent-tabs-mode nil
              tab-width 4)

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
  :hook (prog-mode . eldoc-mode)
  :config
  (global-eldoc-mode -1)
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
  :hook (prog-mode . electric-pair-mode))

(use-package company
  :ensure t
  :config (global-company-mode)
  :diminish company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove whitespaces and empty lines
(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
(use-package magit
  :ensure t
  :bind
  ("C-x g s" . magit-status)
  ("C-x g m" . magit-merge)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . flyspell-mode)
         (org-mode . yas-minor-mode))
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-<tab>") nil)
    (define-key org-mode-map "C-cl" 'org-store-link)
    (define-key org-mode-map "C-ca" 'org-agenda))
  (use-package org-bullets
    :ensure t
    :hook (org-mode . org-bullets-mode))
  (setq org-log-done t
        org-agenda-files (file-expand-wildcards "~/Documents/org/*.org")
        org-todo-keywords '((sequence "TODO"
                                      "WAITING"
                                      "|"
                                      "DEFERRED"
                                      "CANCELLED"
                                      "DONE"))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-v2-ack t)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; READING PATH FROM SHELL
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x))
          (memq window-system 'ns))
  :ensure t
  :config
    (exec-path-from-shell-initialize))

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . visual-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ packages and configs
(setq-default c-default-style "k&r"
              c-basic-offset 4
              indent-tabs-mode nil
              tab-width 4)

(defun cleanup-c-buffer ()
  "Correctly indent, remove tabs and extra whitespace in C source code"
  (interactive)
  (c-indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (whitespace-cleanup-region (point-min) (point-max)))

(use-package cc-mode
  :bind
  (:map c++-mode-map
        ("C-c c d" . clang-format-defun)
        ("C-c c r" . clang-format-region)))

(use-package clang-format
  :ensure t
  :after cc-mode
  :init
  (defun clang-format-defun ()
    (interactive)
    (save-excursion
      (mark-defun)
      (clang-format-region (region-beginning) (region-end))
      (deactivate-mark))))

(defun cppreference-query ()
  "Searches cppreference"
  (interactive)
  (browse-url
   (concat
    "https://en.cppreference.com/mwiki/index.php?title=Special:Search&search="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "cppreference: ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python and ELPY
(use-package python
  :ensure t
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
  :ensure t
  :after (company python)
  :init (elpy-enable)
  :config
  (setq elpy-rpc-backend "jedi"
        py-autopep8-options '("--ignore E402"))
  :hook
  (elpy-mode . py-autopep8-enable-on-save)
  (elpy-mode . hs-minor-mode))

(use-package pyenv-mode
  :ensure t
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
;; AUCTEX
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq-default TeX-master nil
                TeX-auto-save t
                TeX-save-query nil
                TeX-parse-self t)
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
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t))

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :bind ("C-c C-g" . pdf-sync-forward-search)
  :defer t
  :config
  (setq mouse-wheel-follow-mouse t
        pdf-view-resize-factor 1.10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lua-mode
(use-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS2-mode
;; (use-package js2-mode
;;   :ensure nil
;;   :mode ("\\.js\\'" . js2-mode)
;;   :hook ((js2-mode . js2-imenu-extras-mode)
;;          (js2-mode . js2-refactor-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP and SLIME
;; (use-package slime
;;   :ensure t
;;   :defer t
;;   :init
;;   (setq inferior-lisp-program "sbcl"
;;         slime-contribs '(slime-fancy slime-quicklisp slime-asdf)
;;         slime-complete-symbol-function 'slime-fuzzy-complete-symbol
;;         slime-net-coding-system 'utf-8-unix
;;         slime-lisp-implementations '((sbcl ("/usr/bin/sbcl"))))
;;   :config
;;   (setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/HyperSpec/"
;;         common-lisp-hyperspec-symbol-table (concat common-lisp-hyperspec-root "Data/Map_Sym.txt")
;;         common-lisp-hyperspec-issuex-table (concat common-lisp-hyperspec-root "Data/Map_IssX.txt"))
;;   :mode (("\\.lisp\\'" . slime-mode)
;;          ("\\.lisp\\'" . lisp-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Docker
;; (use-package docker
;;   :ensure t
;;   :bind ("C-c d" . docker))
