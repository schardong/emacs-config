;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package managers and installing use-package if needed.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Configure and bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installing all packages
(setq *cpp-pkgs* '(clang-format cmake-mode eldoc project xref eglot))

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

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(unless (display-graphic-p)
  (menu-bar-mode -1))
(when (display-graphic-p)
  (load-theme 'plan9 t))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode +1)

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(use-package which-key
  :ensure t
  :config
  (add-hook 'after-init-hook 'which-key-mode))

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1))

(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

(setq-default line-spacing 3)
(setq-default indent-tabs-mode nil
              tab-width 4)

;; key bindings for macos (from https://www.emacswiki.org/emacs/EmacsForMacOS#h5o-24)
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))

;; Binding modifier keys
;; The variables available for binding the modifier keys:

;; mac-function-modifier
;; mac-control-modifier
;; mac-command-modifier
;; mac-option-modifier
;; mac-right-command-modifier
;; mac-right-control-modifier
;; mac-right-option-modifier

;; values can be 'control, 'alt, 'meta, 'super, 'hyper, nil (setting to nil allows the OS to assign values)
;; Only y/n answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)
;; Sort Dired buffers
(setq dired-listing-switches "-agho --group-directories-first")

(setq dired-guess-shell-alist-user `(("\\.mp4\\'" "mpv")))

(defun w/topic-butterfly (topic)
  (interactive "Mtopic: ")
  (progn
    (switch-to-buffer (get-buffer-create "*topic*"))
    (erase-buffer)
    (sit-for 0)
    (animate-string topic
                    (- (/ (window-height) 2) 5)
                    (- (/ (window-width) 2)
                       (/ (length topic) 2)))
    (sit-for (* 5 (/ (abs (random)) (float most-positive-fixnum))))))

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
;; Org and ROAM
(load-file "~/.emacs.d/modes/org-mode.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; READING PATH FROM SHELL
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x))
          (memq window-system 'ns))
  :ensure t
  :config
    (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ packages and configs
(load-file "~/.emacs.d/modes/c-mode.el")

(use-package project :ensure t)

(use-package eglot
  :ensure t
  :after 'project
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '((python-mode) "jedi-language-server"))
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (python-mode . eglot-ensure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python and ELPY
(load-file "~/.emacs.d/modes/python-mode.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUCTEX
(load-file "~/.emacs.d/modes/auctex-mode.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lua
;; (load-file "~/.emacs.d/modes/lua-mode.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript
;; (load-file "~/.emacs.d/modes/js-mode.el")

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
