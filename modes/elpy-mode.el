(use-package elpy
  :ensure t
  :after (company python)
  :init (elpy-enable)
  :config
  (setq elpy-rpc-backend "jedi"
        elpy-rpc-python-command "python"
        elpy-rpc-virtualenv-path "~/.pyenv/shims/python"
        py-autopep8-options '("--ignore E402"))
  :hook
  ;; (elpy-mode . py-autopep8-enable-on-save)
  (elpy-mode . hs-minor-mode))
