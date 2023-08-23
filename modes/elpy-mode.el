(use-package elpy
  :ensure t
  :after (company python)
  :init (elpy-enable)
  :config
  (setq elpy-rpc-backend "jedi"
        elpy-rpc-python-command "python"
        elpy-rpc-virtualenv-path 'current
        py-autopep8-options '("--ignore E402"))
  :hook
  (elpy-mode . hs-minor-mode))
