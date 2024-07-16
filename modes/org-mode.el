(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . flyspell-mode)
         (org-mode . yas-minor-mode))
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-<tab>") nil))
  (use-package org-bullets
    :ensure t
    :hook (org-mode . org-bullets-mode))
  (setq org-log-done t
        org-agenda-files '("~/Documents/org/codeface.org" "~/Documents/org/courses.org" "~/Documents/org/distributed_graphics.org" "~/Documents/org/home.org" "~/Documents/org/isr.org" "~/Documents/org/isr_implicits.org" "~/Documents/org/research.org" "~/Documents/org/visgraf.org" "~/Documents/org/visualid.org")
        org-todo-keywords '((sequence "TODO"
                                      "WAITING"
                                      "ONGOING"
                                      "|"
                                      "DEFERRED"
                                      "CANCELLED"
                                      "DONE"))))

(use-package org-tree-slide
  :ensure t
  :after org)

(use-package org-pomodoro
  :ensure t)
