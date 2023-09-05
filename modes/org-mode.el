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
        org-agenda-files '("~/Documents/org/codeace.org" "~/Documents/org/distributed_graphics.org" "~/Documents/org/home.org" "~/Documents/org/isr.org" "~/Documents/org/isr_implicits.org" "~/Documents/org/visgraf.org" "~/Documents/org/visualid.org")
        org-todo-keywords '((sequence "TODO"
                                      "WAITING"
                                      "ONGOING"
                                      "|"
                                      "DEFERRED"
                                      "CANCELLED"
                                      "DONE"))))

(use-package org-roam
  :ensure t
  :after org
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/Documents/roam/"))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         ("C-c n g" . org-roam-graph)
         (:map org-mode-map
               (("C-c n l" . org-roam-buffer-toggle)
                ("C-c n i" . org-roam-node-insert)
                ("C-c n c" . org-roam-capture)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add))))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-tree-slide
  :ensure t)
