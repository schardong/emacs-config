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
 ;;If using org-roam-protocol
 (require 'org-roam-protocol))
