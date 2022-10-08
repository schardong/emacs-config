(setq-default c-default-style "k&r"
              c-basic-offset 4
              indent-tabs-mode nil
              tab-width 4)

(defun cppreference-query ()
  "Searches cppreference"
  (interactive)
  (browse-url
   (concat
    "https://en.cppreference.com/mwiki/index.php?title=Special:Search&search="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "cppreference: ")))))

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
        ("C-c c r" . clang-format-region)
        ("C-c c q" . cppreference-query)))

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
