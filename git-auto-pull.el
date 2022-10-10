;;; git-auto-pull.el --- Automatically pulls from selected repositories using git

;; Author: Guilherme Schardong (schardong.gg@gmail.com)

(setq git-auto-pull-monitored-dirs "")

(defun git-installed-p ()
  "Checks if git is installed and reachable."
  (not (s-blank-p (executable-find "git"))))

(defun git-is-repo-p (dir)
  "Checks if the dir is a valid git repository."
  (file-exists-p (s-concat (file-name-as-directory dir) ".git")))

(defun run-git-pull-for-dir (dir)
  "Runs ``git pull'' on a single directory."
  (shell-command (s-concat "git -C " (file-name-as-directory dir) " pull")))

(defun run-git-pull ()
  "Runs git pull on the preconfigured directories."
  )

(provide git-auto-pull)
