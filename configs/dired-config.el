;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)
;; Sort Dired buffers. If we are on a macos system, change the default ls to coreutils ls (aka gls)
;; See: https://github.com/d12frosted/homebrew-emacs-plus/issues/383
(when (eq system-type 'darwin)
  (setq insert-directory-program "gls" dired-use-ls-dired t))

(setq dired-listing-switches "-agho --group-directories-first")
(setq dired-guess-shell-alist-user `(("\\.mp4\\'" "mpv")))
