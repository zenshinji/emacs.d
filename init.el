;;;-*- mode: emacs-lisp; coding: utf-8 -*-
;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.

;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq gc-cons-threshold 100000000)
(setq
 debug-on-error     nil
 debug-on-signal    nil
 debug-on-quit      nil
 )

  ;;; http://www.youtube.com/watch?v=RvPFZL6NJNQ
(global-set-key (kbd "C-c C-d")
		(lambda () (interactive)
		  (setq debug-on-error (if debug-on-error nil t))
		  (message (format "debug-on-error : %s" debug-on-error))
		  )
		)

(setq load-prefer-newer t)
(setq
 coding-system-for-read                  'utf-8
 coding-system-for-write                 'utf-8
 )
(prefer-coding-system                    'utf-8)
(package-initialize)

;; (add-to-list 'load-path "~/src/org-mode/lisp")
(setq
 package-check-signature nil
 )

(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-refresh-contents)
(unless (package-installed-p 'package+)
  (package-install 'package+))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))
  ;; load up Org-mode and Org-babel
  (require 'org-install)
  (require 'ob-tangle)
  )

;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))
