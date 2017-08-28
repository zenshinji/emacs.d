;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; Title: my_init.el --- Where the magic begins
;;; Author: Paul R. Jorgensen
;;; Email: paul@prjorgensen.com
;;; Version: 1.0
;;; Time-stamp: <2017-07-08 20:18:32 paulj>
;;; Purpose: Set up my emacs environment

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq dotfiles-dir (file-name-directory
                    (or
                     (buffer-file-name)
                     load-file-name)
                    )
      )

(setq custom-file (concat user-emacs-directory (car (split-string (system-name) "\\.")) ".el"))
(load custom-file 'noerror)

(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1)
  )

(when (function 'scroll-bar-mode)
  (scroll-bar-mode -1)
  )

(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
(when (functionp 'blink-cursor-mode)
  (blink-cursor-mode -1)
  )

(line-number-mode   1)
(column-number-mode 1)

(setq
 initial-scratch-message ""
 inhibit-startup-message t
 ;;;
 ;;; Don't let Emacs hurt your ears or eyes
 ;;;
 visible-bell nil
 ring-bell-function (lambda ()
                      (unless (memq this-command
                                    '(isearch-abort
                                      mwheel-scroll
                                      abort-recursive-edit
                                      exit-minibuffer
                                      keyboard-quit
                                      previous-line
                                      next-line
                                      scroll-down
                                      scroll-up
                                      cua-scroll-down
                                      cua-scroll-up))
                        (invert-face 'mode-line)
                        (run-with-timer 0.05 nil 'invert-face 'mode-line))
                      )

 ;;;
 ;;; You need to set `inhibit-startup-echo-area-message' from the
 ;;; customization interface:
 ;;; M-x customize-variable RET inhibit-startup-echo-area-message RET
 ;;; then enter your username
 ;;;
 inhibit-startup-echo-area-message user-login-name
 initial-major-mode 'org-mode
 )

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

(require 'package)

(add-to-list 'package-archives '("org"          . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("gnu"          . "http://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade"    . "https://marmalade-repo.org/packages/") t)

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '(
          ;; (dash             . "melpa-stable")
          ;; (package+         . "melpa-stable")
          ;; (paradox          . "melpa-stable")
          (org              . "org")
          ;; (org-plus-contrib . "org")
          (use-package      . "melpa")
          )
        )
  )
(package-initialize)                ;; Initialize & Install Package
(package-refresh-contents)

(require 'cl)
(require 'cl-lib)

(add-to-list 'load-path (concat dotfiles-dir "elisp/"))
(let ((default-directory (concat dotfiles-dir "elisp/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path)
      )
  )

(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load

(setq
 use-package-verbose t
 )

;;; Bootstrap `diminish'
(unless (package-installed-p 'diminish)
  (package-install 'diminish)
  )
(eval-when-compile
  (require 'diminish))

;;; Bootstrap `bind-key'
(unless (package-installed-p 'bind-key)
  (package-install 'bind-key)
  )
(eval-when-compile
  (require 'bind-key))

(use-package auth-source
  :defer t
  :config
  (setq
   auth-sources '(
                  ;; default
                  ;; "secrets:session"
                  ;; "secrets:Login"
                  "~/.authinfo.gpg"
                  "~/.authinfo"
                  "~/.netrc"
                  )
   epa-file-cache-passphrase-for-symmetric-encryption t
   auth-source-debug 'trivia
   )
  )

(use-package async
  :ensure t
  :defer t
)

(use-package paradox
  :ensure t
  :defer t
  :config
  (setq
   paradox-execute-asynchronously t
   )
  (paradox-enable)
  :bind
  ("H-p"   . paradox-list-packages)
  ("C-c p" . paradox-list-packages)
  )

;; (defun ddd ()
;;   "Enter a backslash on the MBA keyboard."
;;   (interactive)
;;   (insert "\\")
;;   )

(global-set-key (kbd "M-¥") '(lambda ()
			       "Enter a backslash on the MBA keyboard."
			         (interactive)
				 (insert "\\")
				 ))

;; on Linux, the menu/apps key syntax is <menu>
;; on Windows, the menu/apps key syntax is <apps>
;; make the syntax equal
;; (define-key key-translation-map (kbd "<apps>") (kbd "<menu>"))
;; (define-key key-translation-map (kbd "<apps>") 'event-apply-hyper-modifier)
;; (define-key key-translation-map (kbd "<menu>") 'event-apply-hyper-modifier)
(define-key key-translation-map (kbd "<apps>") (kbd "<menu>"))
(define-key key-translation-map (kbd "<f13>") (kbd "<menu>"))

(progn
  ;; define set of key sequences
  (define-prefix-command 'my-leader-key-map)

  (define-key my-leader-key-map (kbd "RET") 'execute-extended-command)
  (define-key my-leader-key-map (kbd "<menu>") 'exchange-point-and-mark)
  (define-key my-leader-key-map (kbd "<f12>") 'org-capture)

  (define-key my-leader-key-map (kbd "'") 'quoted-insert)
  ;; (define-key my-leader-key-map (kbd "=") 'balance-windows)
  (define-key my-leader-key-map (kbd "2") 'delete-window)
  (define-key my-leader-key-map (kbd "3") 'delete-other-windows)
  (define-key my-leader-key-map (kbd "4") 'split-window-below)
  (define-key my-leader-key-map (kbd "5") 'split-window-right)

  (define-key my-leader-key-map (kbd "7") 'dired-jump)
  (define-key my-leader-key-map (kbd "9") 'ispell-word)

  (define-key my-leader-key-map (kbd "a") 'mark-whole-buffer)
  (define-key my-leader-key-map (kbd "b") 'end-of-buffer)
  (define-key my-leader-key-map (kbd "c") 'xah-copy-line-or-region)

  (define-key my-leader-key-map (kbd "d") 'beginning-of-buffer)
  (define-key my-leader-key-map (kbd "g") 'isearch-forward)

  (define-key my-leader-key-map (kbd "k") 'eval-buffer)
  (define-key my-leader-key-map (kbd "l") 'recenter-top-bottom)
  (define-key my-leader-key-map (kbd "m") 'universal-argument)
  (define-key my-leader-key-map (kbd "o") 'hydra-org2blog)
  (define-key my-leader-key-map (kbd "p") 'paradox-list-packages)
  (define-key my-leader-key-map (kbd "v") 'xah-paste-or-paste-previous)
  ;; (defaine-key my-leader-key-map (kbd "w") 'ace-window)
  (define-key my-leader-key-map (kbd "x") 'xah-cut-line-or-region)
  (define-key my-leader-key-map (kbd ";") 'comment-dwim)
  ;; (define-key my-leader-key-map (kbd "¥") ')
  )

;; make the menu key as leader key
(global-set-key (kbd "<menu>") 'my-leader-key-map)

(use-package hydra
  :ensure t
  :config
  (setq
   hydra-is-helpful t
   )
  )

(use-package hydra-examples
)

(defhydra hydra-org-template (:color blue :hint nil)
  "
 _c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
 _l_atex   _E_xample   _p_erl          _i_ndex:
 _a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
 _s_rc     _n_ote      plant_u_ml      _H_TML:
 _h_tml    ^ ^         ^ ^             _A_SCII:
"
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<E"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("n" (hot-expand "<not"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (hot-expand "<e")) ; "<s" "emacs-lisp :tangle yes"))
    ("p" (hot-expand "<s" "perl"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
    ("P" (hot-expand "<s" "perl" ":results output :exports both :shebang \"#!/usr/bin/env perl\"\n"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit")
    ("/" hydra-org-template/body "hint")
    )

(defun hot-expand (str &optional mod header)
  "Expand org template.

  STR is a structure template string recognised by org like <s. MOD is a
  string with additional parameters to add the begin line of the
  structure element. HEADER string includes more parameters that are
  prepended to the element after the #+HEADERS: tag."
  (let (text)
    (when (region-active-p)
      (setq text (buffer-substring (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end))
      (deactivate-mark))
    (when header (insert "#+HEADERS: " header))
    (insert str)
    (org-try-structure-completion)
    (when mod (insert mod) (forward-line))
    (when text (insert text))))

(define-key org-mode-map "<"
  (lambda () (interactive)
    (if (or (region-active-p) (looking-back "^"))
        (hydra-org-template/body)
      (self-insert-command 1))))

(eval-after-load "org"
  '(cl-pushnew
    '("not" "#+BEGIN_NOTES\n?\n#+END_NOTES")
    org-structure-template-alist))

(defhydra hydra-org2blog (global-map "<menu> o") 
  "
 ^Post^        q^Page^       ^C&Ts^
------------------------------------------
 _n_ew         post _D_raft _C_omplete C&Ts
 post _d_raft  _P_ublish    list _c_ategories
 _p_ublish                list _t_ags

 show in _b_rowser
 _SPC_ cancel
"
  ("C" org2blog/wp-complete-category)
  ("D" org2blog/wp-post-buffer-as-page)
  ("P" org2blog/wp-post-buffer-as-page-and-publish)
  ("b" org2blog/wp-show-post-in-browser)
  ("c" org2blog/wp-categories-list)
  ("d" org2blog/wp-post-buffer)             ; post the entry as a draft
  ("n" org2blog/wp-new-entry)               ; create a new post
  ("p" org2blog/wp-post-buffer-and-publish) ; post and publish the entry
  ("t" org2blog/wp-tags-list)
  ("SPC" nil)
  )

(use-package org
  :ensure t
  :demand
  :bind
  ("C-c a" . org-agenda )
  ("C-c c" . org-capture)
  ("H-c"   . org-capture)
  ("H-a"   . org-agenda )
  :init
  (add-hook 'org-mode-hook
            (lambda()
              (variable-pitch-mode t)
              (visual-line-mode    1)
              (flyspell-mode       1)
              )
            )
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  :config
  (setq
   org-replace-disputed-keys         t
   org-src-fontify-natively          t
   org-src-preserve-indentation      t
   org-src-tab-acts-natively         t
   org-src-window-setup              'current-window
   org-CUA-compatible                t
   org-directory "~/Dropbox/org"
   )
  (dolist
      (face '(
              org-block-begin-line
              org-block-end-line
              org-verbatim
              ;; org-block-background ; stopped working Aug 2015 http://orgmode.org/cgit.cgi/org-mode.git/commit/?id=f8b42e8
              org-code
              org-block
              org-table
              org-meta-line
              org-document-info
              org-document-info-keyword
            font-lock-comment-face
            )
            )
    (set-face-attribute face nil :inherit 'fixed-pitch)    
    )
  )

(use-package org-mouse)

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/tickler.org"))

(setq org-capture-templates '(
			      ("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")
			      ("n" "Note [inbox]" entry
			       (file+headline "~/gtd/inbox.org" "Notes")
			       "* %i%? \n %U")
			      )
      )

(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))


(setq org-todo-keywords '((sequence
                           "TODO(t)"
                           "WAITING(w)"
                           "|"
                           "DONE(d)"
                           "CANCELLED(c)"
                           )))

(setq org-agenda-custom-commands 
      '(("o" "At the office" tags-todo "@office"
         ((org-agenda-overriding-header "Office")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(setq
 org-structure-template-alist     '(
                                    ("A" "#+ascii: ")
                                    ("H" "#+html: " "<literal style=\"html\">?</literal>")
                                    ("I" "#+include %file ?" "<include file=%file markup=\"?\">")
                                    ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
                                    ("S" "#+end_src\n\n#+NAME: ?\n#+begin_src emacs-lisp" "</src>\n?\n<src lang=\"emacs-lisp\">")
                                    ("a" "#+begin_ascii\n?\n#+end_ascii")
                                    ("c" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
                                    ("E" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
                                    ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
                                    ("i" "#+index: ?" "#+index: ?")
                                    ("e" "#+NAME:\n#+begin_src emacs-lisp :tangle yes\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")
                                    ("p" "#+BEGIN_PRACTICE\n?\n#+END_PRACTICE")
                                    ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
                                    ("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
                                    ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE" "<verse>\n?\n</verse>")
                                    )
     )

;; (define-key org-mode-map (kbd "RET") 'org-return-indent)
;; (define-key org-mode-map (kbd "<return>") 'org-return-indent)

(use-package org-inlinetask
  )

(defun scimax/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond

     ((eq 'line-break (car (org-element-context)))
      (org-return-indent))

     ;; Open links like usual, unless point is at the end of a line.
     ;; and if at beginning of line, just press enter.
     ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
          (bolp))
      (org-return))

     ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
     ;; Johansson!
     ((org-inlinetask-in-task-p)
      (org-return))

     ;; checkboxes too
     ((org-at-item-checkbox-p)
      (org-insert-todo-heading nil))

     ;; lists end with two blank lines, so we need to make sure we are also not
     ;; at the beginning of a line to avoid a loop where a new entry gets
     ;; created with only one blank line.
     ((org-in-item-p)
      (if (save-excursion (beginning-of-line) (org-element-property :contents-begin (org-element-context)))
          (org-insert-heading)
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position))
        (org-return)))

     ;; org-heading
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title (org-element-context))))
          (progn (org-end-of-meta-data)
                 (org-insert-heading-respect-content)
                 (outline-show-entry))
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")))

     ;; tables
     ((org-at-table-p)
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (org-table-to-lisp)))
          (org-return)
        ;; empty row
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))

     ;; fall-through case
     (t
      (org-return))))
  )


(define-key org-mode-map (kbd "RET")
  'scimax/org-return)
(define-key org-mode-map (kbd "<return>")
  'scimax/org-return)

(use-package org-cliplink
  :ensure t
  :bind
  ("C-c l" . org-cliplink)
  )

(defun my-dnd-func (event)
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fname (cadr payload))
         (img-regexp "\\(png\\|jp[e]?g\\)\\>"))
    (cond
     ;; insert image link
     ((and  (eq 'drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; insert image link with caption
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert "#+ATTR_ORG: :width 300\n")
      (insert (concat  "#+CAPTION: " (read-input "Caption: ") "\n"))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; C-drag-n-drop to open a file
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type))
      (find-file fname))
     ((and (eq 'M-drag-n-drop (car event))
           (eq 'file type))
      (insert (format "[[attachfile:%s]]" fname)))
     ;; regular drag and drop on file
     ((eq 'file type)
      (insert (format "[[%s]]\n" fname)))
     (t
      (error "I am not equipped for dnd on %s" payload)))))


(define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<C-drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<M-drag-n-drop>") 'my-dnd-func)









(use-package xml-rpc
  :load-path "~/src/xml-rpc-el"
  )

(use-package metaweblog
  :after xml-rpc
  :load-path "~/src/metaweblog"
)

(use-package org2blog
  :after metaweblog
  :load-path "../src/org2blog"
  ;; :bind
  ;; ("C-c o p" . org2blog/wp-post-buffer-and-publish)
  ;; ("C-c o d" . org2blog/wp-post-buffer)
  ;; ("C-c o c" . org2blog/wp-complete-category)
  ;; ("C-c o n" . org2blog/wp-new-entry)
  :init
  (add-hook 'org-mode-hook #'org2blog/wp-org-mode-hook-fn)
  (add-hook 'org2blog/wp-after-new-post-or-page-functions (lambda (p) (pp p)))
  :config
  (setq
   org2blog/wp-use-sourcecode-shortcode t
   org2blog/wp-confirm-post t
   ;; org2blog/wp-track-posts '("~/Dropbox/org/web/org2blog.org" "Posts")
   org2blog/wp-blog-alist
   `(
     ("PRJ"
      :url "https://www.prjorgensen.com/xmlrpc.php"
      :username ,(car (auth-source-user-and-password "prjorgensen.com"))
      :password ,(cadr (auth-source-user-and-password "prjorgensen.com"))
      :default-title "Hello, World!"
      :default-categories ("Uncategorized" "org2blog")
      ;; :track-posts '("~/Dropbox/org/web/PRJ/org2blog.org" "Posts")
      )
     ("PVCSEC"
      :url "https://www.pvcsec.com/xmlrpc.php"
      :username ,(car (auth-source-user-and-password "pvcsec.com"))
      :password ,(cadr (auth-source-user-and-password "pvcsec.com"))
      :default-title "Hello, World!"
      :default-categories ("Uncategorized" "org2blog")
      ;; :track-posts '("~/Dropbox/org/web/PVCSEC/org2blog.org" "Posts")
      )
     )
   )
  )

(use-package htmlize
  :ensure t
  )

(use-package desktop
  :config
  (setq
   desktop-base-file-name (concat user-emacs-directory (car (split-string (system-name) "\\.")) ".desktop")
   desktop-base-lock-name (concat user-emacs-directory (car (split-string (system-name) "\\.")) ".desktop.lock")
   )
  ;; (add-hook 'after-init-hook #'my/desktop-settings-setup "APPEND")
  ;; (add-hook 'after-init-hook #'my/hide-init-buffers "APPEND")
  ;; (kill-buffer "*scratch*")
  (desktop-save-mode 1)
  )

(winner-mode 1)

(use-package ace-window
  :ensure t
  :defer 1
  :config
  (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 3.0)
  (set-face-attribute 'aw-mode-line-face nil :inherit 'mode-line-buffer-id :foreground "lawn green")
  (setq
   aw-keys   '(?a ?s ?d ?f ?j ?k ?l)
   aw-dispatch-always t
   aw-dispatch-alist
   '(
     (?x aw-delete-window     "Ace - Delete Window")
     (?c aw-swap-window       "Ace - Swap Window")
     (?n aw-flip-window)
     (?v aw-split-window-vert "Ace - Split Vert Window")
     (?h aw-split-window-horz "Ace - Split Horz Window")
     (?m delete-other-windows "Ace - Maximize Window")
     (?g delete-other-windows)
     (?b balance-windows)
     (?u winner-undo)
     (?r winner-redo)
     )
   )
  
  (when (package-installed-p 'hydra)
    (defhydra hydra-window-size (:color red)
      "Windows size"
      ("h" shrink-window-horizontally "shrink horizontal")
      ("j" shrink-window "shrink vertical")
      ("k" enlarge-window "enlarge vertical")
      ("l" enlarge-window-horizontally "enlarge horizontal")
      )
    (defhydra hydra-window-frame (:color red)
      "Frame"
      ("f" make-frame "new frame")
      ("x" delete-frame "delete frame")
      )
    (defhydra hydra-window-scroll (:color red)
      "Scroll other window"
      ("n" joe-scroll-other-window "scroll")
      ("p" joe-scroll-other-window-down "scroll down")
      )
    (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
    (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
    (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
  (ace-window-display-mode t)
  )

(use-package windmove
  :ensure t
  :bind (
  	 ("<f2> <right>" . windmove-right)
  	 ("<f2> <left>"  . windmove-left)
  	 ("<f2> <up>"    . windmove-up)
  	 ("<f2> <down>"  . windmove-down)
  	 )
  :init
  ;; Make windmove work in org-mode:
  ;; (add-hook 'org-shiftup-final-hook 'windmove-up)
  ;; (add-hook 'org-shiftleft-final-hook 'windmove-left)
  ;; (add-hook 'org-shiftdown-final-hook 'windmove-down)
  ;; (add-hook 'org-shiftright-final-hook 'windmove-right)
  :config
  (windmove-default-keybindings 'super)
  )

(use-package framemove
  :ensure t
  ;; :bind (
  ;; 	 ("<menu> <right>" . fm-right-frame)
  ;; 	 ("<menu> <left>"  . fm-left-frame)
  ;; 	 ("<menu> <up>"    . fm-up-frame)
  ;; 	 ("<menu> <down>"  . fm-down-frame)
  ;; 	 )
  :config
  ;; (windmove-default-keybindings)
  (setq
   framemove-hook-into-windmove t
   )
)

(use-package switch-window
  :bind (
	 ("<f2> o" . switch-window)
	 )
  )

(use-package adaptive-wrap
  :ensure t
  :config
  (setq
   adaptive-wrap-extra-indent 4
   )
  (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
  (diminish 'visual-line-mode "🎁")
  )

(use-package aggressive-indent
  :diminish "⏭"
  :ensure t
  :defer t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  )

(use-package solarized-theme
  :ensure t
  :config
  (setq
   solarized-distinct-fringe-background t
   x-underline-at-descent-line          t
   solarized-high-contrast-mode-line    nil
   solarized-scale-org-headlines        nil
   ;; Prefer italics over bold
   solarized-use-less-bold t
   solarized-use-more-italic t
   solarized-distinct-doc-face t ; Emphasize docstrings
   ;; I find different font sizes irritating.
   solarized-height-minus-1 1.0
   solarized-height-plus-1 1.0
   solarized-height-plus-2 1.0
   solarized-height-plus-3 1.0
   solarized-height-plus-4 1.0
   )
  (load-theme 'solarized-light)
  (load-theme 'smart-mode-line-light)
  )

(use-package smart-mode-line
  :ensure t
  :after spaceline-config
  :config
  (sml/setup)
  (add-to-list 'sml/replacer-regexp-list '("^~/OneDrive/"    ":1:") t)
  (add-to-list 'sml/replacer-regexp-list '("[dD]ocuments/"   "📎:") )
  (add-to-list 'sml/replacer-regexp-list '("[dD]ownloads/"   "🢛:" ) )
  (add-to-list 'sml/replacer-regexp-list '("[mM]usic/"       "🎵:" ) )
  (add-to-list 'sml/replacer-regexp-list '("[vV]ideos/"      "📼:") )
  (add-to-list 'sml/replacer-regexp-list '("[pP]ictures/"    "🖼:" ) )
  (add-to-list 'sml/replacer-regexp-list '("/etc/"           ":&:") )
  (add-to-list 'sml/replacer-regexp-list '("[pP]rojects/"    "📽:") )
  )

(use-package mode-icons
  :ensure t
  :after spaceline-config
  :config
  (mode-icons-mode)
  )

(use-package spaceline-config           ; A beautiful mode line
  :ensure spaceline
  :after solarized-theme
  :config
  ;; (spaceline-helm-mode)                 ; Enable a special Helm mode line
  (spaceline-emacs-theme)
  )

(use-package powerline                  ; The work-horse of Spaceline
  :ensure t
  :after spaceline-config
  :config (setq
           powerline-height (truncate (* 1.0 (frame-char-height)))
           powerline-default-separator 'utf-8
           )
  )

(setq scroll-preserve-screen-position 1) ; keep cursor at same position when scrolling
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v")) ; scroll window up by 1 line
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v")) ; scroll window down by 1 line

(setq
 show-paren-style 'expression           ; highlight entire bracket expression
 )
(show-paren-mode 1)

;; make electric-pair-mode work on more brackets
(setq
 electric-pair-pairs '(
                       (?\" . ?\")
                       (?\{ . ?\})
                       )
 )
(electric-pair-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

;; rainbow-mode
  

;; [[file:~/.emacs.d/my_init.org::*rainbow-mode][rainbow-mode:1]]
(use-package rainbow-mode
  :ensure t
  :defer t
  :diminish "🌈"
  :config
  (add-hook 'css-mode-hook 'rainbow-mode)
  ;; (eval-after-load "diminish"
  ;;   '(progn
  ;;      (eval-after-load "rainbow-mode"
  ;;        '(diminish 'rainbow-mode "rb")))
  ;; )
  )
;; rainbow-mode:1 ends here

(use-package pretty-mode
  :ensure t
  :config
  (global-pretty-mode t)
  )

;; Remove BufFace from mode line
(eval-after-load "face-remap"
  '(diminish 'buffer-face-mode))

(use-package beacon                     ; Highlight cursor position in buffer
  :ensure t
  :init (beacon-mode 1)
  :diminish "⚐"
  )

(use-package which-key
  :diminish "¿"
  :ensure t
  :init (which-key-mode)
  :config
  (setq
   which-key-idle-delay 0.4
   which-key-sort-order 'which-key-prefix-then-key-order
   ;; Let's go unicode :)
   which-key-key-replacement-alist  '(("<\\([[:alnum:]-]+\\)>" . "\\1")
                                      ("up"                    . "↑")
                                      ("right"                 . "→")
                                      ("down"                  . "↓")
                                      ("left"                  . "←")
                                      ("DEL"                   . "⌫")
                                      ("deletechar"            . "⌦")
                                      ("RET"                   . "⏎"))
   which-key-description-replacement-alist '(("Prefix Command" . "prefix")
                                             ;; Lambdas
                                             ("\\`\\?\\?\\'"   . "λ")
                                             ;; Prettify hydra entry points
                                             ("/body\\'"       . "|=")
                                             ;; Drop/shorten package prefixes
                                             ("\\`lunaryorn-"  . "")
                                             ("projectile-"    . "proj-")
                                             ("helm-"          . "⎈-")
                                             ("magit-" . "ma-")
                                             )
   )
  
  )

(use-package undo-tree
  :ensure t
  :diminish "⎌"
  :bind(
        ("C-z"   . undo)
        ("C-S-z" . undo-tree-redo)
        )
  :config
  (defalias 'redo 'undo-tree-redo)
  (global-undo-tree-mode 1)
  ;; (global-unset-key (kbd "C-z"))
  ;; (global-unset-key (kbd "C-S-z"))
  )

(setq
 auto-revert-verbose nil               ; don't notify when dired is refreshed
 delete-by-moving-to-trash t
 global-auto-revert-non-file-buffers t ; auto refresh dired
 trash-directory "~/.Trash/"
 )

(global-set-key (kbd "H-r") 'revert-buffer)

(use-package anzu
  :ensure t
  :defer t
  :config
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (setq
   anzu-mode-lighter                ""
   anzu-deactivate-region           t
   anzu-search-threshold            1000
   anzu-replace-to-string-separator " => "
   anzu-cons-mode-line-p            nil
   )
  :bind (
         ([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)
         ;; ("M-%"   . anzu-query-replace)
         ;; ("C-M-%" . anzu-query-replace-regexp)
         )
  :init   (global-anzu-mode)
  :diminish "￼"
  )

(use-package counsel
  :ensure t
  :bind
  (
   ("<f1>-f"  . counsel-describe-function  )
   ("<f1>-l"  . counsel-find-library       )
   ("<f1>-v"  . counsel-describe-variable  )
   ("<f2>-i"  . counsel-info-lookup-symbol )
   ("<f2>-u"  . counsel-unicode-char       )
   ("C-."     . counsel-imenu              )
   ("C-M-i"   . complete-symbol            )
   ("C-S-o"   . counsel-rhythmbox          )
   ("C-c 8"   . counsel-unicode-char       )
   ("C-c C-r" . ivy-resume                 )
   ("C-c V"   . ivy-pop-view               )
   ("C-c g"   . counsel-git                )
   ("C-c j"   . counsel-git-grep           )
   ("C-c k"   . counsel-ag                 )
   ("C-c v"   . ivy-push-view              )
   ("C-s"     . swiper                     )
   ("C-x C-f" . counsel-find-file          )
   ("C-x b"   . ivy-switch-buffer          )
   ("C-x l"   . counsel-locate             )
   ("M-x"     . counsel-M-x                )
   )
  )

(use-package ivy
  :diminish "⚘"
  :config
  (setq
   ivy-use-virtual-buffers      t
   ivy-count-format             "%d/%d "
   use-virtual-buffers          t
   enable-recursive-minibuffers t
   ivy-switch-buffer-faces-alist
   '(
     (emacs-lisp-mode . swiper-match-face-1)
     (dired-mode      . ivy-subdir)
     (org-mode        . org-level-4)
     )
   )
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  )

(use-package ivy-hydra
  :ensure t)

(use-package swiper
  :ensure try)

(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  )

(use-package abbrev
  :diminish "𝄅⥄"
  :config
  (setq-default abbrev-mode t)
  (setq
   save-abbrevs 'silently
   ;; abbrev-file-name (concat user-emacs-directory (car (split-string (system-name) "\\.")) ".abbrev.el")
   )
  (quietly-read-abbrev-file)
  )

(use-package define-word
  :ensure t
  :bind (("H-d" . define-word-at-point)
	 ("H-D" . define-word)))

(use-package artbollocks-mode
  :ensure t
  :config
  (progn
    (setq artbollocks-weasel-words-regex
          (concat "\\b"   (regexp-opt
                           '("I think"
                             ", but "
                             "a lot"
                             "absolutely"
                             "action"
                             "actually"
                             "added bonus"
                             "appears"
                             "asked"
                             "at this point in time"
                             "basically"
                             "been"
                             "began"
                             "begin"
                             "begun"
                             "both of them"
                             "breath"
                             "breathe"
                             "certainly"
                             "completely"
                             "comprised of"
                             "crystal clear"
                             "definitely"
                             "down"
                             "downsize"
                             "even"
                             "exact same"
                             "exactly the same as"
                             "exhale"
                             "feel"
                             "felt"
                             "fewer in number"
                             "frankly"
                             "honestly"
                             "inasmuch"
                             "inhale"
                             "it is said"
                             "just"
                             "kind of"
                             "leverage"
                             "literally"
                             "many"
                             "maybe"
                             "needless to say"
                             "nice"
                             "nod"
                             "often"
                             "one of the"
                             "past history"
                             "perhaps"
                             "point in time"
                             "ponder"
                             "probably"
                             "practically"
                             "pretty"
                             "probably"
                             "quite"
                             "rather"
                             "reach"
                             "realize"
                             "really"
                             "reason why is because"
                             "replied"
                             "rightsizing"
                             "seems"
                             "said"
                             "should"
                             "shrug"
                             "so"
                             "some"
                             "somehow"
                             "somewhat"
                             "sort of"
                             "start"
                             "started"
                             "streamlining"
                             "that"
                             "the fact that"
                             "then"
                             "there is no doubt that"
                             "thing"
                             "things"
                             "think"
                             "thought"
                             "to be honest"
                             "totally"
                             "true fact"
                             "truthfully"
                             "understand"
                             "until such time as"
                             "up"
                             "utterly"
                             "very"
                             "virtually"
                             "was"
                             "well"
                             "went"
                             "were"
                             "what happened was"
                             "wonder"
                             ) t) "\\b")
          artbollocks-jargon t
          )
    )
  ;; ;; Make sure keywords are case-insensitive
  ;; (defadvice search-for-keyword (around sacha activate)
  ;;   "Match in a case-insensitive way."
  ;;   (let ((case-fold-search t))
  ;;     ad-do-it))
  (add-hook 'org-capture-mode-hook 'artbollocks-mode)
  (add-hook 'text-mode-hook        'artbollocks-mode)
  (add-hook 'org-mode-hook         'artbollocks-mode)
  (eval-after-load "artbollocks-mode" '(diminish 'artbollocks-mode "🎨"))
  )

(use-package writegood-mode
  :ensure t
  :bind
  (
   ("C-c g"     . writegood-mode)
   ("C-c C-g g" . writegood-grade-level)
   ("C-c C-g e" . writegood-reading-ease)
   )
  :config
  (add-hook 'org-capture-mode-hook 'writegood-mode)
  (add-hook 'text-mode-hook        'writegood-mode)
  (add-hook 'org-mode-hook         'writegood-mode)
  )

(use-package flyspell
  :diminish "🐛"
  :config
  (setq
   flyspell-issue-message-flag nil
   flyspell-issue-welcome-flag nil
   )
  ;;; Add spell-checking in comments for all programming language modes
  (dolist (hook '(lisp-mode-hook
                  emacs-lisp-mode-hook
                  scheme-mode-hook
                  clojure-mode-hook
                  ruby-mode-hook
                  yaml-mode
                  python-mode-hook
                  shell-mode-hook
                  php-mode-hook
                  css-mode-hook
                  haskell-mode-hook
                  caml-mode-hook
                  c++-mode-hook
                  c-mode-hook
                  lua-mode-hook
                  crontab-mode-hook
                  perl-mode-hook
                  tcl-mode-hook
                  js2-mode-hook))
    (add-hook hook 'flyspell-prog-mode))
  )

;; NO spell check for embedded snippets
(defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
  (let ((rlt ad-return-value)
        (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\|quote\\)")
        (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\|quote\\)")
        old-flag
        b e)
    (when ad-return-value
      (save-excursion
        (setq old-flag case-fold-search)
        (setq case-fold-search t)
        (setq b (re-search-backward begin-regexp nil t))
        (if b (setq e (re-search-forward end-regexp nil t)))
        (setq case-fold-search old-flag))
      (if (and b e (< (point) e)) (setq rlt nil))
      )
    (setq ad-return-value rlt))
  )

(use-package ispell
  :bind (
         ("C-c s" . endless/ispell-word-then-abbrev)
         )
  :config
  ;; find aspell and hunspell automatically
  (cond
   ((executable-find "aspell")
    (setq
     ispell-program-name "aspell"
     ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
     ispell-personal-dictionary "~/.ispell"
     ispell-really-aspell t
     )
    )
   ((executable-find "hunspell")
    (setq
     ispell-program-name "hunspell"
     ispell-extra-args '("-d en_US")
     ispell-really-hunspell t
     ispell-local-dictionary "en_US"
     )
    )
   )
  )

(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'.

URL `http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html'
Version 2016-01-20"
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

;;; Tell ispell.el that ’ can be part of a word.
(setq ispell-local-dictionary-alist
      `((nil "[[:alpha:]]" "[^[:alpha:]]"
             "['\x2019]" nil ("-B") nil utf-8)))

;;; Don't send ’ to the subprocess.
(defun endless/replace-apostrophe (args)
  (cons (replace-regexp-in-string
         "’" "'" (car args))
        (cdr args)))
(advice-add #'ispell-send-string :filter-args
            #'endless/replace-apostrophe)

;;; Convert ' back to ’ from the subprocess.
(defun endless/replace-quote (args)
  (if (not (derived-mode-p 'org-mode))
      args
    (cons (replace-regexp-in-string
           "'" "’" (car args))
          (cdr args))))
(advice-add #'ispell-parse-output :filter-args
            #'endless/replace-quote)

;; move point to previous error
;; based on code by hatschipuh at
;; http://emacs.stackexchange.com/a/14912/2017
(defun flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))
        (forward-word)))))

(global-set-key (kbd "C-c ,") 'flyspell-goto-previous-error)

(defun endless/comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

(global-set-key (kbd "C-;") #'endless/comment-line)

;; This turns on auto-insert-mode
(auto-insert-mode)
;; This turns off the prompt that auto-insert-mode asks before
;; it actually inserts text/code for you
(setq auto-insert-query nil)
;; This is what you'll have inserted for a new .org file
(define-skeleton my-org-defaults
  "Org defaults I use"
  nil
  "# -*- mode: org; coding: utf-8 -*-\n"
  ;; "#+TITLE: " str | (buffer-name) "\n"
  ;; "#+AUTHOR: " (user-full-name) "\n"
  ;; "#+EMAIL: paul@prjorgensen.com\n"
  ;; "#+LANGUAGE: en\n"
  ;; "#+Time-stamp: <>\n"
  ;; "#+INFOJS_OPT: view:showall toc:t ltoc:t mouse:underline path:http://orgmode.org/org-info.js\n"
  ;; "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"../css/notebook.css\" />\n"
  ;; "#+LATEX_HEADER: \\usepackage{lmodern}\n"
  ;; "#+LATEX_HEADER: \\usepackage[T1]{fontenc}\n"
  ;; "#+OPTIONS: toc:nil num:0\n"
  ;; "#+OPTIONS: H:2 num:nil toc:nil \\n:nil @:t ::t |:t ^:nil\n"
  ;; "#+STARTUP: hideblocks\n"
  ;; "#+CREATOR: \n"
  ;; "#+DESCRIPTION: \n"
  ;; "#+SELECT_TAGS: export\n"
  ;; "#+EXCLUDE_TAGS: noexport\n"
  ;; "#+KEYWORDS:\n"
  "\n"
  )
;; This is how to tell auto-insert what to use for .org files
(define-auto-insert "\\.org\\'" 'my-org-defaults)
(define-auto-insert "\\.txt\\'" 'my-org-defaults)

(define-key org-mode-map "'" #'endless/apostrophe)
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map "'"
     #'endless/apostrophe))

(defun endless/apostrophe (opening)
  "Insert ’ in prose or `self-insert-command' in code.
With prefix argument OPENING, insert ‘’ instead and
leave point in the middle.
Inside a code-block, just call `self-insert-command'."
  (interactive "P")
  (if (and (derived-mode-p 'org-mode)
           (org-in-block-p '("src" "latex" "html")))
      (call-interactively #'self-insert-command)
    (if (looking-at "['’][=_/\\*]?")
        (goto-char (match-end 0))
      (if (null opening)
          (insert "’")
        (insert "‘’")
        (forward-char -1)))))

(define-key org-mode-map "\"" #'endless/round-quotes)
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map "\""
     #'endless/round-quotes))

(defun endless/round-quotes (italicize)
  "Insert “” and leave point in the middle.
With prefix argument ITALICIZE, insert /“”/ instead
\(meant for org-mode).
Inside a code-block, just call `self-insert-command'."
  (interactive "P")
  (if (and (derived-mode-p 'org-mode)
           (org-in-block-p '("src" "latex" "html")))
      (call-interactively #'self-insert-command)
    (if (looking-at "”[/=_\\*]?")
        (goto-char (match-end 0))
      (when italicize
        (if (derived-mode-p 'markdown-mode)
            (insert "__")
          (insert "//"))
        (forward-char -1))
      (insert "“”")
      (forward-char -1))))

(delete-selection-mode 1)

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2017-07-08"
  (interactive)
  (if current-prefix-arg
      (progn
        (kill-ring-save (point-min) (point-max))
        (message "All visible buffer text copied"))
    (if (use-region-p)
        (progn
          (kill-ring-save (region-beginning) (region-end))
          (message "Active region copied"))
      (if (eq last-command this-command)
          (if (eobp)
              (progn (message "empty line at end of buffer." ))
            (progn
              (kill-append "\n" nil)
              (kill-append
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))
               nil)
              (message "Line copy appended")
              (progn
                (end-of-line)
                (forward-char))))
        (if (eobp)
            (if (eq (char-before) 10 )
                (progn (message "empty line at end of buffer." ))
              (progn
                (kill-ring-save (line-beginning-position) (line-end-position))
                (end-of-line)
                (message "line copied")))
          (progn
            (kill-ring-save (line-beginning-position) (line-end-position))
            (end-of-line)
            (forward-char)
            (message "line copied")))))))

(defun xah-paste-or-paste-previous ()
  "Paste. When called repeatedly, paste previous.
This command calls `yank', and if repeated, call `yank-pop'.

When `universal-argument' is called first with a number arg, paste that many times.

URL `http://ergoemacs.org/emacs/emacs_paste_or_paste_previous.html'
Version 2017-07-25"
  (interactive)
  (progn
    (when (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (if current-prefix-arg
        (progn
          (dotimes ($i (prefix-numeric-value current-prefix-arg))
            (yank)))
      (if (eq real-last-command this-command)
          (yank-pop 1)
        (yank)))))

(defun xah-show-kill-ring ()
  "Insert all `kill-ring' content in a new buffer.

URL `http://ergoemacs.org/emacs/emacs_show_kill_ring.html'
Version 2017-06-19"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (progn
      (switch-to-buffer $buf)
      (funcall 'fundamental-mode)
      (setq buffer-offer-save t)
      (dolist (x kill-ring )
        (insert x "\n--------------------------------------------------\n\n"))
      (goto-char (point-min)))))

(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-08-25"
  (interactive "P")
  (let (($fpath
         (if (equal major-mode 'dired-mode)
             (progn
               (mapconcat 'identity (dired-get-marked-files) "\n"))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))

(use-package erc
  :defer
  :config
  (use-package erc-sasl
    :after erc
    :load-path "~/.emacs.d/elisp/"
    :config
    (add-to-list 'erc-sasl-server-regexp-list ".*\\freenode.net")
    (add-to-list 'erc-sasl-server-regexp-list ".*\\thinstack.net")
    (add-to-list 'erc-sasl-server-regexp-list ".*\\chatrealm.net")
    (add-to-list 'erc-sasl-server-regexp-list ".*")
    )
  (erc-scrolltobottom-mode)
  (erc-add-scroll-to-bottom)
  (setq
   erc-flood-protect              nil
   erc-format-nick-function      'erc-format-@nick   ; show user mode next to nick
   erc-hide-list                 '("JOIN" "NICK" "PART" "QUIT" "MODE")
   erc-input-line-position       -2     ; pin scroll to the buffer's bottom
   erc-insert-timestamp-function 'erc-insert-timestamp-left
   erc-interpret-mirc-color       t
   erc-kill-buffer-on-part        t     ; Kill buffers for channels after /part
   erc-kill-queries-on-quit       t     ; Kill buffers for private queries after quitting the server
   erc-kill-server-buffer-on-quit t     ; Kill buffers for server messages after quitting the server
   erc-nick                      "zenshinji"
   erc-nick-uniquifier           "`"
   erc-port                       6667
   erc-prompt (lambda ()
                (concat "[" (buffer-name) "]")
                )
   erc-prompt-for-nickserv-password nil
   erc-server-auto-reconnect      t
   erc-server-coding-system      '(utf-8 . utf-8) ; Set UTF-8
   ;; erc-server-flood-penalty       1000000
   erc-server-reconnect-attempts  3
   erc-server-reconnect-timeout   60
   erc-server-reconnect-timeout   30
   erc-server-send-ping-interval  45
   erc-server-send-ping-timeout   180
   erc-spelling-mode              1
   erc-timestamp-format          "%T "
   erc-timestamp-only-if-changed-flag nil
   erc-track-exclude-types       '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477")
   erc-try-new-nick-p             t
   )
  (add-hook 'erc-mode-hook
            (lambda()
              (linum-mode 0)
              ;; 'erc-add-scroll-to-bottom
              ;; 'erc-fill-disable
              (set (make-local-variable 'scroll-conservatively) 100)
              (erc-fill-disable)
              (visual-line-mode 1)
              (variable-pitch-mode t)
              )
            )
  (add-hook 'erc-insert-post-hook 'erc-scroll-to-bottom)
  )

;; authentication

(defun erc-login ()
  "Perform user authentication at the IRC server."
  (erc-log (format "login: nick: %s, user: %s %s %s :%s"
		   (erc-current-nick)
		   (user-login-name)
		   (or erc-system-name (system-name))
		   erc-session-server
		   erc-session-user-full-name))
  (if erc-session-password
      (erc-server-send (format "PASS %s" erc-session-password))
    (message "Logging in without password"))
  (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
    (erc-server-send "CAP REQ :sasl"))
  (erc-server-send (format "NICK %s" (erc-current-nick)))
  (erc-server-send
   (format "USER %s %s %s :%s"
	   ;; hacked - S.B.
	   (if erc-anonymous-login erc-email-userid (user-login-name))
	   "0" "*"
	   erc-session-user-full-name))
  (erc-update-mode-line))

(use-package erc-netsplit
  :defer t
  :config
  (erc-netsplit-mode t)
  )

(use-package erc-track
  :defer t
  :config
  (erc-track-mode t)
  )

(use-package erc-ring
  :defer t
  :config
  (erc-ring-enable)
  (add-to-list 'erc-modules 'ring)
  )

(use-package erc-services
  :defer t
  :config
  (add-to-list 'erc-modules 'services)
  (erc-services-mode 1)
  )

(use-package erc-autoaway
  :defer t
  :config
  (add-to-list 'erc-modules 'autoaway)
  (setq
   erc-autoaway-idle-seconds 1800
   erc-auto-discard-away     t
   )
  )

(use-package erc-log
  :defer t
  :config
  (add-to-list 'erc-modules 'log)
  (erc-log-enable)
  (setq
   erc-generate-log-file-name-function 'erc-generate-log-file-name-short
   erc-log-channels-directory "~/Dropbox/erc-logs/"   ;;default is C-c C-l saves to ~/logs/
   erc-log-insert-log-on-open nil             ;;is on by default
   erc-log-write-after-insert t
   erc-log-write-after-send t
   erc-save-buffer-on-part t                  ;;autosave log when /part or /quit
   )
  (if (not (file-exists-p erc-log-channels-directory))
      (mkdir erc-log-channels-directory t)
    )
  ;; Truncate buffers so they don't hog core.
  (setq erc-max-buffer-size 20000)
  (defvar erc-insert-post-hook)
  (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
  (setq erc-truncate-buffer-on-save t)
  )

(use-package erc-join
  :defer t
  :config
  (erc-autojoin-mode 1)
  (setq
   erc-autojoin-channels-alist
   '(
     (".*\\freenode.net"  "#erc" "#org-mode" "#emacs" "#gnus" "#ahk" "#misec")
     (".*\\debian.org"    "#debian")
     (".*\\oftc.net"      "#bitlbee")
     (".*\\thinstack.net" "#frogpants")
     (".*\\localhost"     "&gtalk" "#twitter_prjorg") ;;"#twitter_pvcsec")
     (".*\\chatrealm.net" "#frogpants" "#live")
     )
   )
  )

(use-package erc-goodies
  :defer t
  )

(use-package erc-match
  :defer t
  :config
    ;;; highlight differently when running the bitlbee command: blist
  (setq
   erc-keywords '(
                  (".*Online.*" (:foreground "green" ))
                  (".*Busy"     (:foreground "red"   ))
                  (".*Away"     (:foreground "orange"))
                  (".*Do not"   (:foreground "red"   ))
                  (".*Idle"     (:foreground "orange"))
                  ;; Above for bitlbee blist command.
                  ("wanderlust" (:foreground "green" ))
                  ("org-mode"   (:foreground "green" ))
                  (" erc "      (:foreground "green" ))
                  ("zenshinji"  (:foreground "red"   ))
                  )
   )
  )

(use-package erc-image
  :ensure t
  :defer t
  :config
  (add-to-list 'erc-modules 'image)
  (erc-update-modules)
  (setq
   erc-image-inline-rescale '32
   )
  )

(use-package erc-hl-nicks
  :ensure t
  :defer t
  )

(use-package erc-youtube
  :ensure t
  :defer t
  :config
  (add-to-list 'erc-modules 'youtube)
  (erc-update-modules)
  (setq
   erc-youtube-apiv3-key "AIzaSyAv0vJqSR7LOqmlwnAYz9Wxcvae4vDCB8g"
   )
  )

(use-package smiley
  :defer t
  :config
  (add-to-list 'erc-modules 'smiley)
  )

(use-package erc-imenu
  :defer t
  :disabled t
  )

(defun irc-fn ()
  "Connect to FreeNode IRC."
  (interactive)
  (erc-tls :server "irc.freenode.net"  :port 6697)
  )

(defun irc-fn-windows ()
  (interactive)
  (select-frame (make-frame '((name . "Emacs Freenode IRC")
 			      (minibuffer . t))))
  (split-window-horizontally)
  (windmove-right)
  (split-window-vertically)
  (windmove-down)
  (split-window-vertically)
  (windmove-down)
  (switch-to-buffer "#erc")
  (windmove-up)
  (switch-to-buffer "#gnus")
  (windmove-up)
  (switch-to-buffer "#org-mode")
  (windmove-left)
  (switch-to-buffer "#emacs")
)

(defun irc-ts ()
  "Connect to Thinstack IRC."
  (interactive)
  (erc-tls :server "irc.thinstack.net" :port 6697)
  )

(defun irc-cr ()
  "Connect to Chatrealm IRC."
  (interactive)
  (erc-tls :server "irc.chatrealm.net" :port 6697)
  )

(defun irc-bb ()
  "Connect to BitlBee local IRC."
  (interactive)
  (erc :server "localhost" :port 6667 :nick "pjorgensen")
  )

(use-package erc-tweet
  :ensure t
  :defer t
  :config
  (add-to-list 'erc-modules 'tweet)
  (erc-update-modules)
  )

(use-package xahk-mode
  :ensure t
  :mode ("\\.ahk\\'" . xahk-mode)
  )

(use-package fountain-mode
  :ensure t
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(add-hook 'markdown-mode-hook
          (lambda()
            (variable-pitch-mode t)
            (visual-line-mode    1)
            (flyspell-mode       1)
            )
          )

(load "~/.emacs.secrets.gpg" t)

;;; Post initialization

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
					    emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))
  
  (add-hook 'after-init-hook
	    `(lambda ()
	       (let ((elapsed (float-time (time-subtract (current-time)
							 emacs-start-time))))
		 (message "Loading %s...done (%.3fs) [after-init]"
			  ,load-file-name elapsed)))
	    t)
  )
