;;; No splash screen please... jeez
(setq inhibit-startup-screen t)

;;;; package.el
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository

(package-initialize)


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq-default
 max-lisp-eval-depth 1000
 locale-coding-system 'utf-8
 read-process-output-max (* 1024 1024)
 user-full-name "Anders Madsen"
 user-mail-address "anders@uber.com")

;; load path
(defun emacs-d (path)
  (let ((user-dir
         (cond ((boundp 'user-init-dir) user-init-dir)  ; check if user-init-dir is defined
               ((boundp 'user-emacs-directory) user-emacs-directory) ; else if user-emacs-directory is defined
               (t "~/.emacs.d/")))) ; else default
    (concat user-dir path)))

;; aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; Better defaults
(use-package better-defaults
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-material t)


  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

;; Windmove
(use-package windmove
  :bind (("C-x <left>" . windmove-left)
	 ("C-x <right>" . windmove-right)
	 ("C-x <up>" . windmove-up)
	 ("C-x <down>" . windmove-down))
  )

;; cua-mode
(use-package cua-base
  :init (cua-mode 1))

;; protobuf-mode
(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode)
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))

  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t))))

(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.tmpl\\'" . json-mode))
  :config (setq-default js-indent-level 2))

(use-package graphql-mode
  :ensure t)

(use-package jq-format
  :ensure t
  :after json-mode)

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; markdown
(use-package markdownfmt
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("\\.md$"        . markdown-mode)
         ("\\.mkd$"       . markdown-mode)
         ("\\.markdown$"  . markdown-mode)
         ("\\bREADME$$"   . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook #'markdownfmt-enable-on-save))

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package git-modes
  :ensure t)

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml$" . yaml-mode)
         ("\\.yml$" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook #'(lambda ()
                                (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package yaml-pro
  :ensure t
  :config
  (add-hook 'yaml-mode-hook #'yaml-pro-mode))

;; ivy, swiper and counsel
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind (("C-c C-r" . ivy-resume)))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c a" . counsel-ag)
         ("C-x l" . counsel-locate))
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-go-use-gofumpt t)
  ;; uncomment to enable gopls http debug server
  ;; :custom (lsp-gopls-server-args '("-debug" "127.0.0.1:0"))
  :commands (lsp lsp-deferred)
  :config (progn
            ;; use flycheck, not flymake
            (setq lsp-diagnostics-provider :flycheck)
            (setq lsp-enable-file-watchers nil)))

;; optional - provides fancy overlay information
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config (progn
            ;; disable inline documentation
            (setq lsp-ui-sideline-enable nil)
            ;; disable showing docs on hover at the top of the window
            (setq lsp-ui-doc-enable nil))
  )

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package company
  :ensure t
  :defer t
  :diminish (company-mode . " ⓐ")
  :init
  (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-idle-delay 0.2
        ;; min prefix of 2 chars
        company-minimum-prefix-length 2
        company-show-numbers 1
        company-require-match nil)
  ;; math
  (use-package company-math
    :ensure t
    :defer t
    :config
    (defun enable-math()
      (setq-local company-backends
                  (append '((company-math-symbols-latex company-latex-commands))
                          company-backends)))
    (add-hook 'text-mode-hook 'enable-math))
  )

;; snippets
(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :bind ((:map yas-keymap
               ("C-a" . my-yas/goto-start-of-active-field)
               ("C-e" . my-yas/goto-end-of-active-field)
               ;; Use C-t to expand snippet instead of conflicting <TAB>
               ("C-t" . yas-next-field-or-maybe-expand)
               ("C-T" . yas-next-field))
         (:map yas-minor-mode-map
               ("C-c C-y a" . yas-reload-all)))
  :init
  ;; set snippet directory
  (setq yas-snippet-dirs (list (emacs-d "snippets")))
  ;; turn on yasnippet everywhere
  (yas-global-mode 1)
  (yas-reload-all)
  :config
  ;; options
  (setq yas-indent-line 'fixed)
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t)
  :custom
  (add-hook 'company-mode-hook
            (lambda ()
              (substitute-key-definition
               'company-complete-common
               'company-yasnippet-or-completion
               company-active-map))))

(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand)))

(defun my-yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun my-yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

;; auto-yasnippet
;; hybrid of keyboard macro and yasnippet
(use-package auto-yasnippet
  :ensure t
  :commands (aya-create aya-expand)
  :bind (("C-c ~" . aya-create)
         ("C-c C-~" . aya-expand)))


(use-package go-mode
  :ensure t
  :preface
  (defun go-mode-config ()
    (whitespace-toggle-options '(tabs))
    (setq tab-width 2
          indent-tabs-mode 1)
    )
  (defun go-organize-imports ()
    (interactive)
    (if (and (string-prefix-p "~/projects/go-code/" (abbreviate-file-name (buffer-file-name))) (string= (file-name-extension buffer-file-name) "go"))
      (let ((gofmt-command "go-code-goimports"))
        (gofmt))
    (lsp-organize-imports)))
  :config
  (add-hook 'go-mode-hook (lambda () (go-mode-config)))
  :bind (
         ;; If you want to switch existing go-mode bindings to use lsp-mode/gopls instead
         ;; uncomment the following lines
         ;; ("C-c C-j" . lsp-find-definition)
         ;; ("C-c C-d" . lsp-describe-thing-at-point)
         )
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . go-organize-imports)))

(use-package gotest
  :ensure t
  :bind
  (:map go-mode-map
        ("C-c ."   . go-test-current-test)
        ("C-c f"   . go-test-current-file)
        ("C-c a"   . go-test-current-project)))

(use-package bazel
  :ensure t)

(use-package go-tag
  :ensure t
  :config (setq go-tag-args (list "-transform" "camelcase")))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package autorevert
  :config
  (setq auto-revert-interval 1)
  (global-auto-revert-mode))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 180) ;; limit line length
  (setq whitespace-style '(face trailing tabs lines empty indentaion::space)))

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (use-package py-isort
    :ensure t)
  (add-hook 'elpy-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'py-isort-before-save)
              )
            )
  (setq elpy-rpc-backend "jedi")
  (setq elpy-modules '(elpy-module-company
                       elpy-module-eldoc
                       elpy-module-pyvenv
                       elpy-module-yasnippet
                       elpy-module-sane-defaults)))

(use-package code-cells
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))
  )

;; default modes
(use-package org
  :ensure t
  :mode ("\\\.org\\\'" . org-mode)
;  :init
;  (setq initial-major-mode 'org-mode)
;  (setq org-startup-indented t)
;  (setq org-hide-leading-stars t)
;  (setq org-indent-indentation-per-level 2)
;  (setq org-startup-folded 'content)
;  :config
;  (setq-default major-mode 'org-mode)
;  (setq org-blank-before-new-entry '((heading . nil)
;                                     (plain-list-item . auto)))
;  ;; tag column
;  (setq org-tags-column -70)
;  ;; dependencies
;  (setq org-enforce-todo-dependencies t)
;  ;; make clock history persistent
;  (setq org-clock-persist 'history)
;  ;; Todo states
;  (setq org-todo-keywords
;        '((sequence "TODO(t)" "|" "WAITING(w)" "DONE(d)")))
;  (setq org-use-fast-todo-selection t)  ; C-c C-<char>
;  ;; priorities
;  (setq org-default-priority 67) ;C
;  ;; highlight math
;  (setf org-highlight-latex-and-related '(latex entities))
;
;  ;; Allow alphabetical lists
;  (setq org-alphabetical-lists t)
;
;  (defun my-org-metacontrolreturn ()
;    "Execute `org-meta-return' followed by `org-meta-right'.
;This usually makes new item indented one level deeper."
;    (interactive)
;    (org-meta-return)
;    (org-metaright))
;  (bind-key "<C-M-return>" 'my-org-metacontrolreturn) ; Decide whether useful only in org-mode
;
;  (defun my-org-make-numbered-list (beg end)
;    (interactive (if (use-region-p)
;                     (list (region-beginning) (region-end))
;                   (list
;                    (save-excursion (outline-previous-heading) (forward-line) (point))
;                    (save-excursion (outline-next-heading) (forward-line -1) (point)))))
;    (string-rectangle beg end "- ")
;    (beginning-of-line)
;    (org-call-with-arg 'org-cycle-list-bullet 'previous)
;    (org-call-with-arg 'org-cycle-list-bullet 'previous))
;
;  ;; org keys
;  (org-defkey org-mode-map (kbd "C-c 1") 'my-org-make-numbered-list)
;  (org-defkey org-mode-map (kbd "C-c x") 'org-export-dispatch)
;  ;; shortcut for C-u C-c C-l
;  (defun org-insert-file-link () (interactive) (org-insert-link '(4)))
;  (org-defkey org-mode-map (kbd "C-c l") 'org-store-link)
;
;  ;; some templates
;  (setcdr (assoc "c" org-structure-template-alist)
;          '("#+BEGIN_COMMENT\n?\n#+END_COMMENT"))
;  (add-to-list 'org-structure-template-alist
;               '("r"
;                 "#+BEGIN_SRC go\n?\n#+END_SRC"
;                 "<src lang=\"go\">\n\n</src>"))
;  (add-to-list 'org-structure-template-alist
;               '("p"
;                 "#+BEGIN_SRC python\n?\n#+END_SRC"
;                 "<src lang=\"python\">\n\n</src>"))
;
;  (setq org-default-notes-file "~/TODO.org")
;
;  ;; From scimax
;  ;; default with images open
;  (setq org-startup-with-inline-images "inlineimages")
;  ;; * Fragment overlays
;  (defun org-latex-fragment-tooltip (beg end image imagetype)
;    "Add the fragment tooltip to the overlay and set click function to toggle it."
;    (overlay-put (ov-at) 'help-echo
;                 (concat (buffer-substring beg end)
;                         "\nmouse-1 to toggle."))
;    (overlay-put (ov-at) 'local-map (let ((map (make-sparse-keymap)))
;                                      (define-key map [mouse-1]
;                                        `(lambda ()
;                                           (interactive)
;                                           (org-remove-latex-fragment-image-overlays ,beg ,end)))
;                                      map)))
;
;  (advice-add 'org--format-latex-make-overlay :after 'org-latex-fragment-tooltip)
;  (defun org-latex-fragment-justify (justification)
;    "Justify the latex fragment at point with JUSTIFICATION.
;JUSTIFICATION is a symbol for 'left, 'center or 'right."
;    (interactive
;     (list (intern-soft
;            (completing-read "Justification (left): " '(left center right)
;                             nil t nil nil 'left))))
;
;    (let* ((ov (ov-at))
;           (beg (ov-beg ov))
;           (end (ov-end ov))
;           (shift (- beg (line-beginning-position)))
;           (img (overlay-get ov 'display))
;           (img (and (and img (consp img) (eq (car img) 'image)
;                          (image-type-available-p (plist-get (cdr img) :type)))
;                     img))
;           space-left offset)
;      (when (and img (= beg (line-beginning-position)))
;        (setq space-left (- (window-max-chars-per-line) (car (image-display-size img)))
;              offset (floor (cond
;                             ((eq justification 'center)
;                              (- (/ space-left 2) shift))
;                             ((eq justification 'right)
;                              (- space-left shift))
;                             (t
;                              0))))
;        (when (>= offset 0)
;          (overlay-put ov 'before-string (make-string offset ?\ ))))))
;
;  (defun org-latex-fragment-justify-advice (beg end image imagetype)
;    "After advice function to justify fragments."
;    (org-latex-fragment-justify (or (plist-get org-format-latex-options :justify) 'left)))
;
;  (advice-add 'org--format-latex-make-overlay :after 'org-latex-fragment-justify-advice)
;
;  ;; ** numbering latex equations
;  (defun org-renumber-environment (orig-func &rest args)
;    "A function to inject numbers in LaTeX fragment previews."
;    (let ((results '())
;          (counter -1)
;          (numberp))
;
;      (setq results (loop for (begin .  env) in
;                          (org-element-map (org-element-parse-buffer) 'latex-environment
;                            (lambda (env)
;                              (cons
;                               (org-element-property :begin env)
;                               (org-element-property :value env))))
;                          collect
;                          (cond
;                           ((and (string-match "\\\\begin{equation}" env)
;                                 (not (string-match "\\\\tag{" env)))
;                            (incf counter)
;                            (cons begin counter))
;                           ((string-match "\\\\begin{align}" env)
;                            (prog2
;                                (incf counter)
;                                (cons begin counter)
;                              (with-temp-buffer
;                                (insert env)
;                                (goto-char (point-min))
;                                ;; \\ is used for a new line. Each one leads to a number
;                                (incf counter (count-matches "\\\\$"))
;                                ;; unless there are nonumbers.
;                                (goto-char (point-min))
;                                (decf counter (count-matches "\\nonumber")))))
;                           (t
;                            (cons begin nil)))))
;
;      (when (setq numberp (cdr (assoc (point) results)))
;        (setf (car args)
;              (concat
;               (format "\\setcounter{equation}{%s}\n" numberp)
;               (car args)))))
;
;    (apply orig-func args))
;
;  (advice-add 'org-create-formula-image :around #'org-renumber-environment)
;
;  (use-package org-bullets
;    :ensure t
;    :commands (org-bullets-mode)
;    :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  )

;(use-package org-modern
;  :ensure t
;  :config
;  (add-hook 'org-mode-hook #'org-modern-mode)
;  )

(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-superstar-mode)
  )

(use-package neotree
  :ensure t
  :bind ("<f8>" . 'neotree-toggle)
  :init
  ;; Every time when the neotree window is opened, let it find current file and jump to node
  (setq neo-smart-open t)
  ;; set icons theme
  (setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
  ;; show hidden files
  (setq-default neo-show-hidden-files t))

; web-mode with js/html/css

(use-package web-mode
  :ensure t
  :mode (("\\.jsx?$" . web-mode)
         ("\\.html$" . web-mode))
  :hook (web-mode . lsp-deferred)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (add-hook 'web-mode-hook 'electric-pair-mode)
  )

(use-package emmet-mode
  :diminish (emmet-mode . "ε")
  :bind* (("C-)" . emmet-next-edit-point)
          ("C-(" . emmet-prev-edit-point))
  :commands (emmet-mode
             emmet-next-edit-point
             emmet-prev-edit-point)
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  ;; Auto-start on any markup modes
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package add-node-modules-path
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'add-node-modules-path)
  )

(use-package prettier-js
  :ensure t)

(use-package chemtable
  :ensure t)

;; Customizations

(menu-bar-mode t)
(column-number-mode t)
(show-paren-mode t)
(delete-selection-mode t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(grep-command "grep -nir ")
 '(org-table-convert-region-max-lines 100000)
 '(package-selected-packages '(py-isort org-bullets org-pdfview gotest use-package)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JuliaMono" :foundry "UKWN" :slant normal :weight normal :height 95 :width normal)))))


(provide 'init)
;;; init.el ends here
