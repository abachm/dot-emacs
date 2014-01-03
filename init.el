;;; init.el --- abachm configuration file

;; Turn off mouse interface early in startup to avoid momentary display
;(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please... jeez
(setq inhibit-startup-screen t)

;;;; package.el
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("quasi-melpa" . "http://quasi.milkbox.net/packages/") t)
(package-initialize)

(defvar abachm-packages
  '(
   powerline
   ido-ubiquitous
   ido-vertical-mode
   find-file-in-project
   magit
   yasnippet
   dropdown-list
   js2-mode
   mmm-mode
   git-commit-mode
   gist
   json-mode
   jedi
   python-mode
   color-theme-solarized
   ))

(defun abachm-install-packages ()
  "Install my list of packages."
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        abachm-packages))


;;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))


;;;; aliases
(defalias 'yes-or-no-p 'y-or-n-p)


;;;; PowerLine Setup
(require 'powerline)
(setq powerline-default-separator 'butt)
(powerline-reset)
(powerline-default-theme)


;;;; Color Theme Setup
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-solarized-dark)

;;;; External libraries
(require 'checkdoc)
(require 'midnight)
(require 'misc)
(require 'recentf)
(require 'saveplace)
(require 'uniquify)


;;;; random number generator
(random t)

;;;; Auto insert mode enabled
(auto-insert-mode t)


;;;; hooks
(add-hook 'write-file-functions 'time-stamp)


;;;; generic
(column-number-mode t)
(recentf-mode t)
(savehist-mode t)
(show-paren-mode t)
(visual-line-mode -1)
(winner-mode t)
(global-subword-mode t)
(delete-selection-mode t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(which-function-mode t)


;;;; the uncustomizable
(setq-default
 ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold]
 ansi-color-for-comint-mode t
 ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]
 auto-revert-verbose nil
 auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosave/" t)))
 backup-directory-alist (quote (("." . "~/.emacs.d/backups/")))
 backward-delete-char-untabify-method nil
 browse-url-generic-program "firefox"
 browse-url-browser-function 'browse-url-generic
 blink-matching-paren-on-screen t
 c-hungry-delete-key t
 c-default-style "bsd"
 coffee-tab-width 2
 custom-theme-directory "~/.emacs.d/themes/"
 delete-auto-save-files nil
 diff-switches "-u"
 dired-use-ls-dired nil
 echo-keystrokes 0.1
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 enable-recursive-minibuffers t
 flymake-gui-warnings-enabled t
 fringe-indicator-alist '((truncation left-arrow right-arrow)
                          (continuation nil right-curly-arrow)
                          (overlay-arrow . right-triangle)
                          (up . up-arrow)
                          (down . down-arrow)
                          (top top-left-angle top-right-angle)
                          (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
                          (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
                          (empty-line . empty-line)
                          (unknown . question-mark))
 font-lock-maximum-decoration t
 global-auto-revert-non-file-buffers t
 global-font-lock-mode t
 ibuffer-expert t
 ibuffer-show-empty-filter-groups nil
 imenu-auto-rescan t
 indent-tabs-mode nil
 indicate-empty-lines t
 ispell-extra-args (quote ("--sug-mode=ultra"))
 ispell-program-name "aspell"
 kill-whole-line t
 line-spacing 0
 locale-coding-system 'utf-8
 mode-line-in-non-selected-windows t
 mode-line-inverse-video t
 mouse-wheel-scroll-amount (quote (0.01))
 mouse-yank-at-point t
 next-line-add-newlines nil
 ns-alternate-modifier (quote super)
 ns-command-modifier (quote meta)
 ns-function-modifier (quote hyper)
 ns-pop-up-frames nil
 ns-tool-bar-display-mode 'both
 ns-tool-bar-size-mode 'regular
 recentf-max-saved-items 100
 redisplay-dont-pause t
 ring-bell-function 'ignore
 require-final-newline t
 save-place t
 save-place-file "~/.emacs.d/places"
 scroll-conservatively 5
 scroll-margin 5
 scroll-step 1
 send-mail-function (quote mailclient-send-it)
 sentence-end-double-space nil
 set-mark-command-repeat-pop t
 shift-select-mode nil
 show-paren-style 'mixed
 split-height-threshold nil
 split-width-threshold 159
 time-stamp-format "%02d-%3b-%:y %02H:%02M:%02S %u"
 tramp-remote-path '(tramp-default-remote-path tramp-own-remote-path "/bin" "/usr/bin" "/usr/sbin" "/usr/local/bin" "/local/bin")
 undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undo/")))
 uniquify-buffer-name-style 'forward
 uniquify-ignore-buffers-re "^\\*"
 uniquify-separator " • "
 user-full-name "Anders Bach Madsen"
 user-mail-address "anders@bachmadsen.net"
 visible-bell nil
 whitespace-style (quote (face trailing tabs lines empty indentaion::space)) ;; '(face tabs trailing lines-tail newline indentation empty space-after-tab)
 whitespace-line-column 89)


;;;; auto revert
(autoload 'auto-revert-mode "autorevert" nil t)
(autoload 'turn-on-auto-revert-mode "autorevert" nil nil)
(autoload 'global-auto-revert-mode "autorevert" nil t)
(global-auto-revert-mode t)


;;;; ido
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(setq ido-max-prospects 10)
(setq ido-read-file-name-non-ido nil)
(setq ido-use-filename-at-point nil)
(setq ido-use-virtual-buffers t)

(global-set-key (kbd "C-x f") 'find-file-in-project)
(define-key ctl-x-4-map (kbd "f") 'find-file-in-project-other-window)
(define-key ctl-x-4-map (kbd "s") 'shell-other-window)

(defun mp-ido-hook ()
  (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
  (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
  (define-key ido-completion-map [tab] 'ido-complete))

(add-hook 'ido-setup-hook 'mp-ido-hook)


;;;; ido-ubiquitous
(after "ido-ubiquitous-autoloads" (ido-ubiquitous-mode t))


;;;; ido-vertical-mode
(after "ido-vertical-mode-autoloads"
  (ido-vertical-mode t))


;;;; find-file-in-project
(after 'find-file-in-project
  (add-to-list 'ffip-patterns "*.c")
  (add-to-list 'ffip-patterns "*.java")
  (add-to-list 'ffip-patterns "*.xml")
  (add-to-list 'ffip-patterns "*.py")
  (add-to-list 'ffip-patterns "*.less")
  (add-to-list 'ffip-patterns "*.coffee")
  (add-to-list 'ffip-patterns "*.css")
  (add-to-list 'ffip-patterns "*.h"))


;;;; magit
(global-set-key (kbd "C-x g") 'magit-status)

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(after 'magit
  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))


;;;; ispell
(setq ispell-list-command "list")


;;;; diff commands
(add-to-list 'command-switch-alist '("-diff" . command-line-diff))


;;;; yas/snippets
(after 'yasnippet
  (yas/reload-all)
  (setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt yas/no-prompt)))

(after "yasnippet-autoloads"
  (add-hook 'prog-mode-hook 'yas-minor-mode))


;;;; js2-mode
(after "js2-mode-autoloads"
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;;;; mmm-mode
(after "mmm-mode-autoloads"
  (require 'mmm-auto)
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-js)
  (mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'embedded-css)
  )


;;;; prog-mode
(defun abachm-buffer-enable-whitespace-cleanup ()
  "enable whitespace-cleanup in the current buffer"
  (interactive)
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))

(defun abachm-buffer-disable-whitespace-cleanup ()
  "enable whitespace-cleanup in the current buffer"
  (interactive)
  (remove-hook 'before-save-hook 'whitespace-cleanup t))

(add-hook 'prog-mode-hook 'abachm-buffer-enable-whitespace-cleanup)
(add-hook 'prog-mode-hook 'whitespace-mode)


;;;; python-mode

;;;; jedi (python autocomplete mode)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional


;;;; functions

(defun other-window-reverse ()
  "Select the other window but in reverse."
  (interactive)
  (other-window -1))

(defun shell-other-window (&optional buffer)
  (interactive
   (list
    (and current-prefix-arg
         (prog1
             (read-buffer "Shell buffer: "
                          (generate-new-buffer-name "*shell*"))
           (if (file-remote-p default-directory)
               ;; It must be possible to declare a local default-directory.
               ;; FIXME: This can't be right: it changes the default-directory
               ;; of the current-buffer rather than of the *shell* buffer.
               (setq default-directory
                     (expand-file-name
                      (read-directory-name
                       "Default directory: " default-directory default-directory
                       t nil))))))))
  (let ((buffer (save-window-excursion
                  (shell buffer))))
    (switch-to-buffer-other-window buffer)))

(defun find-file-in-project-other-window ()
  "Find a file in the current project in the other window."
  (interactive)
  (let ((buffer (save-window-excursion (find-file-in-project))))
    (switch-to-buffer-other-window buffer)))

;;; customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(display-time-mode t)
 '(LaTeX-command "latex --shell-escape ")
 '(TeX-PDF-mode t)
 '(TeX-output-view-style (quote (("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^dvi$" ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$") "%(o?)xdvi %dS -paper a4r -s 0 %d") ("^dvi$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)xdvi %dS -paper a4 %d") ("^dvi$" ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$") "%(o?)xdvi %dS -paper a5r -s 0 %d") ("^dvi$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^dvi$" "." "%(o?)xdvi %dS %d") ("^pdf$" "." "evince %o %(outpage)") ("^html?$" "." "firefox %o"))))
 '(ansi-term-color-vector [unspecified "#081724" "#ff694d" "#68f6cb" "#fffe4e" "#bad6e2" "#afc0fd" "#d2f1ff" "#d3f9ee"])
 '(background-color "#202020")
 '(background-mode dark)
 '(cursor-color "#cccccc")
 '(custom-enabled-themes (quote (mustang)))
 '(custom-safe-themes (quote ("6e05b0a83b83b5efd63c74698e1ad6feaddf69a50b15a8b4a83b157aac45127c" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "f220c05492910a305f5d26414ad82bf25a321c35aa05b1565be12f253579dec6" "d0ff5ea54497471567ed15eb7279c37aef3465713fb97a50d46d95fe11ab4739" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "29a4267a4ae1e8b06934fec2ee49472daebd45e1ee6a10d8ff747853f9a3e622" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" "1cf3f29294c5a3509b7eb3ff9e96f8e8db9d2d08322620a04d862e40dc201fe2" "9ea054db5cdbd5baa4cda9d373a547435ba88d4e345f4b06f732edbc4f017dc3" "f48b43277382ee56a9e3c5d08b71c3639f401f6338da00039dcac3c21c0811b5" "b1e54397de2c207e550dc3a090844c4b52d1a2c4a48a17163cce577b09c28236" "bad832ac33fcbce342b4d69431e7393701f0823a3820f6030ccc361edd2a4be4" "7d090d4b15e530c28a35e084700aca464291264aad1fa2f2b6bba33dd2e92fd5" "b674ccba78eb688bea71ea8a1fd2782fcd69bd462e2504008903b5b6e018b480" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "af5f7d472ffeed85823d563512ce95e8ff7d648d8217d9ab79b17ae601b9a9d5" "7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "e57e7b19da7b4cd0e5512d5e9bc20d31c9cf50112c462de15a76bce0ea3c5ef5" "75d4ccc5e912b93f722e57cca3ca1a15e079032cd69fd9bc67268b4c85639663" "d971315c813b0269a86e7c5e73858070063016d9585492bd8d0f27704d50fee7" "450b29ed22abeeac279b7eeea592f4eea810105737716fc29807e1684e729c55" "1278386c1d30fc24b4248ba69bc5b49d92981c3476de700a074697d777cb0752" "d7f1c86b425e148be505c689fc157d96323682c947b29ef00cf57b4e4e46e6c7" "9f42bccce1e13fa5017eb8718574db099e85358b9f424db78e7318f86d1be08f" "5339210234ec915d7d3fd87bfeb506bfc436ff7277a55516ab1781ec85c57224" "eacfc96fbe418c017f4a00fdde5d5029db8e2800a46251eb2174484fa431917e" "3bd9497fb8f39c28ab58a9e957152ba2dc41223c23c5520ef10fc7bd6b222384" "fa189fcf5074d4964f0a53f58d17c7e360bb8f879bd968ec4a56dc36b0013d29" "47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" "98522c31200ec2ee2c84ae3dddac94e69730650096c3f4f751be4beece0f6781" "eef383086ae1f47f75cda814adfb9868a3dafa99b6eb67fdff780a52d326d468" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "465be5317c7d95a84e376e095c21242f4f2ad75692ed806dcbb6fe27078260f1" "f24462e7d2dcc5a7c6767ffc25cab208e6e3a963682b62bb2526eceb3899fff8" "f0283ec7a2385af8ea0fbb1466f340bbd0b2cededa60831394ec94e98096e1a8" "1d10043a0318d90a71cfa1f7e6b3b67f77c16ff9854f35a5b8722d87cb74f580" "b8f561a188a77e450ab8a060128244c81dea206f15c1152a6899423dd607b327" "1c1e6b2640daffcd23b1f7dd5385ca8484a060aec901b677d0ec0cf2927f7cde" "1a093e45e4c3e86fa5ad1f8003660e7cda4d961cd5d377cee3fee2dad2faf19b" "78cfbd96775588c06c4fff22573aaa5fa762ca2b8eda43cb964b7739194ed3c1" "88d556f828e4ec17ac074077ef9dcaa36a59dccbaa6f2de553d6528b4df79cbd" "76b9b3780c4844712e4a3ab05b8669eecd56a3864aae29e54005ffc68c24414c" "bb1eaf74fcfa24c8868078cac73abba4138d0ddb7f11f44d7e8f849edbf8b912" "383806d341087214fd44864170161c6bf34a41e866f501d1be51883e08cb674b" "a68fa33e66a883ce1a5698bc6ff355b445c87da1867fdb68b9a7325ee6ea3507" "466ae54a7b157ad02fd91da72b7871bccfb9bac98fdab95cf7a0d405c8572bd0" "88b663861db4767f7881e5ecff9bb46d65161a20e40585c8128e8bed8747dae5" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "446c73cdfb49f1dab4c322e51ac00a536fb0e3cb7e6809b9f4616e0858012e92" "9562e9eb5fd01677ac6b76b66f9a81338991fa9d270270802eeae5232c8da0a6" "6f3060ac8300275c990116794e1ba897b6a8af97c51a0cb226a98759752cddcf" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" default)))
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#444444")
 '(foreground-color "#cccccc")
 '(frame-brackground-mode (quote dark))
 '(fringe-mode (quote (4 . 0)) nil (fringe))
 '(indicate-buffer-boundaries (quote left))
 '(linum-format " %7i ")
 '(overflow-newline-into-fringe t)
 '(safe-local-variable-values (quote ((eval when (and (buffer-file-name) (file-regular-p (buffer-file-name)) (string-match-p "^[^.]" (buffer-file-name))) (emacs-lisp-mode) (when (fboundp (quote flycheck-mode)) (flycheck-mode -1)) (unless (featurep (quote package-build)) (let ((load-path (cons ".." load-path))) (require (quote package-build)))) (package-build-minor-mode)) (eval when (and (buffer-file-name) (file-regular-p (buffer-file-name)) (string-match-p "^[^.]" (buffer-file-name))) (emacs-lisp-mode) (unless (featurep (quote package-build)) (let ((load-path (cons ".." load-path))) (require (quote package-build)))) (package-build-minor-mode)))))
 '(vc-annotate-background monokai-bg)
 '(vc-annotate-color-map (quote ((20 . monokai-fg-1) (40 . monokai-bg+2) (60 . monokai-red) (80 . monokai-red+1) (100 . monokai-orange) (120 . monokai-orange+1) (140 . monokai-green) (160 . monokai-green+1) (180 . monokai-yellow) (200 . monokai-yellow+1) (220 . monokai-blue) (240 . monokai-blue+1) (260 . monokai-purple) (280 . monokai-purple+1) (300 . monokai-cyan) (320 . monokai-cyan+1) (340 . monokai-magenta) (360 . monokai-magenta+1))))
 '(vc-annotate-very-old-color monokai-magenta)
 '(grep-command "grep -nir ")
 '(grep-find-command (quote ("find . -type f -exec grep -nH -e  {} +" . 34)))
 '(grep-find-template "find . <X> -type f <F> -exec grep <C> -nH -e <R> {} +")
 '(grep-highlight-matches (quote auto))
 '(grep-template "grep <X> <C> -nH -e <R> <F>")
 '(grep-use-null-device nil)
 '(printer-name "aar-cups01")
 '(python-guess-indent nil)
 '(python-indent 3)
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 3)
 '(blink-cursor-mode nil)
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 98 :width normal)))))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))

;;; init.el ends here

;; Local Variables:
;; time-stamp-start: "Updated: +"
;; time-stamp-end: "$"
;; End:
