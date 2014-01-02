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
    ;; ace-jump-mode
    ;; ag
    ;; auto-complete
    ;; base16-theme
    ;; browse-kill-ring
    ;; clojure-mode
    ;; company
    ;; deft
    ;; diminish
    ;; dired+
    ;; evil
    ;; expand-region
    ;; flx
    ;; company
    ;; git-commit-mode
    ;; gist
    ;; magit
    ;; multiple-cursors
    ;; rainbow-delimiters
    ;; smartparens
    ;; smex
    ;; undo-tree
    ;; dropdown-list
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


;;;; PowerLine Setup
(require 'powerline)
(setq powerline-default-separator 'butt)
(powerline-reset)
(powerline-default-theme)


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
(blink-cursor-mode nil)
(column-number-mode t)
                                        ;(desktop-save-mode t)
(global-auto-revert-mode t)
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
 coffee-tab-width 2
 cua-enable-cua-keys nil
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
 global-auto-revert-non-file-buffers t
 ibuffer-expert t
 ibuffer-show-empty-filter-groups nil
 imenu-auto-rescan t
 indent-tabs-mode nil
 indicate-empty-lines t
 ispell-extra-args (quote ("--sug-mode=ultra"))
 ispell-program-name "aspell"
 line-spacing 0
 locale-coding-system 'utf-8
 mode-line-in-non-selected-windows t
 mode-line-inverse-video t
 mouse-wheel-scroll-amount (quote (0.01))
 mouse-yank-at-point t
 ns-alternate-modifier (quote super)
 ns-command-modifier (quote meta)
 ns-function-modifier (quote hyper)
 ns-pop-up-frames nil
 ns-tool-bar-display-mode 'both
 ns-tool-bar-size-mode 'regular
 recentf-max-saved-items 100
 redisplay-dont-pause t
 ring-bell-function 'ignore
 save-place t
 save-place-file "~/.emacs.d/places"
 scroll-conservatively 5
 scroll-margin 5
 send-mail-function (quote mailclient-send-it)
 sentence-end-double-space nil
 set-mark-command-repeat-pop t
 shift-select-mode nil
 show-paren-style 'mixed
 split-height-threshold nil
 split-width-threshold 159
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)"
 tramp-remote-path '(tramp-default-remote-path tramp-own-remote-path "/bin" "/usr/bin" "/usr/sbin" "/usr/local/bin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin")
 undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undo/")))
 uniquify-buffer-name-style 'forward
 uniquify-ignore-buffers-re "^\\*"
 uniquify-separator " • "
 user-full-name "Anders Bach Madsen"
 user-mail-address "anders@bachmadsen.net"
 visible-bell nil
 whitespace-style '(face tabs trailing lines-tail newline indentation empty space-after-tab)
 whitespace-style '(face tabs trailing lines-tail newline empty space-after-tab))

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



(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))

;;; init.el ends here

;; Local Variables:
;; time-stamp-start: "Updated: +"
;; time-stamp-end: "$"
;; End:
