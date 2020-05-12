;; -------------------------------------------------------------------
;; A very minimal but elegant emacs
;; Copyright 2020 Nicolas P. Rougier
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------


;; Some defaults
;; -------------------------------------------------------------------
(setq gc-cons-threshold 100000000)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq-default indent-tabs-mode nil)
(setq pop-up-windows nil)
(tool-bar-mode 0) (tooltip-mode  0) (scroll-bar-mode 0)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-x") 'execute-extended-command)
(defun custom/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)

(require 'package)
(add-to-list 'package-archives
            '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; OSX specific
;; -------------------------------------------------------------------
(eval-after-load "flyspell" ;; No button 2 on macbook
  '(progn
     (define-key flyspell-mouse-map
       (kbd "<C-down-mouse-3>") #'flyspell-correct-word)
     (define-key flyspell-mouse-map
       (kbd "<C-mouse-3>") 'undefined)))

;; Appeareance
;; -------------------------------------------------------------------
(set-face-font 'default "Roboto Mono Light 14")
(setq default-frame-alist
      (append (list '(width  . 73)
                    '(height . 41)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 24)
                    '(font . "Roboto Mono Light 14"))))
(set-frame-parameter
 (selected-frame) 'internal-border-width 24)
(setq-default line-spacing 0)
(set-default 'cursor-type  '(hbar . 2))
(blink-cursor-mode 0)
(fringe-mode '(0 . 0))
(setq x-underline-at-descent-line t)
(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

;; Org mode (for welcome screen)
;; -------------------------------------------------------------------
(require 'org)
(setq org-display-inline-images t)
(setq org-redisplay-inline-images t)
(setq org-startup-with-inline-images "inlineimages")
(setq org-hide-emphasis-markers t)
(setq org-confirm-elisp-link-function nil)
     
;; Theme: only 5 different faces, deal with it.
;; -------------------------------------------------------------------
(setq frame-background-mode 'light)
(set-background-color "#ffffff")
(set-foreground-color "#444444")

(defun set-face (face style)
  "Reset a face and make it inherit style."
  (set-face-attribute face nil
   :foreground 'unspecified :background 'unspecified
   :family     'unspecified :slant      'unspecified
   :weight     'unspecified :height     'unspecified
   :underline  'unspecified :overline   'unspecified
   :box        'unspecified :inherit    style))

(defface face-critical '((t :foreground "#ffffff"
                           :background "#ff6347")) "Critical")
(defface face-popout   '((t :foreground "#ffa07a"))  "Popout")
(defface face-strong   '((t :weight regular))        "Strong")
(defface face-salient  '((t :foreground "#00008b")) "Salient")
(defface face-faded    '((t :foreground "#999999"))   "Faded")
(defface face-subtle   '((t :background "#f0f0f0"))  "Subtle")

(defface face-display
  '((t :family "Fira Code" :inherit 'face-faded))  "Display")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'face-display))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'face-display))

;; General 
;; -------------------------------------------------------------------
(set-face 'header-line-highlight                          'face-faded)
(set-face 'region                                        'face-subtle)
(set-face 'highlight                                     'face-subtle)
(set-face 'org-link                                     'face-salient)
(set-face 'org-verbatim                                 'face-salient)
(set-face 'bold                                          'face-strong)
(set-face 'bold-italic                                   'face-strong)
(set-face 'buffer-menu-buffer                            'face-strong)
(set-face 'italic                                         'face-faded)
(set-face 'cursor                                        'face-strong)
(set-face-attribute 'cursor nil
                           :background (face-foreground 'face-strong))
(set-face 'minibuffer-prompt                             'face-strong)
(set-face 'link                                         'face-salient)
(set-face 'fringe                                         'face-faded)
(set-face 'isearch                                       'face-strong)
(set-face 'isearch-fail                                   'face-faded)
(set-face 'lazy-highlight                                'face-subtle)
(set-face 'show-paren-match                              'face-popout)
(set-face 'show-paren-mismatch                           'face-normal)
(set-face 'shadow                                         'face-faded)
(set-face 'warning                                       'face-popout)
(set-face 'error                                       'face-critical)
(set-face 'fixed-pitch                                       'default)
(set-face 'fixed-pitch-serif                                 'default)
(set-face 'outline-1                                     'face-strong)
(set-face 'outline-2                                     'face-strong)
(set-face 'outline-3                                     'face-strong)
(set-face 'outline-4                                     'face-strong)
(set-face 'outline-5                                     'face-strong)
(set-face 'outline-6                                     'face-strong)


;; Flyspell
;; -------------------------------------------------------------------
(require 'flyspell)
(set-face 'flyspell-duplicate                            'face-popout)
(set-face 'flyspell-incorrect                            'face-popout)

;; Programmation mode
;; -------------------------------------------------------------------
(set-face 'font-lock-comment-face                         'face-faded)
(set-face 'font-lock-doc-face                             'face-faded)
(set-face 'font-lock-string-face                         'face-popout)
(set-face 'font-lock-constant-face                      'face-salient)
(set-face 'font-lock-warning-face                        'face-popout)
(set-face 'font-lock-function-name-face                  'face-strong)
(set-face 'font-lock-variable-name-face                  'face-strong)
(set-face 'font-lock-builtin-face                       'face-salient)
(set-face 'font-lock-type-face                          'face-salient)
(set-face 'font-lock-keyword-face                       'face-salient)

;; Documentation
;; -------------------------------------------------------------------
(require 'info)
(set-face 'info-menu-header                              'face-strong)
(set-face 'info-header-node                              'face-normal)
(set-face 'Info-quoted                                    'face-faded)
(set-face 'info-title-1                                  'face-strong)
(set-face 'info-title-2                                  'face-strong)
(set-face 'info-title-3                                  'face-strong)
(set-face 'info-title-4                                  'face-strong)

;; Message
;; -------------------------------------------------------------------
(require 'message)
(set-face 'message-cited-text                             'face-faded)
(set-face 'message-header-cc                                 'default)
(set-face 'message-header-name                           'face-strong)
(set-face 'message-header-newsgroups                         'default)
(set-face 'message-header-other                              'default)
(set-face 'message-header-subject                       'face-salient)
(set-face 'message-header-to                                 'default)
(set-face 'message-header-xheader                            'default)
(set-face 'message-mml                                       'default)
(set-face 'message-separator                              'face-faded)

;; Speedbar
;; -------------------------------------------------------------------
(require 'speedbar)
(add-hook 'speedbar-mode-hook
     #'(lambda () (face-remap-add-relative  'default '(:height 120))))
(setq speedbar-use-images nil)
(set-face 'speedbar-button-face                           'face-faded)
(set-face 'speedbar-directory-face                      'face-salient)
(set-face 'speedbar-file-face                                'default)
(set-face 'speedbar-highlight-face                       'face-subtle)
(set-face 'speedbar-selected-face                        'face-popout)
(set-face 'speedbar-tag-face                              'face-faded)
(set-face 'speedbar-separator-face                        'face-faded)
(setq speedbar-frame-parameters
      '((minibuffer) (width . 32) (border-width . 0)
        (menu-bar-lines . 0)  (tool-bar-lines . 0)
        (unsplittable . t)  (left-fringe . 0)))


;; Interface
;; -------------------------------------------------------------------
(require 'cus-edit)
(set-face 'widget-field                                  'face-subtle)
(set-face 'widget-button                                 'face-strong)
(set-face 'custom-group-subtitle                         'face-strong)
(set-face 'custom-group-tag                              'face-strong)
(set-face 'custom-face-tag                               'face-strong)
(set-face 'custom-variable-tag                           'face-strong)
(set-face 'custom-visibility                             'face-popout)
(set-face 'custom-state                                 'face-salient)
(set-face 'custom-link                                  'face-salient)
(set-face-attribute 'custom-button nil
                    :foreground (face-foreground 'face-faded)
                    :background (face-background 'face-subtle)
                    :box `(:line-width 1
                           :color ,(face-foreground 'face-faded)
                           :style nil))
(set-face-attribute 'custom-button-mouse nil
                    :inherit 'face-subtle
                    :box `(:line-width 1
                           :color ,(face-foreground 'face-subtle)
                           :style nil))
(set-face-attribute 'custom-button-pressed nil
                    :foreground "white"
                    :background (face-foreground 'face-salient)
                    :inherit 'face-salient
                    :box `(:line-width 1
                           :color ,(face-foreground 'face-salient)
                           :style nil)
                    :inverse-video nil)


;; Package
;; -------------------------------------------------------------------
(require 'package)
(set-face 'package-description                                'default)
(set-face 'package-help-section-name                          'default)
(set-face 'package-name                                  'face-salient)
(set-face 'package-status-avail-obso                       'face-faded)
(set-face 'package-status-available                           'default)
(set-face 'package-status-built-in                       'face-salient)
(set-face 'package-status-dependency                     'face-salient)
(set-face 'package-status-disabled                         'face-faded)
(set-face 'package-status-external                            'default)
(set-face 'package-status-held                                'default)
(set-face 'package-status-incompat                         'face-faded)
(set-face 'package-status-installed                      'face-salient)
(set-face 'package-status-new                                 'default)
(set-face 'package-status-unsigned                            'default)

;; Button face is hardcoded, we have to redefine the relevant function
(defun package-make-button (text &rest properties)
  "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
  (let ((button-text (if (display-graphic-p)
                         text (concat "[" text "]")))
        (button-face (if (display-graphic-p)
                         '(:box `(:line-width 1
                           :color "#999999":style nil)
                           :foreground "#999999"
                           :background "#f0f0f0")
                       'link)))
    (apply #'insert-text-button button-text
           'face button-face 'follow-link t
           properties)))

;; Header and mode line
;; -------------------------------------------------------------------
(set-face-attribute 'header-line nil
		    :height 140
                    :underline t
                    :underline "black"
                    :foreground "black"
		    :background "white"
                    :box `(:line-width 3 :color "white" :style nil))
(set-face-attribute 'mode-line nil
                    :height 10
                    :underline "black"
                    :background "white"
		    :foreground "white"
                    :box nil)
(set-face 'mode-line-inactive  'mode-line)
(set-face 'mode-line-buffer-id  'default)

(defun mode-line-render (left right)
  "Return a string of `window-width' length containing left, and
   right aligned respectively."
  (let* ((available-width (- (window-total-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))
(define-key mode-line-major-mode-keymap [header-line]
  (lookup-key mode-line-major-mode-keymap [mode-line]))

(setq-default mode-line-format '(""))
(setq-default header-line-format
  '(:eval (mode-line-render
   (format-mode-line
    (list
     (propertize "☰"
                 'face `(:weight regular)
                 'mouse-face 'header-line-highlight
                 'help-echo  "Major mode menu"
                 'local-map   mode-line-major-mode-keymap)
     " %b "
     '(:eval (if (and buffer-file-name (buffer-modified-p))
                 (propertize "(modified)"
              'face `(:foreground ,(face-foreground 'face-faded)))))))
   (format-mode-line
    (propertize "%3l:%2c  "
	'face `(:foreground ,(face-foreground 'face-faded)))))))

