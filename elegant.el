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

;; Appeareance
;; -------------------------------------------------------------------
(set-face-font 'default "Roboto Mono Light 14")
(setq default-frame-alist
      (append (list '(width  . 72)
                    '(height . 41)
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

(defface face-popout  '((t :foreground "#ffa07a"))  "Popout")
(defface face-strong  '((t :weight regular))        "Strong")
(defface face-salient '((t :foreground "#00008b")) "Salient")
(defface face-faded   '((t :foreground "#999999"))   "Faded")
(defface face-subtle  '((t :background "#f0f0f0"))  "Subtle")

;; General 
;; -------------------------------------------------------------------
(set-face 'header-line-highlight                          'face-faded)
(set-face 'region                                        'face-subtle)
(set-face 'highlight                                     'face-subtle)
(set-face 'org-link                                     'face-salient)
(set-face 'org-verbatim                                 'face-salient)
(set-face 'bold                                          'face-strong)
(set-face 'italic                                         'face-faded)
(set-face 'cursor                                        'face-strong)
(set-face-attribute 'cursor nil
                           :background (face-foreground 'face-strong))
(set-face 'minibuffer-prompt                             'face-strong)
(set-face 'link                                         'face-salient)
(set-face 'fringe                                         'face-faded)
(set-face 'isearch                                       'face-strong)
(set-face 'lazy-highlight                                'face-subtle)
(set-face 'show-paren-match                              'face-popout)
(set-face 'show-paren-mismatch                           'face-normal)
(set-face 'shadow                                         'face-faded)
(set-face 'warning                                       'face-popout)
(set-face 'outline-1                                     'face-strong)
(set-face 'outline-2                                     'face-strong)
(set-face 'outline-3                                     'face-strong)
(set-face 'outline-4                                     'face-strong)
(set-face 'outline-5                                     'face-strong)
(set-face 'outline-6                                     'face-strong)

;; Programmation mode
;; -------------------------------------------------------------------
(set-face 'font-lock-comment-face                         'face-faded)
(set-face 'font-lock-doc-face                             'face-faded)
(set-face 'font-lock-string-face                         'face-popout)
(set-face 'font-lock-constant-face                       'face-popout)
(set-face 'font-lock-warning-face                        'face-popout)
(set-face 'font-lock-function-name-face                  'face-strong)
(set-face 'font-lock-variable-name-face                  'face-strong)
(set-face 'font-lock-builtin-face                       'face-salient)
(set-face 'font-lock-type-face                          'face-salient)
(set-face 'font-lock-keyword-face                       'face-salient)

;; Interface
;; -------------------------------------------------------------------
(require 'cus-edit)
(set-face 'widget-field                                  'face-subtle)
(set-face 'custom-group-subtitle                         'face-strong)
(set-face 'custom-group-tag                              'face-strong)
(set-face 'custom-face-tag                               'face-strong)
(set-face 'custom-variable-tag                           'face-strong)
(set-face 'custom-visibility                             'face-popout)
(set-face 'custom-state                                 'face-salient)
(set-face 'custom-link                                  'face-salient)
(set-face-attribute 'custom-button nil
                    :foreground (face-foreground 'face-subtle)
                    :background (face-background 'face-subtle)
                    :box `(:line-width 1
                           :color ,(face-foreground 'face-faded)
                           :style nil)
                    :inverse-video nil)
(set-face-attribute 'custom-button-mouse nil
                    :inherit 'face-subtle
                    :box `(:line-width 1
                           :color ,(face-foreground 'face-subtle)
                           :style nil))
(set-face-attribute 'custom-button-pressed nil
                    :inherit 'face-salient
                    :box `(:line-width 1
                           :color ,(face-foreground 'face-salient)
                           :style nil)
                    :inverse-video t)

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

