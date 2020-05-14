;; A very minimal but elegant theme
;; Copyright 2020 Nicolas P. Rougier
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>

;; Only necessary for the splash screen mockup
;; -------------------------------------------------------------------
(with-eval-after-load 'org
  (setq org-display-inline-images t)
  (setq org-redisplay-inline-images t)
  (setq org-startup-with-inline-images "inlineimages")
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-elisp-link-function nil)
  (setq org-ellipsis " …")
  (setq org-link-frame-setup '((file . find-file))))
;; -------------------------------------------------------------------


;; Default font and frame size
(set-face-font 'default "Roboto Mono Light 14")
(setq default-frame-alist
      (append (list '(width  . 73) '(height . 41)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 24)
                    '(font . "Roboto Mono Light 14"))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 24)

;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 0)

;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Line cursor and no blink
(set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode 0)

;; Small fringe on the right only
(fringe-mode '(0 . 6))

;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset a face and make it inherit style."
  (set-face-attribute face nil
   :foreground 'unspecified :background 'unspecified
   :family     'unspecified :slant      'unspecified
   :weight     'unspecified :height     'unspecified
   :underline  'unspecified :overline   'unspecified
   :box        'unspecified :inherit    style))


;; Default face
(set-background-color "#ffffff")
(set-foreground-color "#333333")
(setq frame-background-mode 'light)

(defface face-critical '((t :foreground "#ffffff"
                            :background "#ff6347"))
"Critical face is for information that requires immediate action
or attention. It should be of high constrast when compared to
other faces. This can be realized (for example) by setting an
intense background color, typically a shade of red.")

(defface face-popout '((t :foreground "#ffa07a"))
"Popout face is used for information that need to attract
attention. To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect.")

(defface face-strong '((t :weight regular))
"Strong face is used for information of a structural nature like
for example titles, keywords, directory, etc. It has to be the
same color as the default color and only the weight differs by
one level (e.g., light/regular or regular/bold).")

(defface face-salient '((t :foreground "#00008b"))
"Salient face is used for information that are more important
than the others while being of the same nature. It is made by
using a different hue with approximately the same intensity as
the default face.")

(defface face-faded '((t :foreground "#999999"))
"Faded face is for information that are less important than the
others while being of the same nature. It is made by using the
same hue as the default but with a smaller intensity than the
default. It can be used for comments, secondary information and
also replace italic (which is generally abused anyway).")

(defface face-subtle '((t :background "#f0f0f0"))
"Subtle face is used to suggest a physical area on the screen
without disturbing the reading of information. This can be made
by setting a very light background color that is barely perceptible.")


;; Structural
(set-face 'bold                                          'face-strong)
(set-face 'italic                                         'face-faded)
(set-face 'bold-italic                                   'face-strong)
(set-face 'region                                        'face-subtle)
(set-face 'highlight                                     'face-subtle)
(set-face 'fixed-pitch                                       'default)
(set-face 'fixed-pitch-serif                                 'default)
(set-face 'variable-pitch                                    'default)
(set-face 'cursor                                            'default)
(set-face-attribute 'cursor nil
                               :background (face-foreground 'default))

;; Modeline
(set-face-attribute 'mode-line nil
		    :height .85
                    :foreground (face-background 'default)
                    :background (face-foreground 'default)
		    :box `(:line-width 2
                           :color ,(face-foreground 'default)
			   :style nil))

(set-face 'mode-line-highlight 'face-popout)
(set-face 'mode-line-emphasis  'face-strong)
(set-face 'mode-line-buffer-id 'face-strong)
(set-face-attribute 'mode-line-inactive nil
		    :height .85
                    :foreground (face-foreground 'face-faded)
                    :background (face-background 'face-subtle)
		    :box `(:line-width 2
                           :color ,(face-background 'face-subtle)
			   :style nil))

;; Semantic
(set-face 'shadow                                         'face-faded)
(set-face 'success                                      'face-salient)
(set-face 'warning                                       'face-popout)
(set-face 'error                                       'face-critical)

;; General
(set-face 'header-line                    '(face-salient face-strong))
(set-face 'buffer-menu-buffer                            'face-strong)
(set-face 'minibuffer-prompt                             'face-strong)
(set-face 'link                                         'face-salient)
(set-face 'fringe                                         'face-faded)
(set-face 'isearch                                       'face-strong)
(set-face 'isearch-fail                                   'face-faded)
(set-face 'lazy-highlight                                'face-subtle)
(set-face 'trailing-whitespace                           'face-subtle)
(set-face 'show-paren-match                              'face-popout)
(set-face 'show-paren-mismatch                           'face-normal)
(set-face-attribute 'tooltip nil                         :height 0.85)

;; Programmation mode
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
(with-eval-after-load 'info
  (set-face 'info-menu-header                            'face-strong)
  (set-face 'info-header-node                            'face-normal)
  (set-face 'Info-quoted                                  'face-faded)
  (set-face 'info-title-1                                'face-strong)
  (set-face 'info-title-2                                'face-strong)
  (set-face 'info-title-3                                'face-strong)
  (set-face 'info-title-4                               'face-strong))

;; Message
(with-eval-after-load 'message
  (set-face 'message-cited-text                           'face-faded)
  (set-face 'message-header-cc                               'default)
  (set-face 'message-header-name                         'face-strong)
  (set-face 'message-header-newsgroups                       'default)
  (set-face 'message-header-other                            'default)
  (set-face 'message-header-subject                     'face-salient)
  (set-face 'message-header-to                          'face-salient)
  (set-face 'message-header-xheader                          'default)
  (set-face 'message-mml                                 'face-popout)
  (set-face 'message-separator                           'face-faded))

;; Interface
(with-eval-after-load 'cus-edit
  (set-face 'widget-field                                'face-subtle)
  (set-face 'widget-button                               'face-strong)
  (set-face 'widget-single-line-field                    'face-subtle)
  (set-face 'custom-group-subtitle                       'face-strong)
  (set-face 'custom-group-tag                            'face-strong)
  (set-face 'custom-group-tag-1                          'face-strong)
  (set-face 'custom-comment                               'face-faded)
  (set-face 'custom-comment-tag                           'face-faded)
  (set-face 'custom-changed                             'face-salient)
  (set-face 'custom-modified                            'face-salient)
  (set-face 'custom-face-tag                             'face-strong)
  (set-face 'custom-variable-tag                         'face-strong)
  (set-face 'custom-invalid                              'face-popout)
  (set-face 'custom-visibility                           'face-popout)
  (set-face 'custom-state                               'face-salient)
  (set-face 'custom-link                                'face-salient)
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
                      :inverse-video nil))

;; Package
(with-eval-after-load 'package
  (set-face 'package-description                             'default)
  (set-face 'package-help-section-name                       'default)
  (set-face 'package-name                               'face-salient)
  (set-face 'package-status-avail-obso                    'face-faded)
  (set-face 'package-status-available                        'default)
  (set-face 'package-status-built-in                    'face-salient)
  (set-face 'package-status-dependency                  'face-salient)
  (set-face 'package-status-disabled                      'face-faded)
  (set-face 'package-status-external                         'default)
  (set-face 'package-status-held                             'default)
  (set-face 'package-status-incompat                      'face-faded)
  (set-face 'package-status-installed                   'face-salient)
  (set-face 'package-status-new                              'default)
  (set-face 'package-status-unsigned                         'default)

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
                            :background "#F0F0F0")
                         'link)))
      (apply #'insert-text-button button-text
             'face button-face 'follow-link t
             properties)))
  )

;; Flyspell
(with-eval-after-load 'flyspell
  (set-face 'flyspell-duplicate                         'face-popout)
  (set-face 'flyspell-incorrect                         'face-popout))

;; Ido 
(with-eval-after-load 'ido
  (set-face 'ido-first-match                            'face-salient)
  (set-face 'ido-only-match                               'face-faded)
  (set-face 'ido-subdir                                 'face-strong))

;; Diff
(with-eval-after-load 'diff-mode
  (set-face 'diff-header                                  'face-faded)
  (set-face 'diff-file-header                            'face-strong)
  (set-face 'diff-context                                    'default)
  (set-face 'diff-removed                                 'face-faded)
  (set-face 'diff-changed                                'face-popout)
  (set-face 'diff-added                                 'face-salient)
  (set-face 'diff-refine-added            '(face-salient face-strong))
  (set-face 'diff-refine-changed                         'face-popout)
  (set-face 'diff-refine-removed                          'face-faded)
  (set-face-attribute     'diff-refine-removed nil :strike-through t))

;; Term
(with-eval-after-load 'term
  (setq eterm-256color-disable-bold nil)
  (set-face 'term-bold                                   'face-strong)
  (set-face-attribute 'term-color-black nil
                                :foreground (face-foreground 'default)
                               :background (face-foreground 'default))
  (set-face-attribute 'term-color-white nil
                              :foreground "white" :background "white")
  (set-face-attribute 'term-color-blue nil
                          :foreground "#42A5F5" :background "#BBDEFB")
  (set-face-attribute 'term-color-cyan nil
                          :foreground "#26C6DA" :background "#B2EBF2")
  (set-face-attribute 'term-color-green nil
                          :foreground "#66BB6A" :background "#C8E6C9")
  (set-face-attribute 'term-color-magenta nil
                          :foreground "#AB47BC" :background "#E1BEE7")
  (set-face-attribute 'term-color-red nil
                          :foreground "#EF5350" :background "#FFCDD2")
  (set-face-attribute 'term-color-yellow nil
                         :foreground "#FFEE58" :background "#FFF9C4"))


;; Header and mode line
(set-face-attribute 'header-line nil
		    :height 140
                    :underline t
                    :underline "black"
		    :weight 'light
                    :foreground "black"
		    :background "white"
                    :box `(:line-width 3 :color "white" :style nil))
(set-face-attribute 'mode-line nil
                    :height 10
                    :underline "black"
                    :background "white"
		    :foreground "white"
                    :box nil)
(set-face 'mode-line-inactive 'mode-line)
(set-face 'mode-line-buffer-id 'default)
(set-face 'header-line-highlight 'face-faded)

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
    (propertize "%3l:%2c "
	'face `(:foreground ,(face-foreground 'face-faded)))))))


(provide 'elegance)
