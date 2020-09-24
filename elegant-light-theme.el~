;;; elegant-emacs-light-theme.el --- A very minimal but elegant and consistent theme
;;; Copyright (C) 2020 Nicolas P. Rougier
;;; -------------------------------------------------------------------
;;; Author: Nicolas P. Rougier
;;; URL: https://github.com/rougier/elegant-emacs
;;; -------------------------------------------------------------------
;;; Version: 0.1
;;; Package-Requires: ((emacs "25.1"))
;;; -------------------------------------------------------------------
;;; This theme was packaged by Nicolò Zorzetto <github.com/NicoloZorzetto>
;;; -------------------------------------------------------------------
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see <http://www.gnu.org/licenses/>
;;; -------------------------------------------------------------------
;;; Commentary:
;;; This theme offers an almost vanilla yet elegant Emacs experience
;;; -------------------------------------------------------------------
;;; Code:


;;; Theme definition
;;; -------------------------------------------------------------------
(deftheme elegant-emacs-light
  "A simple yet elegant theme for emacs.")
;;; -------------------------------------------------------------------


;;; Require elegant-emacs-common
;;; -------------------------------------------------------------------
; (load "~/.emacs.d/elegant-emacs/elegant-emacs-common.el")
(require 'elegant-emacs-common)
;;; -------------------------------------------------------------------


;;; General colors
;;; -------------------------------------------------------------------
(set-background-color "#ffffff")
(set-foreground-color "#333333")
(set-face-attribute 'default nil
		    :foreground (face-foreground 'default)
		    :background (face-background 'default))
(set-face-attribute 'face-critical nil :foreground "#ffffff"
		    :background "#ff6347")
(set-face-attribute 'face-popout nil :foreground "#ffa07a")
(set-face-attribute 'face-strong nil :foreground "#333333"
		    :weight 'regular)
(set-face-attribute 'face-salient nil :foreground "#00008b"
		    :weight 'light)
(set-face-attribute 'face-faded nil :foreground "#999999"
		    :weight 'light)
(set-face-attribute 'face-subtle nil :background "#f0f0f0")

(set-modeline-faces)

'(cus-edit (set-button-faces))
;;; -------------------------------------------------------------------


;;; Provide the elegant-emacs-dark theme
;;; -------------------------------------------------------------------
(provide-theme 'elegant-emacs-light)
;;; -------------------------------------------------------------------


;;; elegant-emacs-light-theme.el ends here
