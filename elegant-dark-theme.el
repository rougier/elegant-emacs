;;; elegant-dark-theme.el --- A very minimal but elegant and consistent theme
;;; Copyright (C) 2020 Nicolas P. Rougier and Nicolò Zorzetto
;;; -------------------------------------------------------------------
;;; Authors: Nicolas P. Rougier and Nicolò Zorzetto
;;; -------------------------------------------------------------------
;;; URL: https://github.com/rougier/elegant-emacs
;;; -------------------------------------------------------------------
;;; Version: 1
;;; Package-Requires: ((emacs "25.1"))
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
(deftheme elegant-dark
  "A simple yet elegant theme for emacs.")
;;; -------------------------------------------------------------------


;;; Require elegant-emacs-common
;;; -------------------------------------------------------------------
; (load "~/.emacs.d/elegant/elegant.el")
(require 'elegant)
;;; -------------------------------------------------------------------


;;; General colors
;;; -------------------------------------------------------------------
(set-background-color "#3f3f3f")
(set-foreground-color "#dcdccc")
(set-face-attribute 'default nil
		    :foreground (face-foreground 'default)
		    :background (face-background 'default))
(set-face-attribute 'face-critical nil :foreground "#385f38"
		    :background "#f8f893")
(set-face-attribute 'face-popout nil :foreground "#f0dfaf")
(set-face-attribute 'face-strong nil :foreground "#dcdccc"
		    :weight 'regular)
(set-face-attribute 'face-salient nil :foreground "#dca3a3"
		    :weight 'light)
(set-face-attribute 'face-faded nil :foreground "#777767"
		    :weight 'light)
(set-face-attribute 'face-subtle nil :background "#4f4f4f")
(set-modeline-faces)
;;; -------------------------------------------------------------------


;;; Provide the elegant-emacs-dark theme
;;; -------------------------------------------------------------------
(provide-theme 'elegant-dark)
;;; -------------------------------------------------------------------


;;; elegant-dark-theme.el ends here

