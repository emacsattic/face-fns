;;; face-fns.el --- face manipulation functions

;; Copyright (C) 1995, 2000, 2002, 2005 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: face-fns.el,v 1.12 2006/09/23 03:14:09 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:

(defun disable-mode-font-lock (mode)
  "Register MODE as a major mode for which font-lock should not be enabled automatically."
  (interactive "aMajor mode: ")
  (cond ((not (boundp 'font-lock-global-modes)))
        ((eq font-lock-global-modes nil))
        ((eq font-lock-global-modes t)
         (setq font-lock-global-modes (list 'not mode)))
        ((consp font-lock-global-modes)
         (if (eq 'not (car font-lock-global-modes))
             (nconc font-lock-global-modes (list mode))
           (setq font-lock-global-modes (delq mode font-lock-global-modes))))))

(defun enable-mode-font-lock (mode)
  "Register MODE as a major mode for which font-lock should be enabled automatically."
  (interactive "aMajor mode: ")
  (cond ((not (boundp 'font-lock-global-modes)))
        ((eq font-lock-global-modes nil)
         (setq font-lock-global-modes (list mode)))
        ((eq font-lock-global-modes t))
        ((consp font-lock-global-modes)
         (if (eq 'not (car font-lock-global-modes))
             (setq font-lock-global-modes (delq mode font-lock-global-modes))
           (nconc font-lock-global-modes (list mode))))))

;; Emacsen prior to emacs 19.29 did not have facep.
(or (fboundp 'facep)
    (defun facep (x)
      "Return t if X is a face name."
      (and (memq x (face-list)) t)))

;; It would be nice if there were an efficient way to resolve the hex
;; values to a readable name if one exists.
;;;###autoload
(defun make-less-bright-color (color &optional factor frame)
  "Return a color name which is 1/FACTOR as bright as COLOR.
If FACTOR is not specified, the value 2 is assumed."
  (let ((values (color-values color frame)))
    (and values
         (concat "#" (mapconcat
                      (lambda (x) (format "%02x" x))
                      (mapcar (lambda (x) (/ (% x 256) (or factor 2)))
                              (color-values color))
                      "")))))

;;;###autoload
(defun delete-face-attributes (face &optional frame &rest properties)
  "From FACE, remove PROPERTIES on FRAME.
If FRAME is nil or not specified \(i.e. the next arg after FACE is not
a frame object\), all current and future frames are affected."
  (unless (or (null frame) (framep frame))
    (setq properties (cons frame properties)
          frame nil))
  (let ((attrlist nil)
        ;; Setting a property to `unspecified' works to remove the attribute
        ;; in emacs 21 (unless they are required; for example you cannot
        ;; remove the :background property from the `default' face).
        ;; Earlier versions do not have set-face-attribute
        ;; but in most cases do the correct thing with a value of `nil'.
        (delvalue (if (fboundp 'set-face-attribute) 'unspecified nil)))
    (while properties
      (setq attrlist (cons (car properties) (cons delvalue attrlist)))
      (setq properties (cdr properties)))
    (apply 'override-face-attributes face frame attrlist)))

(defvar override-face-attributes-settor-alist
  '((:background    . set-face-background)
    (:bold          . set-face-bold-p)
    (:font          . set-face-font)
    (:foreground    . set-face-foreground)
    (:inverse-video . set-face-inverse-video-p)
    (:italic        . set-face-italic-p)
    (:stipple       . set-face-stipple)
    (:underline     . set-face-underline-p))
  "Mapping between face attribute keywords and old-style settor functions.
These are used by `override-face-attributes' in versions of emacs that lack
`set-face-attribute'.")

;;;###autoload
(defun override-face-attributes (face &optional frame &rest properties)
  "Set FACE on FRAME according to PROPERTIES.
If FRAME is nil or not specified, all current and future frames are
 affected.
If FRAME is actually a display spec, the associated
 properties are assigned to the face using `custom-set-faces'.

PROPERTIES is a sequence of the form \(:attr1 value1 :attr2 value2 ...\)
in the same manner as the args to `set-face-attribute'."
  (unless (or (null frame) (consp frame) (framep frame))
    (setq properties (cons frame properties)
          frame nil))
  (cond ((not (facep face)))
        ((consp frame)
         (custom-set-faces (list face (mapcar (lambda (desc)
                                                (append desc properties))
                                              frame))
                           t))
        ((fboundp 'set-face-attribute)
         (apply 'set-face-attribute face frame properties))
        (t
         (let ((settors override-face-attributes-settor-alist))
           (while properties
             (funcall (or (cdr (assq (nth 0 properties) settors)) 'ignore)
                      face (nth 1 properties) frame)
             (setq properties (cdr (cdr properties))))))))

(put 'override-face-attributes 'lisp-indent-function 2)

(provide 'face-fns)

;;; face-fns.el ends here.
