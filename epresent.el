;;; epresent.el --- Simple presentation mode for Emacs Org-mode

;; Copyright (C) 2008 Tom Tromey <tromey@redhat.com>
;;               2010 Eric Schulte <schulte.eric@gmail.com>
;;               2011 Tero Hasu <tero.hasu@hut.fi>

;; Authors: Tom Tromey <tromey@redhat.com>, 
;;          Eric Schulte <schulte.eric@gmail.com>,
;;          Tero Hasu <tero.hasu@hut.fi>
;; Created: 12 Jun 2008
;; Last modified: much later
;; Keywords: gui

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a simple presentation mode for Emacs.  It works best in
;; Emacs 23, which has nicer font rendering.

;; To use, invoke epresent-run in an org-mode buffer. Use n/p to
;; navigate, or q to quit. (There are some other key bindings too.)
;; Each top-level headline becomes a page in the presentation and
;; Org-mode markup is used to nicely display the buffer's contents.

;;; Code:
(require 'org)
(require 'org-exp)
(require 'org-latex)

(defface epresent-title-face
  '((t :weight bold :height 2.2 :underline t :inherit variable-pitch))
  "")
(defface epresent-heading-face
  '((t :weight bold :height 1.8 :underline t :inherit variable-pitch))
  "")
(defface epresent-subheading-face
  '((t :weight bold :height 1.6 :inherit variable-pitch))
  "")
(defface epresent-author-face
  '((t :height 1.6 :inherit variable-pitch))
  "")
(defface epresent-bullet-face
  '((t :weight bold :height 1.8 :underline nil :inherit variable-pitch))
  "")
(defface epresent-hidden-face
  '((t :invisible t))
  "")

(defvar epresent--frame nil
  "Frame for EPresent.")

(defvar epresent--org-buffer nil
  "Original Org-mode buffer")

(defvar epresent-text-scale 3)

(defvar epresent-overlays nil)

(defvar epresent-src-fontify-natively nil)
(defvar epresent-hide-emphasis-markers nil)
(defvar epresent-format-latex-scale nil)
(defvar epresent-hide-todos t)
(defvar epresent-hide-tags t)
(defvar epresent-hide-properties t)

(defvar epresent-frame-level 1)
(make-variable-frame-local 'epresent-frame-local) ;; Obsolete function?

(defun epresent--get-frame ()
  (unless (frame-live-p epresent--frame)
    (setq epresent--frame (make-frame '((minibuffer . nil)
                                        (title . "EPresent")
                                        ;;(fullscreen . fullboth)
                                        (menu-bar-lines . 0)
                                        (tool-bar-lines . 0)
                                        (vertical-scroll-bars . nil)
                                        (left-fringe . 0)
                                        (right-fringe . 0)
                                        (internal-border-width . 20)
                                        ;;(cursor-type . nil)
                                        ))))
  (raise-frame epresent--frame)
  (select-frame-set-input-focus epresent--frame)
  epresent--frame)

(defun epresent-goto-top-level ()
  "Go to the current top level heading containing point."
  (interactive)
  (unless (org-at-heading-p) 
    (outline-previous-heading))
  (let ((level (org-current-level)))
    (when (and level (> level 1))
      ;; Some things are invisible when EPresenting, hence pass "t".
      (outline-up-heading (- level 1) t))))

(defun epresent-current-page ()
  "Present the current outline heading."
  (when (org-current-level)
    (epresent-goto-top-level)
    (org-show-subtree)
    (org-narrow-to-subtree)))

(defun epresent-debug ()
  "Show debug information."
  (interactive)
  (message (format "lv:%s @h:%s" 
		   (org-current-level)
		   (org-at-heading-p))))

(defun epresent-top ()
  "Present the first outline heading."
  (interactive)
  (widen)
  (goto-char (point-min))
  (epresent-current-page))

(defun epresent-bottom ()
  "Present the last outline heading."
  (interactive)
  (beginning-of-line)
  (widen)
  (goto-char (point-max))
  (epresent-current-page))

(defun epresent-this-page ()
  "Present the current outline heading."
  (interactive)
  (beginning-of-line)
  (widen)
  (epresent-goto-top-level)
  (epresent-current-page))

(defun epresent-next-page ()
  "Present the next outline heading."
  (interactive)
  (beginning-of-line)
  (widen)
  (epresent-goto-top-level)
  (org-get-next-sibling)
  (epresent-current-page))

(defun epresent-previous-page ()
  "Present the previous outline heading."
  (interactive)
  (beginning-of-line)
  (widen)
  (epresent-goto-top-level)
  (or (org-get-last-sibling) (goto-char (point-min)))
  (epresent-current-page))

(defun epresent-clean-overlays ()
  (interactive)
  (mapc 'delete-overlay epresent-overlays)
  (setq epresent-overlays nil))

(defun epresent-quit ()
  "Quit the current presentation."
  (interactive)
  ;; restore the font size
  (text-scale-increase 0) ;; remove any active scaling
  (org-remove-latex-fragment-image-overlays)
  (org-remove-inline-images)
  ;; restore the user's Org-mode variables
  (setq org-src-fontify-natively epresent-src-fontify-natively)
  (setq org-hide-emphasis-markers epresent-hide-emphasis-markers)
  (plist-put org-format-latex-options :scale epresent-format-latex-scale)
  ;;(when (string= "EPresent" (frame-parameter nil 'title))
  ;;(delete-frame (selected-frame)))
  (when epresent--org-buffer
    (set-buffer epresent--org-buffer))
  (org-mode)
  (widen)
  ;; delete all epresent overlays
  (epresent-clean-overlays))

(defun epresent-increase-font ()
  "Increase the presentation font size."
  (interactive)
  (dolist (face
           '(epresent-heading-face epresent-content-face epresent-fixed-face))
    (set-face-attribute face nil :height (1+ (face-attribute face :height)))))

(defun epresent-decrease-font ()
  "Decrease the presentation font size."
  (interactive)
  (dolist (face
           '(epresent-heading-face epresent-content-face epresent-fixed-face))
    (set-face-attribute face nil :height (1- (face-attribute face :height)))))

(defun epresent-push-new-overlay (p0 p1 nm val)
  (let ((ol (make-overlay p0 p1)))
    (overlay-put ol nm val)
    (push ol epresent-overlays)))

(defun epresent-fontify ()
  "Overlay additional presentation faces to Org-mode."
  (save-excursion
    ;; hide all comments
    (goto-char (point-min))
    ;; xxx could we just temporarily modify the Org-mode face for comments
    (while (re-search-forward
            "^\\(#\\|[ \t]*#\\+\\).*\n"
            nil t)
      (let ((comment (match-string 0))
	    (cb (match-beginning 0))
	    (ce (match-end 0)))
	(unless (save-match-data 
		  ;; (regexp-opt '("title" "author" "date"))
		  (string-match 
		   "^#\\+\\(author\\|title\\|date\\):"
		   comment))
	  (epresent-push-new-overlay cb ce 'invisible 'epresent-hide))))

    ;; page title faces
    (goto-char (point-min))
    (while (re-search-forward "^\\(*+\\)[ \t]*\\(.*\\)$" nil t)
      (push (make-overlay (match-beginning 1) (match-end 1)) epresent-overlays)
      (overlay-put (car epresent-overlays) 'invisible 'epresent-hide)
      (push (make-overlay (match-beginning 2) (match-end 2)) epresent-overlays)
      (if (> (length (match-string 1)) 1)
          (overlay-put (car epresent-overlays) 'face 'epresent-subheading-face)
        (overlay-put (car epresent-overlays) 'face 'epresent-heading-face)))
    (goto-char (point-min))
    ;; fancy bullet points
    (while (re-search-forward "^[ \t]*\\(-\\) " nil t)
      (push (make-overlay (match-beginning 1) (match-end 1)) epresent-overlays)
      (overlay-put (car epresent-overlays) 'invisible 'epresent-hide)
      (overlay-put (car epresent-overlays)
                   'before-string (propertize "•" 'face 'epresent-bullet-face)))
    ;; hide todos
    (when epresent-hide-todos
      (goto-char (point-min))
      (while (re-search-forward org-todo-regexp nil t) 
        (push (make-overlay (match-beginning 1) (1+ (match-end 1)))
              epresent-overlays)
        (overlay-put (car epresent-overlays) 'invisible 'epresent-hide)))
    ;; hide tags
    (when epresent-hide-tags
      (goto-char (point-min))
      (while (re-search-forward 
              (org-re "^\\*+.*?\\([ \t]+:[[:alnum:]_@#%:]+:\\)[ \r\n]") 
              nil t)
        (push (make-overlay (match-beginning 1) (match-end 1)) epresent-overlays)
        (overlay-put (car epresent-overlays) 'invisible 'epresent-hide)))
    ;; hide properties
    (when epresent-hide-properties
      (goto-char (point-min))
      (while (re-search-forward org-drawer-regexp nil t)
        (let ((beg (match-beginning 0))
              (end (re-search-forward
                    "^[ \t]*:END:[ \r\n]*"
                    (save-excursion (outline-next-heading) (point)) t)))
          (push (make-overlay beg end) epresent-overlays)
          (overlay-put (car epresent-overlays) 'invisible 'epresent-hide))))
    (dolist (el '("title" "author" "date"))
      (goto-char (point-min))
      (when (re-search-forward (format "^\\(#\\+%s:\\)[ \t]*\\(.*\\)$" el) nil t)
        (push (make-overlay (match-beginning 1) (match-end 1)) epresent-overlays)
        (overlay-put (car epresent-overlays) 'invisible 'epresent-hide)
        (push (make-overlay (match-beginning 2) (match-end 2)) epresent-overlays)
        (overlay-put
         (car epresent-overlays) 'face (intern (format "epresent-%s-face" el)))))
    ;; inline images
    (unless org-inline-image-overlays
      (org-display-inline-images))))

(defvar epresent-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map " " 'epresent-next-page)
    (define-key map "b" 'epresent-bottom)
    (define-key map "c" 'epresent-this-page)
    (define-key map "d" 'epresent-debug)
    (define-key map "n" 'epresent-next-page)
    ;;(define-key map [right] 'epresent-next-page)
    (define-key map "p" 'epresent-previous-page)
    ;;(define-key map [left] 'epresent-previous-page)
    (define-key map [backspace] 'epresent-previous-page)
    (define-key map "q" 'epresent-quit)
    ;;(define-key map "1" 'epresent-top)
    (define-key map "t" 'epresent-top)
    map)
  "Local keymap for EPresent display mode.")

(define-derived-mode epresent-mode org-mode "EPresent"
  "Lalala."
  (text-scale-increase 0) ;; remove any active scaling
  (text-scale-increase epresent-text-scale)
  ;; make Org-mode be as pretty as possible
  (setq epresent-src-fontify-natively org-src-fontify-natively)
  (setq org-src-fontify-natively t)
  (setq epresent-hide-emphasis-markers org-hide-emphasis-markers)
  (setq org-hide-emphasis-markers t)
  (setq epresent-format-latex-scale (plist-get org-format-latex-options :scale))
  (setq org-format-latex-options
	(plist-put org-format-latex-options :scale 3.0))
  (org-preview-latex-fragment '(16))
  ;; fontify the buffer
  (add-to-invisibility-spec '(epresent-hide))
  ;; remove flyspell overlays
  (org-remove-flyspell-overlays-in (point-min) (point-max))
  (epresent-fontify))

;;;###autoload
(defun epresent-run ()
  "Present an Org-mode buffer."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "EPresent can only be used from Org Mode"))
  ;; For this to work we must do it early, but do not know why. If we
  ;; don't, and if C-c C-x C-l is active, we retain small size preview
  ;; images, and latex preview state is messed up upon presentation
  ;; exit. Possibly switching modes will change so much state that
  ;; this is no longer effective at that point.
  (org-remove-latex-fragment-image-overlays)
  (setq epresent--org-buffer (current-buffer))
  (epresent-mode))

(provide 'epresent)
;;; epresent.el ends here
