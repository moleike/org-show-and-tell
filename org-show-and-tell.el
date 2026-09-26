;;; org-show-and-tell.el --- A presentation mode for Org files with teleprompter support -*- lexical-binding: t; -*-

;; Author: Alexandre Moreno
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (org "9.5"))

;;; Commentary:
;; org-show-and-tell is an opinionated presentation mode for Org files. It
;; automatically generates title and agenda slides, syncs presenter notes to a
;; separate buffer (*Presenter Notes*), switches your theme for presentation,
;; adds parent breadcrumbs for subheadings, and includes built-in Evil
;; keybindings.

;;; Code:

(require 'org)

(defgroup org-show-and-tell nil
  "A presentation mode for Org mode."
  :group 'org)

;; ---------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------

(defcustom org-show-and-tell-title-slide t
  "If non-nil, automatically generate a title slide from #+TITLE: and #+AUTHOR: keywords."
  :type 'boolean)

(defcustom org-show-and-tell-agenda-slide t
  "If non-nil, automatically generate an Agenda/TOC slide before the content."
  :type 'boolean)

(defcustom org-show-and-tell-text-scale 2
  "Text scaling level during presentations."
  :type 'integer)

(defcustom org-show-and-tell-margin-width 12
  "Left and right window margin width in columns."
  :type 'integer)

(defcustom org-show-and-tell-top-margin 2
  "Number of blank lines to push the top slide down."
  :type 'integer)

(defcustom org-show-and-tell-theme 'doom-homage-white
  "Theme to apply during presentation (or nil to keep current)."
  :type '(choice (const :tag "Keep current theme" nil) symbol))

;; ---------------------------------------------------------------------
;; Faces
;; ---------------------------------------------------------------------

(defface org-show-and-tell-title-face
  '((t (:height 2.0 :weight bold)))
  "Face used for the main title and the Agenda header."
  :group 'org-show-and-tell)

(defface org-show-and-tell-author-face
  '((t (:inherit shadow :height 1.1 :slant italic)))
  "Face used for the author on the title slide."
  :group 'org-show-and-tell)

(defface org-show-and-tell-agenda-face
  '((t (:height 1.2)))
  "Face used for the enumerated agenda items."
  :group 'org-show-and-tell)

(defface org-show-and-tell-breadcrumb
  '((t (:inherit shadow :height 0.9 :slant italic :weight normal)))
  "Face used for the parent heading breadcrumb on sub-slides."
  :group 'org-show-and-tell)

;; ---------------------------------------------------------------------
;; Internal Variables
;; ---------------------------------------------------------------------

(defvar-local org-show-and-tell--slides nil)
(defvar-local org-show-and-tell--index 0)
(defvar-local org-show-and-tell--saved-state nil)
(defvar-local org-show-and-tell--top-margin-ov nil)
(defvar-local org-show-and-tell--overlays nil)
(defvar-local org-show-and-tell--slide-string "")
(defvar-local org-show-and-tell--saved-tilde-fringe nil)
(defvar-local org-show-and-tell--saved-evil-cursor nil)
(defvar-local org-show-and-tell--agenda-items nil)

;; ---------------------------------------------------------------------
;; Data Accessors & Helpers
;; ---------------------------------------------------------------------

(defsubst org-show-and-tell--slide-type (slide) (car slide))
(defsubst org-show-and-tell--slide-start (slide) (cadr slide))
(defsubst org-show-and-tell--slide-end (slide) (cddr slide))

(defun org-show-and-tell--margin-string ()
  "Generate top-margin newline string with neutral face properties."
  (propertize (make-string org-show-and-tell-top-margin ?\n)
              'face '(:height 1.0 :background unspecified)))

(defun org-show-and-tell--get-keyword (keyword)
  "Extract file-level keyword value like TITLE or AUTHOR."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward (format "^#\\+%s:\\s-*\\(.*\\)$" keyword) nil t)
          (string-trim (match-string-no-properties 1)))))))

(defun org-show-and-tell--build-agenda ()
  "Build an alist of (DISPLAY-TITLE . SLIDE-INDEX) for Level 1 headings."
  (let ((items nil)
        (count 0))
    (dotimes (idx (length org-show-and-tell--slides))
      (let ((slide (nth idx org-show-and-tell--slides)))
        (when (eq (org-show-and-tell--slide-type slide) 'slide)
          (save-excursion
            (goto-char (org-show-and-tell--slide-start slide))
            (when (looking-at "^\\* \\(.*\\)")
              (let ((title (string-trim (match-string-no-properties 1))))
                (unless (string-match-p "^\\(agenda\\|index\\|toc\\)$" (downcase title))
                  (setq count (1+ count))
                  (push (cons (format "%d. %s" count title) idx) items))))))))
    (nreverse items)))

(defun org-show-and-tell--generate-agenda-string ()
  "Generate agenda string"
  (if org-show-and-tell--agenda-items
      (mapconcat #'car org-show-and-tell--agenda-items "\n")
    "No content slides found."))

(defun org-show-and-tell--collect-slides ()
  "Scan buffer and return list of (TYPE START . END) tuples."
  (let ((points nil))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "^\\*\\{1,2\\} " nil t)
          (push (line-beginning-position) points))
        (setq points (nreverse points))
        
        (if (null points)
            nil
          (let ((slides nil)
                (max-pos (point-max))
                (first-heading (car points)))
            
            (when (and org-show-and-tell-title-slide
                       (org-show-and-tell--get-keyword "TITLE"))
              (push (cons 'title (cons (point-min) first-heading)) slides))

            (when org-show-and-tell-agenda-slide
              (push (cons 'agenda (cons (point-min) first-heading)) slides))

            (dotimes (i (length points))
              (let ((start (nth i points))
                    (next (if (< (1+ i) (length points))
                              (nth (1+ i) points)
                            max-pos)))
                (push (cons 'slide (cons start next)) slides)))
            
            (nreverse slides)))))))

(defun org-show-and-tell--get-parent-title ()
  "Lookup parent title for current slide if it is a Level 2 subheading."
  (save-excursion
    (save-restriction
      (widen)
      (let ((start (org-show-and-tell--slide-start (nth org-show-and-tell--index org-show-and-tell--slides))))
        (goto-char start)
        (when (and (org-at-heading-p) (> (org-outline-level) 1))
          (when (ignore-errors (org-up-heading-safe))
            (org-get-heading t t t t)))))))

;; ---------------------------------------------------------------------
;; Overlay Rendering Modules
;; ---------------------------------------------------------------------

(defun org-show-and-tell--clear-overlays ()
  "Clear all active slide overlays."
  (when org-show-and-tell--top-margin-ov
    (delete-overlay org-show-and-tell--top-margin-ov)
    (setq org-show-and-tell--top-margin-ov nil))
  (mapc #'delete-overlay org-show-and-tell--overlays)
  (setq org-show-and-tell--overlays nil))

(defun org-show-and-tell--hide-heading-stars ()
  "Hide leading asterisks on heading lines."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\)\\s-+" nil t)
      (let ((ov (make-overlay (match-beginning 1) (match-end 0))))
        (overlay-put ov 'display "")
        (push ov org-show-and-tell--overlays)))))

(defun org-show-and-tell--hide-block-tags ()
  "Hide #+begin_src, #+end_src, and #+begin_notes blocks."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+\\(begin\\|end\\)_src.*$" nil t)
        (let* ((end (match-end 0))
               (end (if (eq (char-after end) ?\n) (1+ end) end))
               (ov (make-overlay (line-beginning-position) end)))
          (overlay-put ov 'display "")
          (push ov org-show-and-tell--overlays)))

      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+begin_notes" nil t)
        (let ((beg (line-beginning-position)))
          (when (re-search-forward "^[ \t]*#\\+end_notes.*$" nil t)
            (let* ((end (match-end 0))
                   (end (if (eq (char-after end) ?\n) (1+ end) end))
                   (ov (make-overlay beg end)))
              (overlay-put ov 'display "")
              (push ov org-show-and-tell--overlays))))))))

(defun org-show-and-tell--apply-title-overlay (margin-str)
  "Render virtual Title slide overlay."
  (let ((title (or (org-show-and-tell--get-keyword "TITLE") "Presentation"))
        (author (or (org-show-and-tell--get-keyword "AUTHOR") ""))
        (ov (make-overlay (point-min) (point-max))))
    (overlay-put ov 'display
                 (concat margin-str
                         "\n"
                         (propertize title 'face 'org-show-and-tell-title-face)
                         "\n\n"
                         (propertize author 'face 'org-show-and-tell-author-face)
                         "\n"))
    (push ov org-show-and-tell--overlays)))

(defun org-show-and-tell--apply-agenda-overlay (margin-str)
  "Render virtual Agenda slide overlay."
  (let ((agenda-str (org-show-and-tell--generate-agenda-string))
        (ov (make-overlay (point-min) (point-max))))
    (overlay-put ov 'display
                 (concat margin-str
                         (propertize "Agenda" 'face 'org-level-1)
                         "\n\n\n"
                         (propertize agenda-str 'face 'org-show-and-tell-agenda-face)
                         "\n"))
    (push ov org-show-and-tell--overlays)))

(defun org-show-and-tell--apply-content-overlay (margin-str)
  "Render overlays for a standard content slide."
  (let* ((parent (org-show-and-tell--get-parent-title))
         (parent-str (if parent (concat (propertize parent 'face 'org-show-and-tell-breadcrumb) "\n\n") "")))
    (setq org-show-and-tell--top-margin-ov (make-overlay (point-min) (point-min)))
    (overlay-put org-show-and-tell--top-margin-ov 'before-string (concat margin-str parent-str)))
  (org-show-and-tell--hide-heading-stars)
  (org-show-and-tell--hide-block-tags))

(defun org-show-and-tell--apply-overlays ()
  "Dispatch overlay application based on slide type."
  (org-show-and-tell--clear-overlays)
  (let* ((slide (nth org-show-and-tell--index org-show-and-tell--slides))
         (type (org-show-and-tell--slide-type slide))
         (margin-str (org-show-and-tell--margin-string)))
    (pcase type
      ('title  (org-show-and-tell--apply-title-overlay margin-str))
      ('agenda (org-show-and-tell--apply-agenda-overlay margin-str))
      ('slide  (org-show-and-tell--apply-content-overlay margin-str)))))

;; ---------------------------------------------------------------------
;; Display Engine & Teleprompter
;; ---------------------------------------------------------------------

(defun org-show-and-tell--apply-margins ()
  "Apply left and right window margins across all windows displaying this buffer."
  (when (and org-show-and-tell-mode (current-buffer))
    (walk-windows
     (lambda (win)
       (when (eq (window-buffer win) (current-buffer))
         (set-window-margins win org-show-and-tell-margin-width org-show-and-tell-margin-width)))
     nil t)))

(defun org-show-and-tell--sync-teleprompter ()
  "Extract presenter notes from current slide and update *Presenter Notes* buffer."
  (let ((notes-buf (get-buffer "*Presenter Notes*"))
        (notes-text ""))
    (when notes-buf
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t))
          (when (re-search-forward "^[ \t]*#\\+begin_notes[ \t]*\n" nil t)
            (let ((start (point)))
              (when (re-search-forward "^[ \t]*#\\+end_notes" nil t)
                (setq notes-text (buffer-substring-no-properties start (match-beginning 0))))))))

      (with-current-buffer notes-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (if (string-empty-p (string-trim notes-text))
                      "\n  (No notes for this slide)"
                    notes-text))
          (goto-char (point-min)))))))

;;;###autoload
(defun org-show-and-tell-presenter-notes ()
  "Spawn the presenter notes window below."
  (interactive)
  (let ((buf (get-buffer-create "*Presenter Notes*")))
    (with-current-buffer buf
      (visual-line-mode 1)
      (read-only-mode 1))

    (delete-other-windows)
    (org-show-and-tell--sync-teleprompter)

    (let ((window-min-height 1)
          (window-safe-min-height 1))
      (display-buffer buf
                      '((display-buffer-below-selected)
                        (window-height . 0.3))))

    (org-show-and-tell--apply-margins)
    (message "Presenter view ready!")))

(defun org-show-and-tell--render ()
  "Narrow buffer strictly to current slide bounds and format display."
  (widen)
  (let* ((slide (nth org-show-and-tell--index org-show-and-tell--slides))
         (start (org-show-and-tell--slide-start slide))
         (end (org-show-and-tell--slide-end slide)))
    (narrow-to-region start end)
    (goto-char (point-min))
    
    (walk-windows
     (lambda (win)
       (when (eq (window-buffer win) (current-buffer))
         (set-window-point win (point-min))))
     nil t)

    (setq org-show-and-tell--slide-string
          (format "Slide %d of %d" (1+ org-show-and-tell--index) (length org-show-and-tell--slides)))
    (force-mode-line-update)
    (org-show-and-tell--apply-margins)
    (org-show-and-tell--apply-overlays)
    (org-show-and-tell--sync-teleprompter)))

;; ---------------------------------------------------------------------
;; Navigation Commands
;; ---------------------------------------------------------------------

(defun org-show-and-tell-next ()
  "Move to the next slide or exit if on the final slide."
  (interactive)
  (if (>= (1+ org-show-and-tell--index) (length org-show-and-tell--slides))
      (progn
        (org-show-and-tell-mode -1)
        (message "Presentation finished!"))
    (setq org-show-and-tell--index (1+ org-show-and-tell--index))
    (org-show-and-tell--render)))

(defun org-show-and-tell-prev ()
  "Move to the previous slide."
  (interactive)
  (when (> org-show-and-tell--index 0)
    (setq org-show-and-tell--index (1- org-show-and-tell--index))
    (org-show-and-tell--render)))

(defun org-show-and-tell-quit ()
  "Exit presentation mode immediately."
  (interactive)
  (org-show-and-tell-mode -1))

;; ---------------------------------------------------------------------
;; Evil Keybindings
;; ---------------------------------------------------------------------

(when (bound-and-true-p evil-mode)
  (evil-define-minor-mode-key 'normal 'org-show-and-tell-mode
    (kbd "h") #'org-show-and-tell-prev
    (kbd "k") #'org-show-and-tell-prev
    (kbd "l") #'org-show-and-tell-next
    (kbd "j") #'org-show-and-tell-next
    (kbd "q") #'org-show-and-tell-quit
    (kbd "<escape>") #'org-show-and-tell-quit))

;; ---------------------------------------------------------------------
;; State Management & Minor Mode Lifecycle
;; ---------------------------------------------------------------------

(defun org-show-and-tell--save-and-apply-ui ()
  "Save baseline buffer state and apply presentation UI settings."
  (setq org-show-and-tell--saved-tilde-fringe (bound-and-true-p vi-tilde-fringe-mode)
        org-show-and-tell--saved-state
        (list :cursor cursor-type
              :line-numbers display-line-numbers
              :hl-line (bound-and-true-p hl-line-mode)
              :mode-line mode-line-format
              :themes custom-enabled-themes))

  (set (make-local-variable 'display-line-numbers) nil)
  (set (make-local-variable 'cursor-type) nil)
  (when (bound-and-true-p hl-line-mode) (hl-line-mode -1))
  (when org-show-and-tell--saved-tilde-fringe (vi-tilde-fringe-mode -1))

  (when (bound-and-true-p evil-mode)
    (setq org-show-and-tell--saved-evil-cursor evil-normal-state-cursor)
    (set (make-local-variable 'evil-normal-state-cursor) nil))

  (when org-show-and-tell-theme
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme org-show-and-tell-theme t))

  (when (bound-and-true-p org-indent-mode)
    (org-indent-mode -1))

  (set (make-local-variable 'mode-line-format)
       '(:eval
         (let ((margin (max 0 (or (car (window-margins)) org-show-and-tell-margin-width))))
           (concat (make-string margin ?\s)
                   (propertize org-show-and-tell--slide-string 'face 'shadow)))))

  (text-scale-set org-show-and-tell-text-scale)
  (add-hook 'window-size-change-functions #'org-show-and-tell--apply-margins nil t))

(defun org-show-and-tell--restore-ui ()
  "Clean up presentation artifacts and restore saved UI state."
  (widen)
  (text-scale-set 0)
  (org-show-and-tell--clear-overlays)
  (walk-windows (lambda (w) (set-window-margins w nil nil)) nil t)
  (remove-hook 'window-size-change-functions #'org-show-and-tell--apply-margins t)

  (when-let ((notes-buf (get-buffer "*Presenter Notes*")))
    (when-let ((win (get-buffer-window notes-buf t)))
      (delete-window win))
    (kill-buffer notes-buf))

  (when org-show-and-tell--saved-state
    (set (make-local-variable 'cursor-type) (plist-get org-show-and-tell--saved-state :cursor))
    (set (make-local-variable 'display-line-numbers) (plist-get org-show-and-tell--saved-state :line-numbers))
    (set (make-local-variable 'mode-line-format) (plist-get org-show-and-tell--saved-state :mode-line))

    (when (plist-get org-show-and-tell--saved-state :hl-line)
      (hl-line-mode 1))

    (when org-show-and-tell--saved-tilde-fringe
      (vi-tilde-fringe-mode 1))

    (when (bound-and-true-p evil-mode)
      (set (make-local-variable 'evil-normal-state-cursor) org-show-and-tell--saved-evil-cursor))

    (when (plist-get org-show-and-tell--saved-state :themes)
      (mapc #'disable-theme custom-enabled-themes)
      (dolist (th (plist-get org-show-and-tell--saved-state :themes))
        (load-theme th t)))
    (setq org-show-and-tell--saved-state nil))

  (when (derived-mode-p 'org-mode)
    (org-indent-mode 1)
    (if (fboundp 'org-fold-show-all)
        (org-fold-show-all)
      (org-show-all))))

;;;###autoload
(define-minor-mode org-show-and-tell-mode
  "A Doom-friendly presentation mode for Org files with."
  :init-value nil
  :global nil
  (if org-show-and-tell-mode
      (let ((slides (org-show-and-tell--collect-slides)))
        (if (null slides)
            (progn
              (setq org-show-and-tell-mode nil)
              (user-error "No Level 1 (*) or Level 2 (**) headings found in buffer"))
          (setq org-show-and-tell--slides slides
                org-show-and-tell--index 0
                org-show-and-tell--agenda-items (org-show-and-tell--build-agenda))
          (org-show-and-tell--save-and-apply-ui)
          (org-show-and-tell--render)))
    (org-show-and-tell--restore-ui)))

;;;###autoload
(defun org-show-and-tell-goto-slide (n)
  "Jump directly to slide number N."
  (interactive "nJump to slide: ")
  (unless (and (boundp 'org-show-and-tell-mode) org-show-and-tell-mode)
    (user-error "Not in org-show-and-tell-mode"))
  (if (and (>= n 1) (<= n (length org-show-and-tell--slides)))
      (progn
        (setq org-show-and-tell--index (1- n))
        (org-show-and-tell--render))
    (user-error "Invalid slide number %d (valid range: 1-%d)" n (length org-show-and-tell--slides))))

;;;###autoload
(defun org-show-and-tell-goto-agenda ()
  "Jump to an agenda section using cached items."
  (interactive)
  (unless (and (boundp 'org-show-and-tell-mode) org-show-and-tell-mode)
    (user-error "Not in org-show-and-tell-mode"))
  (if (null org-show-and-tell--agenda-items)
      (user-error "No agenda items found")
    (let* ((completion-extra-properties '(:display-sort-function identity
                                          :cycle-sort-function identity))
           (choice (completing-read "Agenda: " org-show-and-tell--agenda-items nil t))
           (target-idx (cdr (assoc choice org-show-and-tell--agenda-items))))
      (when target-idx
        (setq org-show-and-tell--index target-idx)
        (org-show-and-tell--render)))))

(provide 'org-show-and-tell)
;;; org-show-and-tell.el ends here
