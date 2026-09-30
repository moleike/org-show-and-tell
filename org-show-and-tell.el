;;; org-show-and-tell.el --- A presentation mode for Org files with teleprompter support -*- lexical-binding: t; -*-

;; Author: Alexandre Moreno
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (org "9.5"))

;;; Commentary:
;; org-show-and-tell is an opinionated presentation mode for Org files. It
;; automatically generates title and agenda slides, syncs presenter notes to a
;; separate buffer (*Presenter Notes*), switches your theme for presentation,
;; adds parent breadcrumbs for subheadings, includes built-in Evil keybindings,
;; natively supports inline images, and external display scaling.

;;; Code:

(require 'org)

(defgroup org-show-and-tell nil
  "A presentation mode for Org mode."
  :group 'org)

;;; Configuration

(defcustom org-show-and-tell-title-slide t
  "If non-nil, automatically generate a title slide from keywords."
  :type 'boolean)

(defcustom org-show-and-tell-agenda-slide t
  "If non-nil, automatically generate an Agenda/TOC slide before the content."
  :type 'boolean)

(defcustom org-show-and-tell-text-scale 2
  "Base text scaling level during presentations."
  :type 'integer)

(defcustom org-show-and-tell-content-width 70
  "Target width of the slide content in columns for dynamic centering."
  :type 'integer)

(defcustom org-show-and-tell-minimum-margin 8
  "The absolute minimum left and right margin in columns."
  :type 'integer)

(defcustom org-show-and-tell-top-margin 4
  "Number of blank lines to push the top slide down."
  :type 'integer)

(defcustom org-show-and-tell-base-pixel-width 1200
  "Baseline window pixel width.
Wider windows dynamically trigger proportional text scaling."
  :type 'integer)

(defcustom org-show-and-tell-theme 'doom-homage-white
  "Theme to apply during presentation (or nil to keep current)."
  :type '(choice (const :tag "Keep current theme" nil) symbol))

(defcustom org-show-and-tell-hide-modes
  '(evil-local-mode
    display-line-numbers-mode
    hl-line-mode
    vi-tilde-fringe-mode
    diff-hl-mode
    org-indent-mode
    flycheck-mode
    flyspell-mode
    display-fill-column-indicator-mode)
  "List of minor modes to disable during a presentation."
  :type '(repeat symbol)
  :group 'org-show-and-tell)

;;; Faces

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

;;; Internal Variables

(defvar-local org-show-and-tell--slides nil)
(defvar-local org-show-and-tell--index 0)
(defvar-local org-show-and-tell--top-margin-ov nil)
(defvar-local org-show-and-tell--overlays nil)
(defvar-local org-show-and-tell--slide-counter "")
(defvar-local org-show-and-tell--saved-evil-cursor nil)
(defvar-local org-show-and-tell--saved-evil-state nil)
(defvar-local org-show-and-tell--agenda-items nil)
(defvar-local org-show-and-tell--disabled-modes nil)
(defvar-local org-show-and-tell--saved-mode-vars nil)
(defvar-local org-show-and-tell--was-read-only nil)
(defvar-local org-show-and-tell--last-pixel-width nil)
(defvar-local org-show-and-tell--is-presenter-clone nil)
(defvar-local org-show-and-tell--was-visual-line nil)
(defvar-local org-show-and-tell--resize-timer nil)

(defvar org-show-and-tell--saved-themes 'uninitialized
  "Stores the active themes prior to presentation start.")

(defsubst org-show-and-tell--slide-type (slide) (car slide))
(defsubst org-show-and-tell--slide-start (slide) (cadr slide))
(defsubst org-show-and-tell--slide-end (slide) (cddr slide))

;;; Data Accessors & Helpers

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
  "Generate formatted agenda string."
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

;;; Overlay Rendering Modules

(defun org-show-and-tell--clear-overlays ()
  "Clear all active slide overlays."
  (when org-show-and-tell--top-margin-ov
    (delete-overlay org-show-and-tell--top-margin-ov)
    (setq org-show-and-tell--top-margin-ov nil))
  (mapc #'delete-overlay org-show-and-tell--overlays)
  (setq org-show-and-tell--overlays nil))

(defun org-show-and-tell--hide-region (beg end)
  "Create an invisible overlay from BEG to END."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'display "")
    (push ov org-show-and-tell--overlays)))

(defun org-show-and-tell--hide-lines-matching (regexp)
  "Hide single lines matching REGEXP."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (let ((end (if (eq (char-after (match-end 0)) ?\n) (1+ (match-end 0)) (match-end 0))))
      (org-show-and-tell--hide-region (line-beginning-position) end))))

(defun org-show-and-tell--hide-heading-stars ()
  "Hide leading asterisks on heading lines."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\)\\s-+" nil t)
      (org-show-and-tell--hide-region (match-beginning 1) (match-end 0)))))

(defun org-show-and-tell--hide-block-tags ()
  "Hide Org meta-lines like #+begin_src, #+ATTR_, and #+CAPTION."
  (save-excursion
    (let ((case-fold-search t))
      (org-show-and-tell--hide-lines-matching "^[ \t]*#\\+\\(begin\\|end\\)_src.*$")
      (org-show-and-tell--hide-lines-matching "^[ \t]*#\\+\\(ATTR_[a-zA-Z0-9_]+\\|CAPTION\\|NAME\\|RESULTS\\).*$")

      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+begin_notes" nil t)
        (let ((beg (line-beginning-position)))
          (when (re-search-forward "^[ \t]*#\\+end_notes.*$" nil t)
            (let ((end (if (eq (char-after (match-end 0)) ?\n) (1+ (match-end 0)) (match-end 0))))
              (org-show-and-tell--hide-region beg end))))))))

(defun org-show-and-tell--apply-title-overlay ()
  "Render virtual Title slide overlay."
  (let ((title (or (org-show-and-tell--get-keyword "TITLE") "Presentation"))
        (author (or (org-show-and-tell--get-keyword "AUTHOR") ""))
        (ov (make-overlay (point-min) (point-max))))
    (overlay-put ov 'display
                 (concat (propertize title 'face 'org-show-and-tell-title-face)
                         "\n\n"
                         (propertize author 'face 'org-show-and-tell-author-face)
                         "\n"))
    (push ov org-show-and-tell--overlays)))

(defun org-show-and-tell--apply-agenda-overlay ()
  "Render virtual Agenda slide overlay."
  (let ((agenda-str (org-show-and-tell--generate-agenda-string))
        (ov (make-overlay (point-min) (point-max))))
    (overlay-put ov 'display
                 (concat (propertize "Agenda" 'face 'org-level-1)
                         "\n\n\n"
                         (propertize agenda-str 'face 'org-show-and-tell-agenda-face)
                         "\n"))
    (push ov org-show-and-tell--overlays)))

(defun org-show-and-tell--apply-overlays ()
  "Dispatch overlay application based on slide type."
  (org-show-and-tell--clear-overlays)
  (setq org-show-and-tell--top-margin-ov (make-overlay (point-min) (point-min)))
  (let* ((slide (nth org-show-and-tell--index org-show-and-tell--slides))
         (type (org-show-and-tell--slide-type slide)))
    (pcase type
      ('title  (org-show-and-tell--apply-title-overlay))
      ('agenda (org-show-and-tell--apply-agenda-overlay))
      ('slide  (progn
                 (org-show-and-tell--hide-heading-stars)
                 (org-show-and-tell--hide-block-tags)))))
  (org-show-and-tell--on-window-change))

;;; Display Engine & Responsive Geometry Helpers

(defun org-show-and-tell--enforce-evil-state ()
  "Hide cursor and force Evil into emacs state."
  (setq-local cursor-type nil
              cursor-in-non-selected-windows nil)
  (when (bound-and-true-p evil-local-mode)
    (setq-local evil-normal-state-cursor nil
                evil-emacs-state-cursor nil)
    (unless (eq evil-state 'emacs)
      (evil-emacs-state))
    (when (fboundp 'evil-refresh-cursor)
      (evil-refresh-cursor))))

(defun org-show-and-tell--update-text-scale ()
  "Calculate and apply responsive text scaling."
  (let ((max-pw 0)
        (step (if (boundp 'text-scale-mode-step) text-scale-mode-step 1.2)))
    (walk-windows
     (lambda (w)
       (when (eq (window-buffer w) (current-buffer))
         (setq max-pw (max max-pw (window-pixel-width w)))))
     nil t)
    (when (and (> max-pw 0) (not org-show-and-tell--is-presenter-clone))
      (unless (equal max-pw org-show-and-tell--last-pixel-width)
        (setq-local org-show-and-tell--last-pixel-width max-pw)
        (let* ((base-pw (float org-show-and-tell-base-pixel-width))
               (ratio (max 0.1 (/ (float max-pw) base-pw)))
               ;; This guarantees the font won't scale up until the window has
               ;; grown completely enough to fit it, protecting the margins.
               (extra-scale (floor (/ (log ratio) (log step))))
               (target-scale (+ org-show-and-tell-text-scale extra-scale)))
          (unless (= (if (boundp 'text-scale-mode-amount) text-scale-mode-amount 0) target-scale)
            (text-scale-set target-scale)))))))

(defun org-show-and-tell--apply-window-margin (win content-width is-clone)
  "Calculate and apply the pixel-perfect centering margin for a single WIN."
  (when (eq (window-buffer win) (current-buffer))
    (let* ((font-px     (window-font-width win))
           (char-px     (frame-char-width (window-frame win)))
           (win-px      (window-pixel-width win))
           (content-px  (* (+ 4 content-width) font-px)) ; +4 padding so visual-line-mode NEVER wraps early
           (calculated  (floor (/ (float (- win-px content-px)) 2.0 (float char-px))))
           (min-margin  (if is-clone 4 org-show-and-tell-minimum-margin))
           (margin-cols (max min-margin calculated))
           (cur-margins (window-margins win)))
      
      (unless (equal cur-margins (cons margin-cols margin-cols))
        (set-window-margins win margin-cols margin-cols)))))

(defun org-show-and-tell--update-margins ()
  "Trigger asynchronous recalculation of window margins."
  (let ((buf (current-buffer))
        (width org-show-and-tell-content-width)
        (is-clone org-show-and-tell--is-presenter-clone))

    ;; Wait 100ms for macOS to finish redrawing the frame
    (run-with-timer 0.1 nil
                    (lambda ()
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (walk-windows 
                           (lambda (win) 
                             (org-show-and-tell--apply-window-margin win width is-clone))
                           nil t)))))))

(defun org-show-and-tell--debounced-window-change (&optional _arg)
  "Debounce window changes to prevent interrupting OS-level window dragging."
  (when org-show-and-tell--resize-timer
    (cancel-timer org-show-and-tell--resize-timer))
  (setq org-show-and-tell--resize-timer
        (run-with-timer 0.2 nil
                        (lambda (buf)
                          (when (buffer-live-p buf)
                            (with-current-buffer buf
                              (org-show-and-tell--on-window-change))))
                        (current-buffer))))

(defun org-show-and-tell--on-window-change (&optional _arg)
  "Recalculate layout geometries and assert presentation states."
  (when (and (boundp 'org-show-and-tell-mode)
             org-show-and-tell-mode
             (current-buffer))

    (org-show-and-tell--enforce-evil-state)
    (org-show-and-tell--update-text-scale)
    (org-show-and-tell--update-margins)

    (when org-show-and-tell--top-margin-ov
      (let* ((pad-str (make-string org-show-and-tell-top-margin ?\n))
             (parent (org-show-and-tell--get-parent-title))
             (parent-str (if parent (concat (propertize parent 'face 'org-show-and-tell-breadcrumb) "\n\n") "")))
        (overlay-put org-show-and-tell--top-margin-ov 'before-string (concat pad-str parent-str))))))

;;; Presentation Sync Engine

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

(defun org-show-and-tell--render ()
  "Narrow buffer strictly to current slide bounds and format display."
  (widen)
  (let* ((slide (nth org-show-and-tell--index org-show-and-tell--slides))
         (start (org-show-and-tell--slide-start slide))
         (end (org-show-and-tell--slide-end slide)))
    (narrow-to-region start end)
    (goto-char (point-min))

    (ignore-errors
      (when (fboundp 'font-lock-flush)
        (font-lock-flush)))

    (walk-windows
     (lambda (win)
       (when (eq (window-buffer win) (current-buffer))
         (set-window-point win (point-min))))
     nil t)

    (setq org-show-and-tell--slide-counter
          (format "Slide %d of %d" (1+ org-show-and-tell--index) (length org-show-and-tell--slides)))
    (force-mode-line-update)
    (org-show-and-tell--apply-overlays)
    (org-display-inline-images nil t (point-min) (point-max))
    (org-show-and-tell--sync-teleprompter)))

(defun org-show-and-tell--sync-state (new-idx)
  "Synchronize the presentation index across base and cloned teleprompter buffers."
  (let ((base (or (buffer-base-buffer) (current-buffer))))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (boundp 'org-show-and-tell-mode)
                   org-show-and-tell-mode
                   (eq (or (buffer-base-buffer) (current-buffer)) base))
          (setq org-show-and-tell--index new-idx)
          (org-show-and-tell--render))))))

;;; Teleprompter Creation

;;;###autoload
(defun org-show-and-tell-presenter-view ()
  "Spawn the dual-monitor teleprompter frame."
  (interactive)
  (unless (and (boundp 'org-show-and-tell-mode) org-show-and-tell-mode)
    (user-error "Start org-show-and-tell-mode first"))

  (let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
         (clone-name (generate-new-buffer-name (concat (buffer-name) " (Teleprompter)")))
         (current-idx org-show-and-tell--index)
         (presenter-frame (make-frame '((name . "Teleprompter")))))

    (select-frame presenter-frame)
    (let ((clone-buf (clone-indirect-buffer clone-name nil t)))
      (switch-to-buffer clone-buf)
      (widen)

      (remove-overlays (point-min) (point-max))
      (setq org-show-and-tell--overlays nil
            org-show-and-tell--top-margin-ov nil
            org-show-and-tell--is-presenter-clone t
            org-show-and-tell--index current-idx)

      (visual-line-mode 1)
      (text-scale-set 0)
      (org-show-and-tell--render)
      (delete-other-windows)

      (let ((notes-buf (get-buffer-create "*Presenter Notes*")))
        (with-current-buffer notes-buf
          (visual-line-mode 1)
          (read-only-mode 1))

        (org-show-and-tell--sync-teleprompter)
        (display-buffer notes-buf '((display-buffer-below-selected) (window-height . 0.3))))

      (org-show-and-tell--on-window-change)
      (message "Teleprompter ready! Drag your original frame to the big screen."))))

;;; Navigation Commands

(defun org-show-and-tell-next ()
  "Move to the next slide or exit if on the final slide."
  (interactive)
  (if (>= (1+ org-show-and-tell--index) (length org-show-and-tell--slides))
      (progn
        (org-show-and-tell-quit)
        (message "Presentation finished!"))
    (org-show-and-tell--sync-state (1+ org-show-and-tell--index))))

(defun org-show-and-tell-prev ()
  "Move to the previous slide."
  (interactive)
  (when (> org-show-and-tell--index 0)
    (org-show-and-tell--sync-state (1- org-show-and-tell--index))))

(defun org-show-and-tell-quit ()
  "Exit presentation mode across all synced screens and safely clean up clones."
  (interactive)
  (let ((base (or (buffer-base-buffer) (current-buffer))))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (boundp 'org-show-and-tell-mode)
                   org-show-and-tell-mode
                   (eq (or (buffer-base-buffer) (current-buffer)) base))
          (org-show-and-tell-mode -1)
          (when (buffer-base-buffer)
            (let ((frame (when-let ((w (get-buffer-window buf t))) (window-frame w))))
              (kill-buffer buf)
              (when (and frame (> (length (frame-list)) 1))
                (delete-frame frame)))))))))

;;; Keybindings

(defvar org-show-and-tell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") #'org-show-and-tell-prev)
    (define-key map (kbd "k") #'org-show-and-tell-prev)
    (define-key map (kbd "l") #'org-show-and-tell-next)
    (define-key map (kbd "j") #'org-show-and-tell-next)
    (define-key map (kbd "q") #'org-show-and-tell-quit)
    map)
  "Keymap for `org-show-and-tell-mode'.")

(with-eval-after-load 'evil
  (evil-define-minor-mode-key '(normal motion emacs) 'org-show-and-tell-mode
    (kbd "h") #'org-show-and-tell-prev
    (kbd "k") #'org-show-and-tell-prev
    (kbd "l") #'org-show-and-tell-next
    (kbd "j") #'org-show-and-tell-next
    (kbd "q") #'org-show-and-tell-quit))

;;; State Backup & Restore Helpers

(defun org-show-and-tell--apply-mode-line ()
  "Apply mode line displaying the centered slide counter."
  (setq-local mode-line-format
              '(:eval
                (let ((margin (max 0 (or (car (window-margins)) 0))))
                  (concat (make-string margin ?\s)
                          (propertize org-show-and-tell--slide-counter 'face 'shadow))))))

(defun org-show-and-tell--save-evil-state ()
  "Backup current evil state and default to emacs state."
  (when (bound-and-true-p evil-local-mode)
    (setq-local org-show-and-tell--saved-evil-state evil-state)
    (org-show-and-tell--enforce-evil-state)))

(defun org-show-and-tell--restore-evil-state ()
  "Restore original evil state."
  (when (bound-and-true-p evil-local-mode)
    (kill-local-variable 'evil-emacs-state-cursor)
    (kill-local-variable 'evil-normal-state-cursor)
    (when org-show-and-tell--saved-evil-state
      (funcall (intern (format "evil-%s-state" org-show-and-tell--saved-evil-state))))
    (when (fboundp 'evil-refresh-cursor)
      (evil-refresh-cursor))))

(defun org-show-and-tell--disable-minor-modes ()
  "Backup and disable interfering minor modes."
  (setq org-show-and-tell--disabled-modes nil
        org-show-and-tell--saved-mode-vars nil)
  (dolist (mode org-show-and-tell-hide-modes)
    (when (and (boundp mode) (symbol-value mode))
      (unless (eq mode 'evil-local-mode)
        (push mode org-show-and-tell--disabled-modes)
        (let ((base-var (intern (replace-regexp-in-string "-mode\\'" "" (symbol-name mode)))))
          (when (and (boundp base-var) (not (eq base-var mode)))
            (push (list base-var (local-variable-p base-var) (symbol-value base-var)) 
                  org-show-and-tell--saved-mode-vars)))
        (when (fboundp mode)
          (funcall mode -1))))))

(defun org-show-and-tell--restore-minor-modes ()
  "Restore previously disabled minor modes."
  (dolist (mode org-show-and-tell-hide-modes)
    (when (and (fboundp mode) (not (eq mode 'evil-local-mode)))
      (if (memq mode org-show-and-tell--disabled-modes)
          (funcall mode 1)
        (when (and (boundp mode) (symbol-value mode))
          (funcall mode -1)))))
  (dolist (var-info org-show-and-tell--saved-mode-vars)
    (if (nth 1 var-info)
        (set (make-local-variable (nth 0 var-info)) (nth 2 var-info))
      (kill-local-variable (nth 0 var-info))))
  (setq org-show-and-tell--disabled-modes nil
        org-show-and-tell--saved-mode-vars nil))

(defun org-show-and-tell--apply-theme ()
  "Apply presentation theme if this is the first active presentation buffer."
  (when (and org-show-and-tell-theme (eq org-show-and-tell--saved-themes 'uninitialized))
    (setq org-show-and-tell--saved-themes custom-enabled-themes)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme org-show-and-tell-theme t)))

(defun org-show-and-tell--restore-theme ()
  "Restore original theme if no other presentation buffers are still open."
  (when (and org-show-and-tell-theme (not (eq org-show-and-tell--saved-themes 'uninitialized)))
    (let ((others-active nil))
      (dolist (b (buffer-list))
        (when (and (not (eq b (current-buffer)))
                   (buffer-local-value 'org-show-and-tell-mode b))
          (setq others-active t)))
      (unless others-active
        (mapc #'disable-theme custom-enabled-themes)
        (dolist (th org-show-and-tell--saved-themes)
          (ignore-errors (load-theme th t)))
        (setq org-show-and-tell--saved-themes 'uninitialized)))))

;;; Minor Mode Initialization

(defun org-show-and-tell--save-and-apply-ui ()
  "Save baseline buffer state and apply presentation UI settings."
  (setq-local org-hide-emphasis-markers t
              org-show-and-tell--was-read-only buffer-read-only
              org-show-and-tell--was-visual-line visual-line-mode
              org-show-and-tell--last-pixel-width nil)

  (read-only-mode 1)
  (visual-line-mode 1)

  (when (fboundp 'org-restart-font-lock)
    (org-restart-font-lock))

  (org-show-and-tell--save-evil-state)
  (org-show-and-tell--disable-minor-modes)
  (org-show-and-tell--apply-theme)
  (org-show-and-tell--apply-mode-line)

  (add-hook 'window-size-change-functions #'org-show-and-tell--debounced-window-change nil t)
  (add-hook 'window-selection-change-functions #'org-show-and-tell--debounced-window-change nil t))


(defun org-show-and-tell--restore-ui ()
  "Clean up presentation artifacts and restore saved UI state."
  (widen)
  (text-scale-set 0)
  (org-show-and-tell--clear-overlays)
  (org-remove-inline-images)

  (walk-windows (lambda (w) (set-window-margins w nil nil)) nil t)

  (when org-show-and-tell--resize-timer
    (cancel-timer org-show-and-tell--resize-timer)
    (kill-local-variable 'org-show-and-tell--resize-timer))

  (remove-hook 'window-size-change-functions #'org-show-and-tell--debounced-window-change t)
  (remove-hook 'window-selection-change-functions #'org-show-and-tell--debounced-window-change t)

  (when-let ((notes-buf (get-buffer "*Presenter Notes*")))
    (when-let ((win (get-buffer-window notes-buf t)))
      (delete-window win))
    (kill-buffer notes-buf))

  (kill-local-variable 'cursor-type)
  (kill-local-variable 'cursor-in-non-selected-windows)
  (kill-local-variable 'org-hide-emphasis-markers)
  (kill-local-variable 'mode-line-format)

  (if org-show-and-tell--was-read-only (read-only-mode 1) (read-only-mode -1))
  (if org-show-and-tell--was-visual-line (visual-line-mode 1) (visual-line-mode -1))

  (kill-local-variable 'org-show-and-tell--was-read-only)
  (kill-local-variable 'org-show-and-tell--was-visual-line)

  (org-show-and-tell--restore-evil-state)
  (org-show-and-tell--restore-theme)
  (org-show-and-tell--restore-minor-modes)

  (when (fboundp 'org-restart-font-lock)
    (org-restart-font-lock))
  (when (derived-mode-p 'org-mode)
    (if (fboundp 'org-fold-show-all)
        (org-fold-show-all)
      (org-show-all))))

;;;###autoload
(define-minor-mode org-show-and-tell-mode
  "A Doom-friendly presentation mode for Org files with teleprompter."
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
        (org-show-and-tell--sync-state (1- n)))
    (user-error "Invalid slide number %d (valid range: 1-%d)" n (length org-show-and-tell--slides))))

;;;###autoload
(defun org-show-and-tell-goto-agenda ()
  "Jump to an agenda section."
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
        (org-show-and-tell--sync-state target-idx)))))

(provide 'org-show-and-tell)
;;; org-show-and-tell.el ends here
