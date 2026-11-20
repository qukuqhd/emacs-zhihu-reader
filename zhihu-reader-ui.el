;;; zhihu-reader-ui.el --- Org rendering, images and mode for Zhihu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 qukuqhd
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Part of emacs-zhihu-reader.  See zhihu-reader.el for the entry point.

;;; Code:

(require 'zhihu-reader-core)

;;;; Rendering

(defun zhihu-reader--safe-title (title)
  "Make TITLE safe for an Org heading."
  (replace-regexp-in-string "[\n\r]" " " (or title "")))

(defun zhihu-reader--hide-drawers ()
  "Hide all :PROPERTIES: drawers in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n" nil t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (unless (cl-some (lambda (overlay)
                           (overlay-get overlay 'zhihu-drawer))
                         (overlays-in beg end))
          (let ((overlay (make-overlay beg end)))
            (overlay-put overlay 'invisible t)
            (overlay-put overlay 'zhihu-drawer t)))))))

(defun zhihu-reader--heading (level title props)
  "Insert an Org heading at LEVEL with TITLE and PROPS."
  (insert (make-string level ?*) " " (zhihu-reader--safe-title title) "\n")
  (when props
    (insert ":PROPERTIES:\n")
    (dolist (prop props)
      (insert (format ":%s: %s\n" (car prop) (cdr prop))))
    (insert ":END:\n")))

(defun zhihu-reader--body (text)
  "Insert TEXT as an Org body, protecting leading stars."
  (dolist (line (split-string (or text "") "\n"))
    (insert (if (string-match-p "\\`\\*+" line)
                (concat "  " line)
              line)
            "\n")))

(defun zhihu-reader--comment-heading (level count id &optional kind)
  "Insert a comments heading at LEVEL with COUNT, ID and KIND."
  (zhihu-reader--heading
   level (format "评论 (%s) · c 加载" (or count "0"))
   (list (cons "ZHIHU-COMMENTS" id)
         (cons "ZHIHU-COMMENTS-COUNT" (or count "0"))
         (cons "ZHIHU-COMMENTS-KIND" (or kind "answer"))
         (cons "ZHIHU-COMMENTS-STATUS" "idle")
         (cons "ZHIHU-COMMENTS-PAGE" "1")
         (cons "ZHIHU-COMMENTS-HAS-NEXT" "1")
         (cons "ZHIHU-COMMENTS-HAS-PREV" "0"))))

(defun zhihu-reader--show-comment-subtree (marker)
  "Reveal the comments subtree at MARKER, including its body."
  (when (and (markerp marker)
             (marker-buffer marker)
             (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (ignore-errors (org-back-to-heading t))
        ;; Only reveal this comments subtree.  Do not use
        ;; `org-fold-show-all': it can leak into unrelated headings when
        ;; comments arrive while the user has moved the cursor.
        (ignore-errors (org-fold-show-subtree)))
      (ignore-errors (force-window-update (current-buffer))))))

(defun zhihu-reader--set-comment-status (marker status &optional actual)
  "Update comment heading at MARKER to STATUS.
ACTUAL is an optional loaded comment count."
  (when (and (markerp marker)
             (marker-buffer marker)
             (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (org-back-to-heading t)
          (let* ((level (or (org-current-level) 1))
                 (count (or (org-entry-get nil "ZHIHU-COMMENTS-COUNT") "0"))
                 (title
                  (pcase status
                    ("loading" (format "评论 (%s) · 加载中…" count))
                    ("loaded" (format "评论 (%s) · 已加载 %s 条"
                                      count (or actual "")))
                    ("empty" (format "评论 (%s) · 无内容" count))
                    ("failed" (format "评论 (%s) · 加载失败" count))
                    (_ (format "评论 (%s) · c 加载" count)))))
            (delete-region (line-beginning-position) (line-end-position))
            (insert (make-string level ?*) " " title)
            (org-entry-put nil "ZHIHU-COMMENTS-STATUS" status)))))))

(defun zhihu-reader--image-file (url)
  "Download URL into the image cache and return its local path."
  (let* ((path (url-generic-parse-url url))
         (extension (or (and (stringp (url-filename path))
                             (file-name-extension (url-filename path)))
                        "jpg"))
         (file (expand-file-name
                (concat (secure-hash 'sha1 url) "." extension)
                zhihu-reader-image-cache)))
    (unless (file-exists-p file)
      (make-directory zhihu-reader-image-cache t)
      (let ((url-request-extra-headers
             `(("User-Agent" . ,zhihu-reader-user-agent)
               ("Referer" . "https://www.zhihu.com/")
               ("Accept" . "image/avif,image/webp,image/apng,image/*,*/*;q=0.8")))
            (buf (condition-case nil
                     (url-retrieve-synchronously url t t 25)
                   (error nil))))
        (when buf
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (when (equal url-http-response-status 200)
                  (goto-char url-http-end-of-headers)
                  (skip-chars-forward "\r\n")
                  (let ((data (buffer-substring-no-properties
                               (point) (point-max))))
                    (with-temp-file file
                      (set-buffer-multibyte nil)
                      (insert data)))))
            (kill-buffer buf)))))
    (and (file-exists-p file) file)))

(defvar zhihu-reader--image-inflight (make-hash-table :test #'equal)
  "Image URLs currently being downloaded.")
(defvar zhihu-reader--image-failed (make-hash-table :test #'equal)
  "Image URLs that failed to download.")
(defcustom zhihu-reader-auto-comments nil
  "Automatically load comments when a subtree is rendered or expanded."
  :type 'boolean
  :group 'zhihu-reader)

(defcustom zhihu-reader-auto-comments-limit 10
  "Maximum number of comment sections auto-loaded per render."
  :type 'integer
  :group 'zhihu-reader)

(defcustom zhihu-reader-comment-order "score"
  "Zhihu comment sort order: `score' or `time'."
  :type '(choice (const "score") (const "time"))
  :group 'zhihu-reader)

(defcustom zhihu-reader-default-expand-questions nil
  "Whether question subtrees start expanded when a page is rendered."
  :type 'boolean
  :group 'zhihu-reader)

(defcustom zhihu-reader-default-expand-answers nil
  "Whether answer bodies start expanded when a page is rendered."
  :type 'boolean
  :group 'zhihu-reader)

(defcustom zhihu-reader-default-expand-comments nil
  "Whether comments start expanded when loaded.
The default is nil: questions and answers can be expanded while comments
remain folded until you press `c' or `TAB' on a comment heading."
  :type 'boolean
  :group 'zhihu-reader)

(defvar zhihu-reader--comments-inflight (make-hash-table :test #'equal))

(defvar-local zhihu-reader--image-loading 0)
(defvar-local zhihu-reader--image-spinner-timer nil)
(defvar-local zhihu-reader--image-spinner-index 0)
(defconst zhihu-reader--image-spinner
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])

(defun zhihu-reader--image-spinner-tick (buffer)
  "Advance the image-loading spinner in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (> zhihu-reader--image-loading 0)
        (setq zhihu-reader--image-spinner-index
              (mod (1+ zhihu-reader--image-spinner-index)
                   (length zhihu-reader--image-spinner)))
        (setq mode-line-process
              (list (concat " 图片加载中 "
                            (aref zhihu-reader--image-spinner
                                  zhihu-reader--image-spinner-index))))))))

(defvar-local zhihu-reader--image-display-timer nil)

(defun zhihu-reader--queue-image-display ()
  "Debounce inline image rendering in the current buffer.
Links produced by the reader carry a description, so INCLUDE-LINKED must
be non-nil; vanilla `org-display-inline-images' only renders bare links."
  (when (and zhihu-reader-render-images (display-graphic-p))
    (when (timerp zhihu-reader--image-display-timer)
      (cancel-timer zhihu-reader--image-display-timer))
    (let ((buffer (current-buffer)))
      (setq zhihu-reader--image-display-timer
            (run-with-timer
             0.2 nil
             (lambda (buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq zhihu-reader--image-display-timer nil)
                   (when (and zhihu-reader-render-images
                              (display-graphic-p))
                     (setq-local org-image-actual-width
                                 (list zhihu-reader-image-width))
                     (ignore-errors
                       (org-display-inline-images t t))
                     (ignore-errors (force-window-update (current-buffer)))))))
             buffer)))))

(defun zhihu-reader--image-start-loading ()
  "Mark one image download as started."
  (setq zhihu-reader--image-loading (1+ zhihu-reader--image-loading))
  (unless zhihu-reader--image-spinner-timer
    (let ((buffer (current-buffer)))
      (setq zhihu-reader--image-spinner-timer
            (run-with-timer 0 0.2 #'zhihu-reader--image-spinner-tick buffer)))))

(defun zhihu-reader--image-finish-loading ()
  "Mark one image download as finished."
  (setq zhihu-reader--image-loading (max 0 (1- zhihu-reader--image-loading)))
  (when (<= zhihu-reader--image-loading 0)
    (when zhihu-reader--image-spinner-timer
      (cancel-timer zhihu-reader--image-spinner-timer)
      (setq zhihu-reader--image-spinner-timer nil))
    (setq mode-line-process nil)))

(defun zhihu-reader--image-valid-p (file)
  "Return non-nil if FILE starts with a known image signature."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (or (looking-at "\xff\xd8")
          (looking-at "\x89PNG")
          (looking-at "GIF8")
          (looking-at "RIFF")))))

(defun zhihu-reader--image-cache-file (url)
  "Return the cache file path for image URL."
  (let* ((parsed (url-generic-parse-url url))
         (filename (and (stringp (url-filename parsed)) (url-filename parsed)))
         (filename (and filename (car (split-string filename "[?#]"))))
         (extension (or (and filename (file-name-extension filename)) "jpg"))
         (file (expand-file-name
                (concat (secure-hash 'sha1 url) "." extension)
                zhihu-reader-image-cache)))
    file))

(defun zhihu-reader--replace-image-link (marker file)
  "Replace the Org link at MARKER with a local FILE link."
  (when (and (markerp marker)
             (marker-buffer marker)
             (buffer-live-p (marker-buffer marker))
             (file-exists-p file))
    (with-current-buffer (marker-buffer marker)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (when (looking-at "\\[\\[")
            (delete-region (point) (line-end-position))
            (insert (org-make-link-string (concat "file:" file) "图片"))))
        (when (and zhihu-reader-render-images
                   (display-graphic-p))
          (setq-local org-image-actual-width
                      (list zhihu-reader-image-width))
          (zhihu-reader--queue-image-display))))))

(defun zhihu-reader--image-failed (marker url)
  "Replace MARKER with a failed-image label."
  (puthash url t zhihu-reader--image-failed)
  (when (and (markerp marker)
             (marker-buffer marker)
             (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (when (looking-at "\\[\\[")
            (delete-region (point) (line-end-position))
            (insert (org-make-link-string url "图片加载失败"))))))))

(defun zhihu-reader--download-image-async (url marker)
  "Download image URL asynchronously and replace MARKER on success."
  (let* ((file (zhihu-reader--image-cache-file url))
         (buffer (current-buffer))
         (generation zhihu-reader--generation))
    (if (zhihu-reader--image-valid-p file)
        (progn
          (remhash url zhihu-reader--image-inflight)
          (zhihu-reader--replace-image-link marker file))
      (when (file-exists-p file) (ignore-errors (delete-file file)))
      (let ((url-request-extra-headers
             `(("User-Agent" . ,zhihu-reader-user-agent)
               ("Cookie" . ,(condition-case nil (zhihu-reader--cookies) (error "")))
               ("Referer" . "https://www.zhihu.com/")
               ("Accept" . "image/avif,image/webp,image/apng,image/*,*/*;q=0.8"))))
        (zhihu-reader--image-start-loading)
        (condition-case err
            (url-retrieve
             url
             (lambda (_status)
               (let ((request (current-buffer)))
                 (unwind-protect
                     (condition-case nil
                         (progn
                           (goto-char (point-min))
                           (if (and (equal url-http-response-status 200)
                                    (zhihu-reader--generation-current-p
                                     buffer generation))
                               (progn
                                 (goto-char url-http-end-of-headers)
                                 (skip-chars-forward "\r\n")
                                 (make-directory zhihu-reader-image-cache t)
                                 (let ((data (buffer-substring-no-properties
                                              (point) (point-max))))
                                   (with-temp-file file
                                     (set-buffer-multibyte nil)
                                     (insert data))
                                   (when (buffer-live-p buffer)
                                     (zhihu-reader--replace-image-link
                                      marker file))))
                             (when (buffer-live-p buffer)
                               (zhihu-reader--image-failed marker url))))
                       (error
                        (when (buffer-live-p buffer)
                          (zhihu-reader--image-failed marker url))))
                   (when (buffer-live-p request)
                     (kill-buffer request))
                   (remhash url zhihu-reader--image-inflight)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (zhihu-reader--image-finish-loading))))))
             nil t)
          (error
           (remhash url zhihu-reader--image-inflight)
           (when (buffer-live-p buffer)
             (zhihu-reader--image-failed marker url)
             (with-current-buffer buffer
               (zhihu-reader--image-finish-loading)))
           (signal (car err) (cdr err))))))))

(defun zhihu-reader--close-open-org-blocks (&optional position)
  "Close unclosed #+begin_ blocks before POSITION.
Scan only the current subtree body and insert the closing lines at
POSITION (default point), so later headings/images cannot be swallowed."
  (let ((pos (or position (point)))
        (case-fold-search t)
        (stack nil))
    (save-excursion
      (ignore-errors
        (org-back-to-heading t)
        (forward-line 1)
        (when (looking-at ":PROPERTIES:")
          (re-search-forward "^:END:" nil t)
          (forward-line 1))
        (let ((start (point)))
          (goto-char start)
          (while (re-search-forward
                  "^\\(#\\+\\(begin\\|end\\)_\\([a-zA-Z0-9_-]+\\)\\)"
                  pos t)
            (let ((kind (match-string 2))
                  (name (downcase (match-string 3))))
              (if (string-equal kind "begin")
                  (push name stack)
                (when (equal (car stack) name)
                  (pop stack)))))))
      (when stack
        (goto-char pos)
        (dolist (name (reverse stack))
          (insert (format "#+end_%s\n" name)))))))

(defun zhihu-reader--insert-images (images)
  "Insert IMAGES as Org links and download them asynchronously."
  (when (and zhihu-reader-render-images (display-graphic-p))
    (setq-local org-image-actual-width (list zhihu-reader-image-width)))
  (dolist (url images)
    (let* ((file (and zhihu-reader-render-images
                      (zhihu-reader--image-cache-file url)))
           (cached (and file (zhihu-reader--image-valid-p file)))
           (marker nil))
      (if cached
          (insert (org-make-link-string (concat "file:" file) "图片"))
        (setq marker (copy-marker (point)))
        (insert (org-make-link-string url "图片")))
      (insert "\n")
      ;; Images are downloaded lazily by `zhihu-reader--process-buffer-images'
      ;; when the subtree is expanded or when `i' is pressed.
      (ignore marker)))
  (when (and zhihu-reader-render-images
             (display-graphic-p)
             images)
    (zhihu-reader--queue-image-display)))

(defun zhihu-reader--render-answer (item level)
  "Render answer ITEM at LEVEL."
  (let ((author (plist-get item :author))
        (votes (plist-get item :voteup-count))
        (time (zhihu-reader--time-string (plist-get item :created-time))))
    (zhihu-reader--heading
     level
     (format "回答%s%s%s"
             (if (string-empty-p author) "" (concat " · " author))
             (if (string-empty-p votes) "" (concat " · " votes " 赞"))
             (if time (concat " · " time) ""))
     (list (cons "ZHIHU-ANSWER" (plist-get item :id))
           (cons "ZHIHU-QUESTION" (plist-get item :question-id))
           (cons "ZHIHU-AUTHOR-TOKEN" (plist-get item :author-token))))
    (when (not (string-empty-p (or (plist-get item :content) "")))
      (zhihu-reader--body (plist-get item :content))
      (zhihu-reader--close-open-org-blocks))
    (when (plist-get item :images)
      (zhihu-reader--close-open-org-blocks)
      (zhihu-reader--insert-images (plist-get item :images)))
    (when (plist-get item :truncated)
      (insert "（内容有截断，RET 查看全文）\n"))
    (insert "\n")
    (zhihu-reader--comment-heading
     (1+ level) (plist-get item :comment-count) (plist-get item :id))
    (insert "\n")))

(defun zhihu-reader--render-question-detail (item level)
  "Render ITEM's question detail as a child heading at LEVEL."
  (let ((detail (plist-get item :question-detail))
        (images (plist-get item :question-images))
        (qid (plist-get item :question-id)))
    (when (or (and detail (not (string-empty-p detail))) images)
      (zhihu-reader--heading
       level "问题详情"
       (append (when (and qid (not (string-empty-p qid)))
                 (list (cons "ZHIHU-QUESTION" qid)))
               (list (cons "ZHIHU-QUESTION-DETAIL" "t"))))
      (when (and detail (not (string-empty-p detail)))
        (zhihu-reader--body detail))
      (when images
        (zhihu-reader--close-open-org-blocks)
        (zhihu-reader--insert-images images))
      (zhihu-reader--close-open-org-blocks)
      (insert "\n"))))

(defun zhihu-reader--render-group (group)
  "Render a group of items sharing a question/article."
  (let* ((first (car group))
         (kind (plist-get first :kind))
         (qid (plist-get first :question-id))
         (qtitle (plist-get first :question-title))
         (answers (seq-filter (lambda (item)
                                (equal (plist-get item :kind) "answer"))
                              group)))
    (cond
     ((equal kind "answer")
      (zhihu-reader--heading
       1 (or qtitle "知乎问题")
       (when (and qid (not (string-empty-p qid)))
         (list (cons "ZHIHU-QUESTION" qid))))
      (zhihu-reader--render-question-detail first 2)
      (dolist (item answers)
        (zhihu-reader--render-answer item 2)))
     ((equal kind "pin")
      (let ((item (car group)))
        (zhihu-reader--heading
         1 (or (plist-get item :title) "想法")
         (list (cons "ZHIHU-PIN" (plist-get item :id))))
        (zhihu-reader--body (plist-get item :content))
        (when (plist-get item :images)
          (zhihu-reader--close-open-org-blocks)
          (zhihu-reader--insert-images (plist-get item :images)))
        (zhihu-reader--close-open-org-blocks)
        (insert "\n")
        (zhihu-reader--comment-heading
         2 (plist-get item :comment-count) (plist-get item :id) "pin")
        (insert "\n")))
     ((equal kind "article")
      (let ((item (car group)))
        (zhihu-reader--heading
         1 (plist-get item :title)
         (list (cons "ZHIHU-ARTICLE" (plist-get item :id))))
        (zhihu-reader--body (plist-get item :content))
        (when (plist-get item :images)
          (zhihu-reader--close-open-org-blocks)
          (zhihu-reader--insert-images (plist-get item :images)))
        (zhihu-reader--close-open-org-blocks)
        (insert "\n")
        (zhihu-reader--comment-heading
         2 (plist-get item :comment-count) (plist-get item :id) "article")
        (insert "\n")))
     (t
      (let ((item (car group)))
        (zhihu-reader--heading
         1 (plist-get item :title)
         (list (cons "ZHIHU-QUESTION" (plist-get item :id))))
        (zhihu-reader--render-question-detail item 2)
        (when (and (plist-get item :content)
                   (not (string-empty-p (plist-get item :content))))
          (zhihu-reader--body (plist-get item :content)))
        (when (plist-get item :images)
          (zhihu-reader--close-open-org-blocks)
          (zhihu-reader--insert-images (plist-get item :images)))
        (insert "\n"))))))

(defun zhihu-reader--group-key (item)
  "Return a grouping key for ITEM."
  (pcase (plist-get item :kind)
    ("answer" (list 'question (plist-get item :question-id)))
    ("article" (list 'article (plist-get item :id)))
    (_ (list 'question (or (plist-get item :question-id)
                           (plist-get item :id))))))

(defun zhihu-reader--render-items (items)
  "Render ITEMS as grouped Org entries."
  (let ((groups (make-hash-table :test #'equal))
        (order nil))
    (dolist (item items)
      (let ((key (zhihu-reader--group-key item)))
        (unless (gethash key groups)
          (push key order))
        (push item (gethash key groups))))
    (dolist (key (nreverse order))
      (zhihu-reader--render-group (nreverse (gethash key groups))))
    (when (null order)
      (insert "没有返回内容。\n"))))

(defun zhihu-reader--apply-default-folds ()
  "Apply question/answer/comment expansion defaults to the current buffer."
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (org-overview))
    (when (or zhihu-reader-default-expand-questions
              zhihu-reader-default-expand-answers)
      (ignore-errors (org-fold-show-all)))
    (unless zhihu-reader-default-expand-answers
      (org-map-entries
       (lambda ()
         (when (org-entry-get nil "ZHIHU-ANSWER")
           (ignore-errors (org-fold-hide-subtree))))
       nil 'file))
    (unless zhihu-reader-default-expand-questions
      (org-map-entries
       (lambda ()
         (when (and (org-entry-get nil "ZHIHU-QUESTION")
                    (not (org-entry-get nil "ZHIHU-ANSWER")))
           (ignore-errors (org-fold-hide-subtree))))
       nil 'file))
    (unless zhihu-reader-default-expand-comments
      (org-map-entries
       (lambda ()
         (when (or (org-entry-get nil "ZHIHU-COMMENTS")
                   (org-entry-get nil "ZHIHU-COMMENT"))
           (ignore-errors (org-fold-hide-subtree))))
       nil 'file))))

(defun zhihu-reader--render (title items view &optional buffer-name)
  "Render ITEMS into an Org buffer.
TITLE is the buffer title, VIEW is stored for refresh, BUFFER-NAME is
the target buffer."
  (let ((buffer (get-buffer-create (or buffer-name zhihu-reader--main-buffer))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (zhihu-reader-mode)
        (setq-local zhihu-reader--generation (1+ zhihu-reader--generation))
        (setq-local zhihu-reader--view view)
        (setq-local header-line-format
                    (format " Zhihu %s  | TAB fold  RET enter  c comments  ] next  [ prev  i images  g refresh  q quit"
                            title))
        (insert (format "#+TITLE: %s\n#+STARTUP: overview\n\n" title))
        (zhihu-reader--render-items items)
        (goto-char (point-min))
        (zhihu-reader--apply-default-folds)
        (ignore-errors (org-fold-hide-drawer-all))
        (zhihu-reader--hide-drawers)
        (setq buffer-read-only t)))
    (pop-to-buffer buffer)
    (when (and zhihu-reader-render-images (display-graphic-p))
      (with-current-buffer buffer
        (if (memq (car-safe view) '(question answer article pin))
            (zhihu-reader--process-buffer-images)
          (zhihu-reader--queue-image-display))))
    (when (memq (car-safe view) '(question answer article pin))
      (with-current-buffer buffer
        (zhihu-reader--auto-load-comments)))
    (zhihu-reader--restore-navigation-anchor)
    ;; Re-apply the configured defaults after restoring the anchor so a
    ;; previous page's fold state cannot leak into this page.
    (with-current-buffer buffer
      (setq-local org-cycle-global-status nil)
      (zhihu-reader--apply-default-folds)
      (setq-local org-cycle-global-status 'overview))
    (when (and items (fboundp 'zhihu-reader--record-history))
      (let* ((kind (car-safe view))
             (id (nth 1 view))
             (first (car items))
             (title (or (plist-get first :question-title)
                        (plist-get first :title))))
        (pcase kind
          ('question
           (zhihu-reader--record-history
            "question" id title
            (format "https://www.zhihu.com/question/%s" id)))
          ('answer
           (zhihu-reader--record-history
            "answer" id title
            (format "https://www.zhihu.com/answer/%s" id)))
          ('article
           (zhihu-reader--record-history
            "article" id title
            (format "https://zhuanlan.zhihu.com/p/%s" id))))))))

(defun zhihu-reader--process-buffer-images (&optional beg end)
  "Download remote image links between BEG and END, and display local ones."
  (let ((count 0))
    (save-excursion
      (save-restriction
        (when (or beg end)
          (narrow-to-region (or beg (point-min)) (or end (point-max))))
        (goto-char (point-min))
        (while (re-search-forward "\\[\\[\\(https?://[^]\n]+\\)\\]\\[图片\\]\\]" nil t)
        (let ((url (match-string 1))
              (marker (copy-marker (match-beginning 0))))
          (unless (or (gethash url zhihu-reader--image-inflight)
                      (gethash url zhihu-reader--image-failed))
            (puthash url t zhihu-reader--image-inflight)
            (setq count (1+ count))
            (zhihu-reader--download-image-async url marker))))))
    (when (display-graphic-p)
      (zhihu-reader--queue-image-display))
    count))

(defvar-local zhihu-reader--zoom-level 0)

(defun zhihu-reader--apply-zoom ()
  "Apply text and image zoom for the current Zhihu buffer."
  (text-scale-set zhihu-reader--zoom-level)
  (when (and zhihu-reader-render-images (display-graphic-p))
    (setq-local org-image-actual-width
                (list (round (* zhihu-reader-image-width
                                (expt 1.15 zhihu-reader--zoom-level)))))
    (ignore-errors (org-display-inline-images t t))
    (ignore-errors (force-window-update (current-buffer)))))

(defun zhihu-reader-zoom-in ()
  "Zoom in text and inline images."
  (interactive)
  (setq zhihu-reader--zoom-level (1+ zhihu-reader--zoom-level))
  (zhihu-reader--apply-zoom)
  (message "缩放: %d" zhihu-reader--zoom-level))

(defun zhihu-reader-zoom-out ()
  "Zoom out text and inline images."
  (interactive)
  (setq zhihu-reader--zoom-level (1- zhihu-reader--zoom-level))
  (zhihu-reader--apply-zoom)
  (message "缩放: %d" zhihu-reader--zoom-level))

(defun zhihu-reader-zoom-reset ()
  "Reset text and image zoom."
  (interactive)
  (setq zhihu-reader--zoom-level 0)
  (zhihu-reader--apply-zoom)
  (message "缩放已重置"))

(defun zhihu-reader-clear-image-cache ()
  "Delete all cached Zhihu images."
  (interactive)
  (when (file-directory-p zhihu-reader-image-cache)
    (delete-directory zhihu-reader-image-cache t))
  (clrhash zhihu-reader--image-failed)
  (clrhash zhihu-reader--image-inflight)
  (message "已清空 %s" zhihu-reader-image-cache))

(defun zhihu-reader-toggle-images (&optional off)
  "Ensure images are rendered in the current buffer.
With prefix argument OFF, disable inline images instead."
  (interactive "P")
  (if off
      (progn
        (setq zhihu-reader-render-images nil)
        (when (fboundp 'org-remove-inline-images)
          (ignore-errors (org-remove-inline-images)))
        (message "图片渲染已关闭"))
    (setq zhihu-reader-render-images t)
    (let ((count (zhihu-reader--process-buffer-images)))
      (message "图片渲染已开启；处理了 %d 个图片链接" count))))

(defun zhihu-reader-read-url-from-point ()
  "Open the URL at point internally when it is a Zhihu URL.
Non-Zhihu URLs keep the EAF/Readability fallback."
  (interactive)
  (let ((url (zhihu-reader--current-url)))
    (if (and url (not (string-empty-p url)))
        (if (and (fboundp 'zhihu-reader-open-zhihu-url)
                 (zhihu-reader--zhihu-url-p url))
            (zhihu-reader-open-zhihu-url url)
          (zhihu-reader-read-url url))
      (user-error "光标不在链接或知乎标题上"))))

(defun zhihu-reader-help ()
  "显示知乎阅读器的按键说明。"
  (interactive)
  (with-help-window "*Zhihu Help*"
    (princ
     "知乎阅读器

【浏览】
  TAB       折叠 / 展开当前标题
  RET       进入问题、回答或打开链接
  n / p     下一个 / 上一个标题
  ] / [     下一页 / 上一页
  g         刷新当前视图
  q         关闭当前窗口
  ?         显示本帮助

【评论】
  c         加载评论或子评论
  C-c ]     下一页评论
  C-c [     上一页评论
  C-c o     切换评论排序（score / time）
  C-c l     点赞 / 取消点赞评论
  C-c d     删除评论
  C-c c     发表评论或回复
  C-c C-o   在新 buffer 打开评论/回复
  C-c C-r   通知全部标为已读
  C-c C-d   从当前收藏夹移除
  C-c I     私信
  C-c C-c   在私信 buffer 发送消息

【内容操作】
  C-c v     赞同 / 取消赞同；C-u 反对
  C-c f     关注 / 取消关注
  C-c t     感谢 / 取消感谢回答
  C-c F     加入收藏夹
  C-c b     收藏当前内容为书签
  C-c B     打开书签列表
  C-c m     标记已读
  C-c u     标记未读
  C-c a     查看作者资料
  C-c A     查看作者回答列表
  e         编辑回答或评论

【分享与阅读】
  w         复制分享链接（🔗）
  C-c w     复制分享链接
  o         在 Emacs 内打开当前知乎链接
  i         切换内联图片
  C-c +     放大文字和图片
  C-c -     缩小文字和图片
  C-c 0     重置缩放
  C-c C-h   阅读历史
  C-c C-l   加入稍后读
  C-c C-L   打开稍后读

【入口】
  r         推荐流
  h         热榜
  C-c M     通知
  s         搜索
  S         回答排序（问题视图）

全局快捷键：
  C-c Z     推荐流
  C-c H     热榜
  C-c S     搜索
  C-c C-Z   通过 URL 阅读
  C-c G     关注动态
  C-c T     话题
  C-c L     收藏夹
")))
(defvar zhihu-reader-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zhihu-reader-open-at-point)
    (define-key map (kbd "]") #'zhihu-reader-next-page)
    (define-key map (kbd "[") #'zhihu-reader-previous-page)
    (define-key map (kbd "i") #'zhihu-reader-toggle-images)
    (define-key map (kbd "g") #'zhihu-reader-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "n") #'org-next-visible-heading)
    (define-key map (kbd "p") #'org-previous-visible-heading)
    (define-key map (kbd "c") #'zhihu-reader-comments)
    (define-key map (kbd "C-c ]") #'zhihu-reader-comments-more)
    (define-key map (kbd "C-c [") #'zhihu-reader-comments-previous)
    (define-key map (kbd "C-c o") #'zhihu-reader-comment-sort)
    (define-key map (kbd "C-c l") #'zhihu-reader-like-comment)
    (define-key map (kbd "C-c d") #'zhihu-reader-delete-comment)
    (define-key map (kbd "C-c v") #'zhihu-reader-vote)
    (define-key map (kbd "C-c f") #'zhihu-reader-follow)
    (define-key map (kbd "C-c t") #'zhihu-reader-thank)
    (define-key map (kbd "C-c F") #'zhihu-reader-favorite)
    (define-key map (kbd "C-c b") #'zhihu-reader-bookmark)
    (define-key map (kbd "C-c B") #'zhihu-reader-bookmarks)
    (define-key map (kbd "C-c m") #'zhihu-reader-mark-read)
    (define-key map (kbd "C-c u") #'zhihu-reader-mark-unread)
    (define-key map (kbd "C-c c") #'zhihu-reader-compose-comment)
    (define-key map (kbd "C-c C-o") #'zhihu-reader-open-comments-buffer)
    (define-key map (kbd "C-c C-r") #'zhihu-reader-notifications-readall)
    (define-key map (kbd "C-c C-d") #'zhihu-reader-remove-from-collection)
    (define-key map (kbd "C-c C-c") #'zhihu-reader-chat-send)
    (define-key map (kbd "C-c +") #'zhihu-reader-zoom-in)
    (define-key map (kbd "C-c -") #'zhihu-reader-zoom-out)
    (define-key map (kbd "C-c 0") #'zhihu-reader-zoom-reset)
    (define-key map (kbd "C-c C-h") #'zhihu-reader-history)
    (define-key map (kbd "C-c C-l") #'zhihu-reader-read-later)
    (define-key map (kbd "C-c C-L") #'zhihu-reader-later-list)
    (define-key map (kbd "C-c a") #'zhihu-reader-open-author)
    (define-key map (kbd "C-c A") #'zhihu-reader-author)
    (define-key map (kbd "C-c n") #'zhihu-reader-new-answer)
    (define-key map (kbd "C-c N") #'zhihu-reader-new-article)
    (define-key map (kbd "C-c s") #'zhihu-reader-save-draft)
    (define-key map (kbd "e") #'zhihu-reader-edit-at-point)
    (define-key map (kbd "o") #'zhihu-reader-read-url-from-point)
    (define-key map (kbd "w") #'zhihu-reader-copy-share-link)
    (define-key map (kbd "C-c w") #'zhihu-reader-copy-share-link)
    (define-key map (kbd "s") #'zhihu-reader-search)
    (define-key map (kbd "S") #'zhihu-reader-sort)
    (define-key map (kbd "h") #'zhihu-reader-hot)
    (define-key map (kbd "r") #'zhihu-reader-feed)
    (define-key map (kbd "?") #'zhihu-reader-help)
    map)
  "Keymap for `zhihu-reader-mode'.")

(defun zhihu-reader--auto-load-comments (&optional beg end)
  "Auto-load comments for visible comment headings.
Limit to `zhihu-reader-auto-comments-limit' sections."
  (when (and zhihu-reader-auto-comments
             (derived-mode-p 'zhihu-reader-mode))
    (let ((count 0))
      (save-restriction
        (when (or beg end)
          (narrow-to-region (or beg (point-min)) (or end (point-max))))
        (org-map-entries
         (lambda ()
           (when (< count zhihu-reader-auto-comments-limit)
             (let* ((id (org-entry-get nil "ZHIHU-COMMENTS"))
                    (loaded (org-entry-get nil "ZHIHU-COMMENTS-LOADED"))
                    (heading (org-get-heading t t t t))
                    (kind (or (org-entry-get nil "ZHIHU-COMMENTS-KIND") "answer")))
               (when (and id (not loaded)
                          (not (gethash id zhihu-reader--comments-inflight))
                          (string-match "评论 (\\([0-9]+\\))" heading)
                          (> (string-to-number (match-string 1 heading)) 0))
                 (puthash id t zhihu-reader--comments-inflight)
                 (setq count (1+ count))
                 (zhihu-reader--load-comments-async
                  id (point-marker) kind nil
                  zhihu-reader-default-expand-comments))))))))))

(defun zhihu-reader--isolate-current-subtree ()
  "Collapse top-level questions that do not contain the current heading.
This is a belt-and-braces defense against Org expanding sibling
questions when a subtree is shown, even if global fold state leaked."
  (let ((current (save-excursion
                   (org-back-to-heading t)
                   (point))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (let* ((heading (match-beginning 0))
               (end (save-excursion
                      (goto-char heading)
                      (org-end-of-subtree t t)
                      (point))))
          (unless (and (<= heading current) (<= current end))
            (ignore-errors
              (goto-char heading)
              (org-fold-hide-subtree)
              ;; Continue after this subtree; hiding can leave point at
              ;; the heading and otherwise loop forever.
              (goto-char end))))))))

(defun zhihu-reader--after-cycle (state &rest _)
  "Load comments and images after a visibility STATE change."
  (unless (eq state 'folded)
    (when (derived-mode-p 'zhihu-reader-mode)
      (condition-case nil
          (save-excursion
            (org-back-to-heading t)
            (let ((beg (point))
                  (end (save-excursion (org-end-of-subtree t t))))
              ;; Comments must not depend on image rendering being enabled.
              (zhihu-reader--auto-load-comments beg end)
              (when (and zhihu-reader-render-images (display-graphic-p))
                (zhihu-reader--process-buffer-images beg end))
              ;; Refresh only the current subtree; never expand every
              ;; top-level question after a page switch.  Narrowing also
              ;; isolates us from a global fold state left by the prior page.
              (save-restriction
                (ignore-errors (org-narrow-to-subtree))
                (ignore-errors (org-fold-show-all)))
              (zhihu-reader--isolate-current-subtree)
              (ignore-errors (force-window-update (current-buffer)))))
        (error nil)))))

(defun zhihu-reader--mode-line-unread ()
  "Return a mode-line unread badge when available."
  (when (> zhihu-reader--unread-total 0)
    (propertize (format " [知乎 %d]" zhihu-reader--unread-total)
                'face 'warning)))

(define-derived-mode zhihu-reader-mode org-mode "Zhihu"
  "Major mode for Zhihu reader buffers."
  (setq-local org-startup-folded 'overview)
  (setq-local truncate-lines nil)
  (setq-local org-display-remote-inline-images 'skip)
  (add-hook 'org-cycle-hook #'zhihu-reader--after-cycle nil t)
  (setq-local mode-line-misc-info
              (cons '(:eval (zhihu-reader--mode-line-unread))
                    mode-line-misc-info)))

(provide 'zhihu-reader-ui)
;;; zhihu-reader-ui.el ends here
