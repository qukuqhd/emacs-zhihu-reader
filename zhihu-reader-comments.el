;;; zhihu-reader-comments.el --- Comments and composition for Zhihu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 qukuqhd
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Part of emacs-zhihu-reader.  See zhihu-reader.el for the entry point.

;;; Code:

(require 'zhihu-reader-core)
(require 'zhihu-reader-api)
(require 'zhihu-reader-ui)

;;;; Detail APIs

(defun zhihu-reader--question-answers-url (question-id &optional page order)
  "Return the answers API URL for QUESTION-ID, PAGE and ORDER."
  (format "https://www.zhihu.com/api/v4/questions/%s/answers?include=data%%5B*%%5D.content%%2Cexcerpt%%2Cauthor%%2Ccomment_count%%2Cvoteup_count%%2Cquestion.detail&limit=20&offset=%d&order=%s"
          question-id (* 20 (1- (or page 1))) (or order "default")))

(defun zhihu-reader--answer-url (answer-id)
  "Return the answer detail API URL for ANSWER-ID."
  (format "https://www.zhihu.com/api/v4/answers/%s?include=content%%2Cexcerpt%%2Cauthor%%2Cquestion%%2Ccomment_count%%2Cvoteup_count"
          answer-id))

(defun zhihu-reader--comments-url (id &optional kind cursor)
  "Return root comments URL for ID, KIND and optional CURSOR."
  (if (and cursor (stringp cursor)
           (string-prefix-p "https://www.zhihu.com/api/v4/comment_v5/" cursor))
      cursor
    (let ((type (if (equal kind "article") "articles" "answers")))
      (format "https://www.zhihu.com/api/v4/comment_v5/%s/%s/root_comment?order_by=%s"
              type id (if (equal zhihu-reader-comment-order "time") "ts" "score")))))

(defun zhihu-reader--child-comments-url (comment-id &optional cursor)
  "Return child comments URL for COMMENT-ID and optional CURSOR."
  (if (and cursor (stringp cursor)
           (string-prefix-p "https://www.zhihu.com/api/v4/comment_v5/" cursor))
      cursor
    (format "https://www.zhihu.com/api/v4/comment_v5/comment/%s/child_comment?limit=20&order=normal&status=open"
            comment-id)))

(defun zhihu-reader--fetch-question-detail (question-id)
  "Fetch and return QUESTION-ID detail with a native ZSE signature."
  (let* ((url (format "https://www.zhihu.com/api/v4/questions/%s?include=detail"
                      question-id))
         (resp (zhihu-reader--http-json "GET" url nil t))
         (json (plist-get resp :json)))
    (when (and (memq (plist-get resp :status) '(200 201))
               (zhihu-reader--alist-get 'detail json))
      json)))

(defun zhihu-reader--attach-question-detail (items question-id)
  "Attach question detail and images to ITEMS."
  (let ((detail (zhihu-reader--fetch-question-detail question-id)))
    (if (not detail)
        items
      (let ((html (or (zhihu-reader--alist-get 'detail detail) "")))
        (mapcar
         (lambda (item)
           (setq item (plist-put item :question-detail
                                 (zhihu-reader--html-to-org html)))
           (plist-put item :question-images
                      (zhihu-reader--extract-images html)))
         items)))))

(defun zhihu-reader--id-or-url (value)
  "Extract a numeric Zhihu id from VALUE, which may be an id or URL."
  (cond
   ((string-match "\\([0-9]\\{6,\\}\\)" (or value ""))
    (match-string 1 value))
   (t (user-error "无法从 %s 解析知乎 ID" value))))

(defun zhihu-reader-question (id-or-url &optional page order force)
  "Open question ID-OR-URL with its answer list.
PAGE selects the answer page; ORDER is `default', `updated' or `created'.
With FORCE non-nil, bypass the page cache."
  (interactive (list (read-string "知乎问题 ID 或 URL: ")
                     (prefix-numeric-value current-prefix-arg)
                     nil
                     nil))
  (let* ((id (zhihu-reader--id-or-url id-or-url))
         (page (or page 1))
         (order (or order "default"))
         (key (list 'question id page order))
         (url (zhihu-reader--question-answers-url id page order))
         (title (format "问题 %s · 第 %d 页 · %s" id page order)))
    (if (and (not force) (zhihu-reader--show-cached key))
        (message "使用缓存的问题第 %d 页" page)
      (if noninteractive
          (let* ((items (zhihu-reader--normalize-items
                         (zhihu-reader--fetch-json url)))
                 (qtitle (or (plist-get (car items) :question-title)
                             (format "问题 %s" id))))
            (puthash key
                     (cons (format "问题 · %s · 第 %d 页 · %s"
                                   qtitle page order)
                           items)
                     zhihu-reader--page-cache)
            (zhihu-reader--render (format "问题 · %s · 第 %d 页 · %s"
                                         qtitle page order)
                                  items key
                                  zhihu-reader--question-buffer)
            (zhihu-reader--restore-point key))
        (zhihu-reader--async-list
         title key url zhihu-reader--question-buffer
         (lambda (json)
           (let* ((items (zhihu-reader--normalize-items json))
                  (qtitle (or (plist-get (car items) :question-title)
                              (format "问题 %s" id))))
             (cons (format "问题 · %s · 第 %d 页 · %s" qtitle page order)
                   items)))
         key)))))

(defun zhihu-reader-answer (answer-id)
  "Open answer ANSWER-ID with its comments."
  (interactive "s知乎回答 ID: ")
  (let* ((id (zhihu-reader--id-or-url answer-id))
         (url (zhihu-reader--answer-url id)))
    (if noninteractive
        (let* ((json (zhihu-reader--fetch-json url))
               (items (zhihu-reader--normalize-items
                       (list (cons 'data (vector json)))))
               (title (or (plist-get (car items) :question-title)
                          (format "回答 %s" id))))
          (zhihu-reader--render (format "回答 · %s" title)
                                items (list 'answer id)
                                zhihu-reader--answer-buffer))
      (zhihu-reader--async-list
       (format "回答 %s" id) (list 'answer id) url zhihu-reader--answer-buffer
       (lambda (json)
         (let* ((items (zhihu-reader--normalize-items
                        (list (cons 'data (vector json)))))
                (title (or (plist-get (car items) :question-title)
                           (format "回答 %s" id))))
           (cons (format "回答 · %s" title) items)))))))

(defcustom zhihu-reader-comments-separate-buffer nil
  "When non-nil, \`c' opens comments and replies in a separate buffer."
  :type 'boolean
  :group 'zhihu-reader)

(defvar-local zhihu-reader--comments-buffer nil
  "Non-nil when the current buffer is a standalone comments buffer.")

;;;; Comments

(defun zhihu-reader--normalize-comments (json)
  "Return comment plists from JSON."
  (let ((data (zhihu-reader--alist-get 'data json)))
    (when (vectorp data)
      (delq nil
            (mapcar
             (lambda (comment)
               (let* ((author (zhihu-reader--alist-get 'author comment))
                      (content (zhihu-reader--html-to-org
                                (zhihu-reader--alist-get 'content comment)))
                      (child-count (or (zhihu-reader--alist-get 'child_comment_count comment)
                                       (length (alist-get 'child_comments comment)))))
                 (when (not (string-empty-p content))
                   (list :id (zhihu-reader--string
                              (zhihu-reader--alist-get 'id comment))
                         :author (zhihu-reader--first
                                  (zhihu-reader--alist-get 'name author)
                                  (zhihu-reader--alist-get
                                   'name (zhihu-reader--alist-get 'member author))
                                  (zhihu-reader--alist-get 'author_name comment))
                         :content content
                         :author-token (zhihu-reader--string
                                        (or (zhihu-reader--alist-get 'url_token author)
                                            (zhihu-reader--alist-get
                                             'url_token
                                             (zhihu-reader--alist-get 'member author))))
                         :child-count (zhihu-reader--string child-count)
                         :votes (zhihu-reader--string
                                 (or (zhihu-reader--alist-get 'vote_count comment)
                                     (zhihu-reader--alist-get 'voteup_count comment)))
                         :time (zhihu-reader--time-string
                                (zhihu-reader--alist-get 'created_time comment))))))
             (append data nil))))))

(defun zhihu-reader--insert-comments (comments level)
  "Insert COMMENTS at Org LEVEL."
  (dolist (comment comments)
    (let ((id (plist-get comment :id))
          (child-count (plist-get comment :child-count)))
      (zhihu-reader--heading
       level
       (format "%s%s%s%s"
               (or (plist-get comment :author) "匿名")
               (let ((votes (plist-get comment :votes)))
                 (if (and votes (not (string-empty-p votes)))
                     (format " · %s 赞" votes) ""))
               (let ((time (plist-get comment :time)))
                 (if time (concat " · " time) ""))
               (let ((count (and child-count
                                 (not (string= child-count "0"))
                                 child-count)))
                 (if count (format " · 回复 %s" count) "")))
       (when (and id (not (string-empty-p id)))
         (list (cons "ZHIHU-COMMENT" id)
               (cons "ZHIHU-CHILD-COUNT" (or child-count "0"))
               (cons "ZHIHU-AUTHOR-TOKEN"
                     (or (plist-get comment :author-token) "")))))
      (when (not (string-empty-p (or (plist-get comment :content) "")))
        (zhihu-reader--body (plist-get comment :content))
        (zhihu-reader--close-open-org-blocks))
      (insert "\n"))))

(defun zhihu-reader--clear-comment-body (marker)
  "Remove existing comments below MARKER, preserving heading properties."
  (when (and (markerp marker)
             (marker-buffer marker)
             (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (org-back-to-heading t)
          (let ((start (progn
                         (forward-line 1)
                         (when (looking-at ":PROPERTIES:")
                           (re-search-forward "^:END:" nil t)
                           (forward-line 1))
                         (point)))
                (end (save-excursion
                       (org-back-to-heading t)
                       (org-end-of-subtree t t))))
            (when (< start end)
              (delete-region start end))))))))

(defun zhihu-reader--set-comment-paging (marker json)
  "Set cursor properties at MARKER from comment JSON."
  (when (and (markerp marker) (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (let* ((paging (zhihu-reader--alist-get 'paging json))
             (next (zhihu-reader--alist-get 'next paging))
             (previous (zhihu-reader--alist-get 'previous paging))
             (is-end (zhihu-reader--alist-get 'is_end paging)))
        (save-excursion
          (goto-char marker)
          (org-entry-put nil "ZHIHU-COMMENTS-NEXT" (or next ""))
          (org-entry-put nil "ZHIHU-COMMENTS-PREV" (or previous ""))
          (org-entry-put nil "ZHIHU-COMMENTS-HAS-NEXT"
                         (if (and next (not (eq is-end t))) "1" "0"))
          (org-entry-put nil "ZHIHU-COMMENTS-HAS-PREV" (if previous "1" "0")))))))

(defun zhihu-reader--load-comments-at (id marker kind &optional cursor)
  "Load a root comments page for ID at MARKER.
KIND is \"answer\" or \"article\"; CURSOR is an optional paging URL."
  (let* ((json (zhihu-reader--fetch-json
                (zhihu-reader--comments-url id kind cursor)))
         (comments (zhihu-reader--normalize-comments json))
         (level (save-excursion
                  (goto-char marker)
                  (org-back-to-heading t)
                  (1+ (org-current-level)))))
    (let ((inhibit-read-only t))
      (goto-char marker)
      (org-back-to-heading t)
      (org-end-of-subtree)
      (insert "\n")
      (unless comments
        (message "知乎返回 0 条评论（可能已删除或折叠）"))
      (zhihu-reader--clear-comment-body marker)
      (zhihu-reader--insert-comments comments level)
      (zhihu-reader--set-comment-paging marker json)
      (zhihu-reader--set-comment-status
       marker (if comments "loaded" "empty") (length comments))
      (save-excursion
        (goto-char marker)
        (org-entry-put nil "ZHIHU-COMMENTS-LOADED" "t"))
      (zhihu-reader--show-comment-subtree marker)
      (zhihu-reader--hide-drawers)
      (setq buffer-read-only t))))

(defun zhihu-reader--load-comments-async (id marker kind &optional cursor show)
  "Load a root comments page for ID asynchronously at MARKER.
KIND is \"answer\" or \"article\"; CURSOR is an optional paging URL.
When SHOW is non-nil, expand the comments subtree."
  (let ((buffer (current-buffer))
        (generation zhihu-reader--generation)
        (level (save-excursion
                 (goto-char marker)
                 (org-back-to-heading t)
                 (1+ (org-current-level)))))
    (zhihu-reader--set-comment-status marker "loading")
    (zhihu-reader--fetch-json-async
     (zhihu-reader--comments-url id kind cursor)
     (lambda (json)
       (unwind-protect
           (when (zhihu-reader--generation-current-p buffer generation)
             (with-current-buffer buffer
               (let ((inhibit-read-only t))
                 (goto-char marker)
                 (org-back-to-heading t)
                 (org-end-of-subtree)
                 (insert "\n")
                 (let ((comments (zhihu-reader--normalize-comments json)))
                   (unless comments
                     (message "知乎返回 0 条评论（可能已删除或折叠）"))
                   (zhihu-reader--clear-comment-body marker)
                   (zhihu-reader--insert-comments comments level)
                   (zhihu-reader--set-comment-paging marker json)
                   (zhihu-reader--set-comment-status
                    marker (if comments "loaded" "empty") (length comments)))
                 (save-excursion
                   (goto-char marker)
                   (org-entry-put nil "ZHIHU-COMMENTS-LOADED" "t"))
                 (if show
                     (zhihu-reader--show-comment-subtree marker)
                   (ignore-errors (org-fold-hide-subtree)))
                 (zhihu-reader--hide-drawers)
                 (setq buffer-read-only t))))
         (remhash id zhihu-reader--comments-inflight)))
     (lambda (error)
       (remhash id zhihu-reader--comments-inflight)
       (zhihu-reader--set-comment-status marker "failed")
       (message "评论加载失败: %s" error)))))

(defun zhihu-reader--set-child-paging (marker json)
  "Store child comment cursors at MARKER."
  (when (and (markerp marker) (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (let* ((paging (zhihu-reader--alist-get 'paging json))
             (next (zhihu-reader--alist-get 'next paging))
             (previous (zhihu-reader--alist-get 'previous paging))
             (is-end (zhihu-reader--alist-get 'is_end paging)))
        (save-excursion
          (goto-char marker)
          (org-entry-put nil "ZHIHU-CHILDREN-NEXT" (or next ""))
          (org-entry-put nil "ZHIHU-CHILDREN-PREV" (or previous ""))
          (org-entry-put nil "ZHIHU-CHILDREN-HAS-NEXT"
                         (if (and next (not (eq is-end t))) "1" "0"))
          (org-entry-put nil "ZHIHU-CHILDREN-HAS-PREV" (if previous "1" "0")))))))

(defun zhihu-reader--load-child-comments-async
    (comment-id marker &optional cursor show replace)
  "Load child comments for COMMENT-ID asynchronously at MARKER.
CURSOR selects the page; SHOW expands the subtree; REPLACE clears the old
child comments first (used by previous-page navigation)."
  (let ((buffer (current-buffer))
        (generation zhihu-reader--generation)
        (level (save-excursion
                 (goto-char marker)
                 (org-back-to-heading t)
                 (1+ (org-current-level)))))
    (zhihu-reader--fetch-json-async
     (zhihu-reader--child-comments-url comment-id cursor)
     (lambda (json)
       (when (zhihu-reader--generation-current-p buffer generation)
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (goto-char marker)
             (org-back-to-heading t)
             (when replace
               (zhihu-reader--clear-comment-body marker)
               (goto-char marker)
               (org-back-to-heading t))
             (org-end-of-subtree)
             (insert "\n")
             (let ((comments (zhihu-reader--normalize-comments json)))
               (unless comments
                 (message "知乎返回 0 条评论（可能已删除或折叠）"))
               (zhihu-reader--insert-comments comments level))
             (save-excursion
               (goto-char marker)
               (org-entry-put nil "ZHIHU-CHILDREN-LOADED" "t")
               (zhihu-reader--set-child-paging marker json)
               (if show
                   (zhihu-reader--show-comment-subtree marker)
                 (ignore-errors (org-fold-hide-subtree)))
               (zhihu-reader--hide-drawers))
             (setq buffer-read-only t)))))
     (lambda (error)
       (message "子评论加载失败: %s" error)))))

(defun zhihu-reader--comment-target ()
  "Return (ROOT ID KIND MARKER COMMENT-ID) for the heading at point."
  (let* ((comment-id (org-entry-get nil "ZHIHU-COMMENT"))
         (marker (point-marker))
         (root (null comment-id))
         (id nil)
         (kind "answer"))
    (unless comment-id
      (setq id (or (org-entry-get nil "ZHIHU-COMMENTS")
                   (save-excursion
                     (when (or (org-entry-get nil "ZHIHU-ANSWER")
                               (org-entry-get nil "ZHIHU-ARTICLE"))
                       (org-end-of-subtree)
                       (when (re-search-backward
                              "^:ZHIHU-COMMENTS: \\([0-9]+\\)" nil t)
                         (org-back-to-heading t)
                         (match-string 1))))))
      (setq kind (or (org-entry-get nil "ZHIHU-COMMENTS-KIND") "answer")))
    (unless (or id comment-id)
      (user-error "光标不在评论标题上"))
    (list root id kind marker comment-id)))

(defun zhihu-reader-comment-sort ()
  "Change root comment sort order and reload the first page."
  (interactive)
  (let* ((target (zhihu-reader--comment-target))
         (root (nth 0 target))
         (id (nth 1 target))
         (kind (nth 2 target))
         (marker (nth 3 target)))
    (unless root
      (user-error "只能在根评论标题上切换排序"))
    (setq zhihu-reader-comment-order
          (completing-read "评论排序: "
                           '("score" "time") nil t nil nil
                           zhihu-reader-comment-order))
    (save-excursion
      (goto-char marker)
      (org-entry-put nil "ZHIHU-COMMENTS-LOADED" nil))
    (message "评论排序: %s" zhihu-reader-comment-order)
    (zhihu-reader--load-comments-async id marker kind nil t)))

(defun zhihu-reader-like-comment ()
  "Toggle like on the comment at point."
  (interactive)
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (let* ((id (org-entry-get nil "ZHIHU-COMMENT"))
         (liked (equal (org-entry-get nil "ZHIHU-COMMENT-LIKED") "t")))
    (unless id
      (user-error "光标不在评论标题上"))
    (let* ((url (format "https://www.zhihu.com/api/v4/comments/%s/like" id))
           (resp (zhihu-reader--http-json (if liked "DELETE" "POST") url nil))
           (status (plist-get resp :status)))
      (unless (memq status '(200 201 202 204))
        (error "评论点赞失败 (HTTP %s): %s" status (plist-get resp :body)))
      (org-entry-put nil "ZHIHU-COMMENT-LIKED" (if liked "nil" "t"))
      (message (if liked "已取消点赞" "已点赞")))))

(defun zhihu-reader-comments-more ()
  "Load the next page of comments at point."
  (interactive)
  (let* ((target (zhihu-reader--comment-target))
         (root (nth 0 target))
         (id (nth 1 target))
         (kind (nth 2 target))
         (marker (nth 3 target))
         (comment-id (nth 4 target)))
    (if root
        (let ((next (org-entry-get nil "ZHIHU-COMMENTS-NEXT")))
          (when (or (null next) (string-empty-p next))
            (user-error "已经是最后一页评论"))
          (message "加载下一页评论…")
          (zhihu-reader--load-comments-async id marker kind next t))
      (let ((next (org-entry-get nil "ZHIHU-CHILDREN-NEXT")))
        (when (or (null next) (string-empty-p next))
          (user-error "已经是最后一页子评论"))
        (message "加载下一页子评论…")
        (zhihu-reader--load-child-comments-async
         comment-id marker next t)))))

(defun zhihu-reader-comments-previous ()
  "Load the previous page of comments at point."
  (interactive)
  (let* ((target (zhihu-reader--comment-target))
         (root (nth 0 target))
         (id (nth 1 target))
         (kind (nth 2 target))
         (marker (nth 3 target))
         (comment-id (nth 4 target)))
    (if root
        (let ((previous (org-entry-get nil "ZHIHU-COMMENTS-PREV")))
          (when (or (null previous) (string-empty-p previous))
            (user-error "已经是第一页评论"))
          (message "加载上一页评论…")
          (zhihu-reader--load-comments-async id marker kind previous t))
      (let ((previous (org-entry-get nil "ZHIHU-CHILDREN-PREV")))
        (when (or (null previous) (string-empty-p previous))
          (user-error "已经是第一页子评论"))
        (message "加载上一页子评论…")
        (zhihu-reader--load-child-comments-async
         comment-id marker previous t t)))))

(defun zhihu-reader--open-comments-buffer (target)
  "Open TARGET comments or replies in a standalone Org buffer."
  (let* ((root (nth 0 target))
         (id (nth 1 target))
         (kind (nth 2 target))
         (comment-id (nth 4 target))
         (root-id (if comment-id
                      (or (org-entry-get nil "ZHIHU-COMMENTS" t)
                          (org-entry-get nil "ZHIHU-COMMENT" t))
                    id))
         (title (if comment-id "知乎评论回复" "知乎评论"))
         (buffer (get-buffer-create
                  (if comment-id "*Zhihu Comment Replies*" "*Zhihu Comments*")))
         marker)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (zhihu-reader-mode)
        (setq-local zhihu-reader--comments-buffer t)
        (insert (format "#+TITLE: %s\n#+STARTUP: overview\n\n" title))
        (if comment-id
            (insert (format "* 评论\n:PROPERTIES:\n:ZHIHU-COMMENT: %s\n:ZHIHU-COMMENTS: %s\n:ZHIHU-COMMENTS-KIND: %s\n:END:\n"
                            comment-id (or root-id id) kind))
          (insert (format "* 评论\n:PROPERTIES:\n:ZHIHU-COMMENTS: %s\n:ZHIHU-COMMENTS-KIND: %s\n:END:\n"
                          id kind)))
        (goto-char (point-min))
        (re-search-forward "^\\* " nil t)
        (setq marker (copy-marker (match-beginning 0)))
        (setq buffer-read-only t)
        (if comment-id
            (zhihu-reader--load-child-comments-async comment-id marker nil t)
          (zhihu-reader--load-comments-async id marker kind nil t))))
    (pop-to-buffer buffer)
    (message "已在新 buffer 打开%s" (if comment-id "回复" "评论"))))

(defun zhihu-reader-comments ()
  "Load root or child comments at point.
When `zhihu-reader-comments-separate-buffer' is non-nil and this is not
already a standalone comments buffer, open them in a new buffer."
  (interactive)
  (if (and zhihu-reader-comments-separate-buffer
           (not zhihu-reader--comments-buffer))
      (zhihu-reader--open-comments-buffer (zhihu-reader--comment-target))
    (let ((comment-id (org-entry-get nil "ZHIHU-COMMENT"))
        (id nil)
        (kind "answer")
        (root t))
    (if comment-id
        (setq root nil)
      (setq id (or (org-entry-get nil "ZHIHU-COMMENTS")
                   (save-excursion
                     (when (or (org-entry-get nil "ZHIHU-ANSWER")
                               (org-entry-get nil "ZHIHU-ARTICLE"))
                       (org-end-of-subtree)
                       (when (re-search-backward
                              "^:ZHIHU-COMMENTS: \\([0-9]+\\)" nil t)
                         (org-back-to-heading t)
                         (match-string 1))))))
      (setq kind (or (org-entry-get nil "ZHIHU-COMMENTS-KIND") "answer")))
    (unless (or id comment-id)
      (user-error "光标不在评论标题上"))
    (let* ((marker (point-marker))
           (loaded (if root
                       (org-entry-get nil "ZHIHU-COMMENTS-LOADED")
                     (org-entry-get nil "ZHIHU-CHILDREN-LOADED"))))
      (cond
       (loaded
        (message "评论已经加载"))
       (root
        (if noninteractive
            (zhihu-reader--load-comments-at id marker kind)
          (message "评论加载中…")
          (zhihu-reader--load-comments-async id marker kind nil t)))
       (t
        (if noninteractive
            (let* ((json (zhihu-reader--fetch-json
                          (zhihu-reader--child-comments-url comment-id)))
                   (comments (zhihu-reader--normalize-comments json)))
              (goto-char marker)
              (org-back-to-heading t)
              (let ((inhibit-read-only t)
                    (level (1+ (org-current-level))))
                (org-end-of-subtree)
                (insert "\n")
                (zhihu-reader--insert-comments comments level)
                (org-entry-put nil "ZHIHU-CHILDREN-LOADED" "t")
                (zhihu-reader--set-child-paging marker json)
                (zhihu-reader--show-comment-subtree marker)
                (zhihu-reader--hide-drawers)
                (setq buffer-read-only t)))
          (message "子评论加载中…")
          (zhihu-reader--load-child-comments-async comment-id marker nil t))))))))

(defun zhihu-reader-open-comments-buffer ()
  "Always open comments/replies at point in a separate buffer."
  (interactive)
  (zhihu-reader--open-comments-buffer (zhihu-reader--comment-target)))

(defvar-local zhihu-reader-compose-kind nil)
(defvar-local zhihu-reader-compose-id nil)
(defvar-local zhihu-reader-compose-root-kind nil)
(defvar-local zhihu-reader-compose-root-id nil)

(defun zhihu-reader--open-compose-buffer (kind id label &optional root-kind root-id)
  "Open a compose buffer for KIND/ID with LABEL."
  (let ((buffer (get-buffer-create "*Zhihu Comment*")))
    (with-current-buffer buffer
      (erase-buffer)
      (text-mode)
      (setq-local zhihu-reader-compose-kind kind)
      (setq-local zhihu-reader-compose-id id)
      (setq-local zhihu-reader-compose-root-kind (or root-kind kind))
      (setq-local zhihu-reader-compose-root-id (or root-id id))
      (setq-local header-line-format
                  (format " 评论 %s  | C-c C-p 发布  C-c C-k 取消" label))
      (local-set-key (kbd "C-c C-p") #'zhihu-reader-publish-comment)
      (local-set-key (kbd "C-c C-k") #'zhihu-reader-cancel-compose))
    (pop-to-buffer buffer)))

(defun zhihu-reader-compose-comment ()
  "Compose a comment or reply at point."
  (interactive)
  (let ((comment-id (org-entry-get nil "ZHIHU-COMMENT"))
        (answer-id (org-entry-get nil "ZHIHU-ANSWER" t))
        (article-id (org-entry-get nil "ZHIHU-ARTICLE" t)))
    (cond
     (comment-id
      (let ((root-kind (if article-id "article" "answer"))
            (root-id (or answer-id article-id)))
        (unless root-id
          (user-error "缺少评论所属回答/文章 ID"))
        (zhihu-reader--open-compose-buffer
         'reply comment-id (format "回复 %s" comment-id)
         root-kind root-id)))
     (answer-id
      (zhihu-reader--open-compose-buffer
       'answer answer-id (format "回答 %s" answer-id)
       "answer" answer-id))
     (article-id
      (zhihu-reader--open-compose-buffer
       'article article-id (format "文章 %s" article-id)
       "article" article-id))
     (t
      (user-error "光标不在回答、文章或评论标题上")))))

(defun zhihu-reader-cancel-compose ()
  "Close the comment compose buffer."
  (interactive)
  (when (derived-mode-p 'text-mode)
    (kill-buffer)))

(defun zhihu-reader-publish-comment ()
  "Publish the current comment compose buffer."
  (interactive)
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (unless (derived-mode-p 'text-mode)
    (user-error "当前 buffer 不是评论编辑 buffer"))
  (let ((kind zhihu-reader-compose-kind)
        (id zhihu-reader-compose-id)
        (root-kind zhihu-reader-compose-root-kind)
        (root-id zhihu-reader-compose-root-id)
        (text (string-trim (buffer-string))))
    (when (string-empty-p text)
      (user-error "评论不能为空"))
    (unless (yes-or-no-p "确认发布这条评论？")
      (user-error "已取消"))
    (zhihu-reader--publish-comment-native
     root-kind root-id (if (eq kind 'reply) id nil) text)
    (message "评论已发布")
    (kill-buffer)))

(provide 'zhihu-reader-comments)
;;; zhihu-reader-comments.el ends here
