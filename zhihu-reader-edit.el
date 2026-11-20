;;; zhihu-reader-edit.el --- Editing and article workflows for Zhihu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 qukuqhd
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Part of emacs-zhihu-reader.  See zhihu-reader.el for the entry point.

;;; Code:

(require 'zhihu-reader-core)
(require 'zhihu-reader-api)
(require 'zhihu-reader-ui)
(require 'zhihu-reader-comments)

;;;; Editing

(defvar-local zhihu-reader-edit-kind nil)
(defvar-local zhihu-reader-edit-id nil)
(defvar-local zhihu-reader-edit-question-id nil)
(defvar-local zhihu-reader-edit-title nil)

(defun zhihu-reader--ensure-zhihu ()
  "Compatibility no-op; this package does not load external libraries."
  nil)

(defun zhihu-reader--subtree-body ()
  "Return the body of the current Org subtree, excluding child headings."
  (let* ((level (or (org-current-level) 1))
         (start (save-excursion
                  (org-back-to-heading t)
                  (forward-line 1)
                  (when (looking-at ":PROPERTIES:")
                    (re-search-forward "^:END:" nil t)
                    (forward-line 1))
                  (point)))
         (end (save-excursion
                (org-back-to-heading t)
                (org-end-of-subtree t)
                (when (re-search-backward
                       (format "^\\*\\{%d\\} " (1+ level)) nil t)
                  (match-beginning 0))
                (point))))
    (string-trim (buffer-substring-no-properties start end))))

(defun zhihu-reader--open-edit-buffer (kind id question-id title body)
  "Open an editable Org buffer for KIND/ID with TITLE and BODY."
  (let ((buffer (get-buffer-create "*Zhihu Edit*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (or body ""))
      (zhihu-reader-edit-mode)
      (setq-local zhihu-reader-edit-kind kind)
      (setq-local zhihu-reader-edit-id id)
      (setq-local zhihu-reader-edit-question-id question-id)
      (setq-local zhihu-reader-edit-title title)
      (setq-local header-line-format
                  (format " 编辑 %s %s  | C-c C-c 预览  C-c C-p 发布  C-c C-k 取消"
                          kind id))
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun zhihu-reader-publish-article ()
  "Publish the current Zhihu article Org source file.
This implementation is self-contained and respects
`zhihu-reader-enable-write'."
  (interactive)
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (unless buffer-file-name
    (user-error "当前 buffer 没有对应的 Org 源文件"))
  (unless (derived-mode-p 'org-mode)
    (user-error "当前 buffer 不是 Org 文件"))
  (when (buffer-modified-p)
    (save-buffer))
  (let* ((file buffer-file-name)
         (meta (zhihu-reader--article-source-meta file)))
    (unless (plist-get meta :title)
      (user-error "文章缺少 #+TITLE"))
    (unless (yes-or-no-p
             (format "确认发布文章「%s」？" (plist-get meta :title)))
      (user-error "已取消发布"))
    (let ((id (zhihu-reader--publish-article-native file)))
      (revert-buffer t t t)
      (message "文章已发布：%s" id)
      id)))

(defun zhihu-reader-new-article (title)
  "Create a new Zhihu article Org source file."
  (interactive "s文章标题: ")
  (unless (and (stringp title) (not (string-empty-p (string-trim title))))
    (user-error "标题不能为空"))
  (let* ((dir (expand-file-name zhihu-reader-article-directory))
         (slug (replace-regexp-in-string
                "[^[:alnum:]]+" "-"
                (string-trim title)))
         (file (expand-file-name
                (format "%s-%s.org"
                        (format-time-string "%Y%m%d-%H%M%S")
                        slug)
                dir))
         (topics (read-string "话题 JSON 数组（可空）: " "[]"))
         (column (read-string "专栏 ID（可空）: " "")))
    (make-directory dir t)
    (find-file file)
    (insert (format "#+TITLE: %s\n" (string-trim title)))
    (insert "#+ZHIHU_ARTICLE_ID:\n")
    (insert (format "#+ZHIHU_TOPICS: %s\n" (if (string-empty-p topics) "[]" topics)))
    (when (not (string-empty-p column))
      (insert (format "#+ZHIHU_COLUMN_ID: %s\n" column)))
    (insert "#+TOC: headlines 2\n\n* 正文\n\n")
    (save-buffer)
    (local-set-key (kbd "C-c C-p") #'zhihu-reader-publish-article)
    (message "文章源稿已创建: %s\n发布: C-c C-p 或 M-x zhihu-reader-publish-article" file)))

(defun zhihu-reader-new-answer (question-id)
  "Start a new answer draft for QUESTION-ID."
  (interactive "s知乎问题 ID 或 URL: ")
  (let ((qid (zhihu-reader--id-or-url question-id)))
    (zhihu-reader--open-edit-buffer
     "answer" nil qid "新回答" "")))

(defun zhihu-reader-insert-uploaded-image (file)
  "Upload local FILE and insert its Zhihu image link at point."
  (interactive "f图片文件: ")
  (unless (derived-mode-p 'org-mode)
    (user-error "请在 Org 文章或编辑 buffer 中使用"))
  (let ((url (zhihu-reader-upload-image file "article")))
    (insert (format "[[%s][图片]]\n" url))
    (kill-new url)
    (message "图片已上传并插入: %s" url)))

(defun zhihu-reader-save-draft ()
  "Save the current edit buffer as an Org draft."
  (interactive)
  (unless (derived-mode-p 'zhihu-reader-edit-mode)
    (user-error "当前 buffer 不是知乎编辑 buffer"))
  (let* ((dir (expand-file-name "~/.local/state/zhihu-reader/drafts/"))
         (file (expand-file-name
                (format "%s-%s.org"
                        (format-time-string "%Y%m%d-%H%M%S")
                        (or zhihu-reader-edit-id "new"))
                dir)))
    (make-directory dir t)
    (with-temp-file file
      (insert (format "#+ZHIHU_KIND: %s\n#+ZHIHU_ID: %s\n#+ZHIHU_QUESTION_ID: %s\n\n"
                      zhihu-reader-edit-kind
                      (or zhihu-reader-edit-id "")
                      (or zhihu-reader-edit-question-id "")))
      (insert (buffer-string)))
    (message "草稿已保存: %s" file)))

(defun zhihu-reader-open-draft (file)
  "Open a saved Org draft FILE."
  (interactive
   (list (read-file-name "草稿: "
                         (expand-file-name "~/.local/state/zhihu-reader/drafts/")
                         nil t nil)))
  (let (kind id qid)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+ZHIHU_KIND: \\(.*\\)$" nil t)
        (setq kind (match-string 1)))
      (when (re-search-forward "^#\\+ZHIHU_ID: \\(.*\\)$" nil t)
        (setq id (match-string 1)))
      (when (re-search-forward "^#\\+ZHIHU_QUESTION_ID: \\(.*\\)$" nil t)
        (setq qid (match-string 1))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+ZHIHU_QUESTION_ID: .*\\n\\n" nil t)
        (delete-region (point-min) (point)))
      (zhihu-reader--open-edit-buffer
       (or kind "answer") (and id (not (string-empty-p id)) id) qid
       "草稿" (buffer-string)))))

(defun zhihu-reader-edit-at-point ()
  "Edit the answer or comment at point in an Org buffer."
  (interactive)
  (let ((comment-id (org-entry-get nil "ZHIHU-COMMENT"))
        (answer-id (org-entry-get nil "ZHIHU-ANSWER"))
        (question-id (org-entry-get nil "ZHIHU-QUESTION")))
    (cond
     (comment-id
      (zhihu-reader--open-edit-buffer
       "comment" comment-id nil
       (org-get-heading t t t t)
       (zhihu-reader--subtree-body)))
     (answer-id
      (unless question-id
        (user-error "缺少问题 ID，无法编辑回答"))
      (zhihu-reader--open-edit-buffer
       "answer" answer-id question-id
       (org-get-heading t t t t)
       (zhihu-reader--subtree-body)))
     (t
      (user-error "光标不在回答或评论标题上")))))

(defun zhihu-reader-edit-preview ()
  "Render the current edit buffer as HTML in Eww."
  (interactive)
  (let ((html (zhihu-reader--org-to-html (buffer-string)))
        (file (make-temp-file "zhihu-preview-" nil ".html")))
    (with-temp-file file (insert html))
    (require 'eww)
    (eww-open-file file)))

(defun zhihu-reader-edit-publish ()
  "Preview and publish the current edit buffer.
This is destructive and requires `zhihu-reader-enable-write'."
  (interactive)
  (unless zhihu-reader-enable-write
    (user-error "写操作已关闭；请设置 `zhihu-reader-enable-write' 为 t"))
  (unless (derived-mode-p 'zhihu-reader-edit-mode)
    (user-error "当前 buffer 不是知乎编辑 buffer"))
  (let ((kind zhihu-reader-edit-kind)
        (id zhihu-reader-edit-id)
        (qid zhihu-reader-edit-question-id)
        (text (string-trim (buffer-string))))
    (unless (yes-or-no-p
             (format "确认发布这个%s？此操作会直接更新知乎线上内容。"
                     (if (equal kind "answer") "回答" "评论")))
      (user-error "已取消发布"))
    (pcase kind
      ("answer"
       (let* ((html (zhihu-reader--org-to-html text))
              (new-id (zhihu-reader--publish-answer id qid html)))
         (unless id
           (setq-local zhihu-reader-edit-id new-id))
         (message (if id "回答已更新" "新回答已发布"))))
      ("comment"
       (let* ((url (format "https://www.zhihu.com/api/v4/comment_v5/comment/%s" id))
              (resp (zhihu-reader--http-json "PATCH" url
                                             (list :content (concat "<p>" (xml-escape-string text) "</p>"))))
              (status (plist-get resp :status)))
         (unless (memq status '(200 201 202))
           (error "评论更新失败（HTTP %s）：%s"
                  status (plist-get resp :body)))
         (message "评论已更新")))
      (_ (user-error "未知编辑类型: %s" kind)))))

(defun zhihu-reader-edit-cancel ()
  "Leave the current edit buffer without publishing."
  (interactive)
  (when (derived-mode-p 'zhihu-reader-edit-mode)
    (kill-buffer)))

(defvar zhihu-reader-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'zhihu-reader-edit-preview)
    (define-key map (kbd "C-c C-p") #'zhihu-reader-edit-publish)
    (define-key map (kbd "C-c C-k") #'zhihu-reader-edit-cancel)
    (define-key map (kbd "C-c C-s") #'zhihu-reader-save-draft)
    (define-key map (kbd "C-c I") #'zhihu-reader-insert-uploaded-image)
    map)
  "Keymap for `zhihu-reader-edit-mode'.")

(define-derived-mode zhihu-reader-edit-mode org-mode "Zhihu-Edit"
  "Major mode for editing a Zhihu answer or comment before publishing."
  (setq-local buffer-read-only nil))

(provide 'zhihu-reader-edit)
;;; zhihu-reader-edit.el ends here
