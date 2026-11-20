;;; zhihu-reader-test.el --- Unit tests for emacs-zhihu-reader  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'zhihu-reader)

(ert-deftest zhihu-reader-seen-key-answer-is-per-answer ()
  (should (equal (zhihu-reader--item-seen-key
                  '(:kind "answer" :id "11" :question-id "99"))
                 "answer:11"))
  (should (equal (zhihu-reader--item-seen-key
                  '(:kind "answer" :id "22" :question-id "99"))
                 "answer:22")))

(ert-deftest zhihu-reader-seen-key-article ()
  (should (equal (zhihu-reader--item-seen-key
                  '(:kind "article" :id "42"))
                 "article:42")))

(ert-deftest zhihu-reader-search-url-includes-filters ()
  (let ((zhihu-reader-search-content-type "article")
        (zhihu-reader-search-time-range "month")
        (zhihu-reader-search-sort "upvoted_count"))
    (let ((url (zhihu-reader--search-url "emacs" 2)))
      (should (string-match-p "offset=20" url))
      (should (string-match-p "vertical=article" url))
      (should (string-match-p "time_interval=a_month" url))
      (should (string-match-p "search_source=Filter" url))
      (should (string-match-p "sort=upvoted_count" url)))))

(ert-deftest zhihu-reader-image-url-detection ()
  (should (zhihu-reader--image-url-p
           "https://picx.zhimg.com/v2-abc_r.jpg?source=x"))
  (should (zhihu-reader--image-url-p
           "https://example.com/image.webp"))
  (should-not (zhihu-reader--image-url-p
               "https://example.com/article/1")))

(ert-deftest zhihu-reader-comment-image-only-is-kept ()
  (let* ((comment (list (cons 'id "c1")
                        (cons 'content "<p><img src=\"https://picx.zhimg.com/a.jpg\"></p>")))
         (json (list (cons 'data (vector comment))))
         (comments (zhihu-reader--normalize-comments json)))
    (should (= (length comments) 1))
    (should (equal (plist-get (car comments) :images)
                   '("https://picx.zhimg.com/a.jpg")))))

(ert-deftest zhihu-reader-zhihu-url-detection ()
  (should (zhihu-reader--zhihu-url-p
           "https://www.zhihu.com/question/1"))
  (should (zhihu-reader--zhihu-url-p
           "https://zhuanlan.zhihu.com/p/1"))
  (should-not (zhihu-reader--zhihu-url-p
               "https://example.com/zhihu")))

(provide 'zhihu-reader-test)
;;; zhihu-reader-test.el ends here
