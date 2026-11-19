;;; zhihu-reader.el --- Read Zhihu inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 qukuqhd
;; Author: qukuqhd
;; Version: 0.4.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: hypermedia, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A Zhihu reader/writer for Emacs.  The implementation is split into
;; focused modules:
;;
;;   core      settings, cookies, HTTP, normalization
;;   api       ZSE signing, write actions and publishing
;;   ui        Org rendering, images, mode and key map
;;   comments  comment trees, paging and composition
;;   edit      editing, drafts and article publishing
;;   views     feed, hot, search, question/answer and navigation
;;
;; Login is external: sign in with Zen/Firefox, then the reader picks up
;; the cookies.  Only emacs-zhihu-reader is required; no zhihu.el,
;; ox-zhihu or plz package is loaded.

;;; Code:

(require 'zhihu-reader-core)
(require 'zhihu-reader-api)
(require 'zhihu-reader-ui)
(require 'zhihu-reader-comments)
(require 'zhihu-reader-edit)
(require 'zhihu-reader-views)

(provide 'zhihu-reader)
;;; zhihu-reader.el ends here
