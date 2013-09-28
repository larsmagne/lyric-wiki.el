;;; lyric-wiki.el --- Fetch and display lyrics from Lyric Wiki
;; Copyright (C) 2013 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; This file is not part of GNU Emacs.

;; lyric-wiki.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; lyric-wiki.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'mm-url)
(require 'dom)
(require 'eww)

(defun lyric-wiki-fetch-lyrics (artist track)
  (url-retrieve
   (format
    "http://lyrics.wikia.com/api.php?func=getSong&artist=%s&song=%s&fmt=xml"
    artist track)
   (lambda (&rest args)
     (when (search-forward "\n\n" nil t)
       (let* ((data (libxml-parse-xml-region (point) (point-max)))
	      (url (nth 2 (assq 'url data))))
	 (url-retrieve url 'lyric-wiki-scrape-html))))))

(defun lyric-wiki-scrape-html (&rest args)
  (when (search-forward "\n\n" nil t)
    (let* ((dom (shr-transform-dom
		 (libxml-parse-html-region (point) (point-max))))
	   (box (car (dom-by-class dom "lyricbox"))))
      (pop-to-buffer "*lyrics*")
      (erase-buffer)
      (shr-descend (cons 'div (loop for elem in (cdr box)
				    when (memq (car elem) '(text br))
				    collect elem)))
      (goto-char (point-min)))))
      
(provide 'lyric-wiki)

;; lyric-wiki.el ends here
