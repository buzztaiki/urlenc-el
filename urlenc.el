;;; urlenc.el --- URL encode/decode utility

;; Copyright (C) 2012  Taiki SUGAWARA

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: url

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'url-util) 

(defcustom urlenc:default-coding-system 'utf-8
  "Default encode/decode coding-system for `urlenc'."
  :group 'urlenc
  :type 'coding-system)

(defun urlenc:decode-string (url &optional cs)
  "Decode URL with coding system CS."
  (decode-coding-string
   (url-unhex-string (string-to-unibyte url))
   (or cs urlenc:default-coding-system)))

(defun urlenc:encode-string (url &optional cs)
  "Encode URL with coding system CS."
  (url-hexify-string
   (encode-coding-string url (or cs urlenc:default-coding-system))))

(defun urlenc:replace-region (start end func cs)
  (let ((url (buffer-substring start end))
	(marker (point-marker)))
    (goto-char start)
    (delete-region start end)
    (insert (funcall func url cs))
    (goto-char marker)))

(defun urlenc:read-cs ()
  (let ((def urlenc:default-coding-system))
    (read-coding-system (format "coding-system(default: %s): " def) def)))

(defun urlenc:insert-read ()
  (list (read-string "url: ")
	(urlenc:read-cs)))

(defun urlenc:region-read ()
  (list (region-beginning) (region-end)
	(urlenc:read-cs)))

(defun urlenc:decode-region (start end &optional cs)
  "Decode region between START and CS as url with coding system CS."
  (interactive (urlenc:region-read))
  (urlenc:replace-region start end 'urlenc:decode-string cs))

(defun urlenc:encode-region (start end &optional cs)
  "Encode region between START and CS as url with coding system CS."
  (interactive (urlenc:region-read))
  (urlenc:replace-region start end 'urlenc:encode-string cs))

(defun urlenc:decode-insert (url &optional cs)
  "Insert decoded URL into current position with coding system CS."
  (interactive (urlenc:insert-read))
  (insert (urlenc:decode-string url cs)))

(defun urlenc:encode-insert (url &optional cs)
  "Insert encoded URL into current position with coding system CS."
  (interactive (urlenc:insert-read))
    (insert (urlenc:encode-string url cs)))

(provide 'urlenc)
;;; urlenc.el ends here
