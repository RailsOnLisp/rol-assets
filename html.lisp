;;
;;  LowH Triangle Assets  -  Asset pipeline
;;
;;  Copyright 2012 Thomas de Grivel <billitch@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :lowh.triangle.assets)

(defstruct (entity-table (:constructor make-entity-table (chars entities)))
  chars
  entities)

(defvar *html-entities*
  (make-entity-table "&<>\"'"
		     (the (simple-array simple-base-string)
		       #("&amp;" "&lt;" "&gt;" "&quot;" "&apos;"))))

(defun char-entity (char &optional (table *html-entities*))
  (with-slots (chars entities) table
    (let ((i (position char chars)))
      (when i
	(svref entities i)))))

(defun quote-html (string &key stream (start 0) (end (length string)))
  (declare (optimize (safety 0) (debug 0) (speed 3))
	   (type fixnum start end)
	   (type string string))
  (unless (emptyp string)
    (labels ((print-raw (raw i)
	       (declare (type fixnum raw i))
	       (when (< raw i)
		 (write-string string stream :start raw :end i)))
	     (chr (i)
	       (typecase string
		 (simple-array  (char string i))
		 (simple-string (char string i))
		 (string        (char string i))))
	     (skip (raw i)
	       (declare (type fixnum raw i))
	       (cond ((= i end) (print-raw raw i))
		     (t (let* ((char (chr i))
			       (entity (char-entity char)))
			  (cond (entity (print-raw raw i)
					(write-string entity stream)
					(incf i)
					(skip i i))
				(t (skip raw (1+ i))))))))
	     (stream-or-string (i)
	       (declare (type fixnum i))
	       (cond ((= i end) (if (and (= 0 start)
					 (= end (length string)))
				    string
				    (subseq string start end)))
		     (t (let* ((char (chr i))
			       (entity (char-entity char)))
			  (cond (entity (with-output-to-string (out)
					  (setq stream out)
					  (print-raw start i)
					  (write-string entity stream)
					  (incf i)
					  (skip i i)))
				(t (stream-or-string (1+ i)))))))))
      (if stream
	  (skip start start)
	  (stream-or-string start)))))

(define-compiler-macro quote-html (&whole whole string &key stream
					  (start 0)
					  (end nil end-p))
  (if (and (null stream)
	   (stringp string)
	   (or (not end-p)
	       (integerp end)))
      (quote-html string
		  :start start
		  :end (if end-p end (length string)))
      whole))
