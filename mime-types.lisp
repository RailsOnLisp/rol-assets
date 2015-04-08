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

(in-package :RoL-assets)

(defgeneric mime-type (object))

(defvar *mime-type/extension*
  (make-hash-table :test 'eq)
  "Hash table mapping extension symbols to mime type keywords.")

(defmethod mime-type ((ext symbol))
  (if (extension-p ext)
      (gethash ext *mime-type/extension*)
      (call-next-method)))

(defmethod mime-type ((pathname pathname))
  (mime-type (intern-extension (pathname-type pathname))))

(defmethod mime-type ((file file-stream))
  (mime-type (pathname file)))

(defmethod (setf mime-type) (value (ext symbol))
  (if (extension-p ext)
      (setf (gethash ext *mime-type/extension*) value)
      (call-next-method)))

;;  Parser for mime.types file

(defun read-mime.types (input)
  (regex-lines "^\\s*([a-zA-Z0-9_.+-]+/[-a-zA-Z0-9_.+]+)\\s+([\\sa-zA-Z0-9]+)"
	       input
	       :match (lambda (match type extensions)
			(declare (ignorable match))
			(setq type (intern (string-upcase type) :keyword))
			(dolist (ext (cl-ppcre:split "\\s+" extensions))
			  (unless (emptyp ext)
			    (setf (mime-type (intern-extension ext)) type))))))

(find-if (lambda (path)
	   (when (probe-file path)
	     (read-mime.types path)
	     t))
	 '(#P"mime.types"
	   #P"conf/mime.types"
	   #P"/etc/nginx/mime.types"
	   #P"/var/www/conf/mime.types"
	   #P"/etc/apache/mime.types"))
