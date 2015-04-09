;;
;;  RoL-assets  -  Asset pipeline
;;
;;  Copyright 2012-2015 Thomas de Grivel <thomas@lowh.net>
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

;;  Compile

(defmethod json:encode-json ((object pathname) &optional stream)
  (json:encode-json (namestring object) stream))

(defun less (src-path parser-options css-options &optional out)
  (let ((opt (json:make-object `((src . ,src-path)
				 (parser . ,(plist-alist parser-options))
				 (css . ,(plist-alist css-options)))
			       nil)))
    (with-input-from-string (in (json:encode-json-to-string opt))
      (exec-js:from-file #P"lib/rol/assets/less.js"
			 :safely nil :in in :out out))))

;;  LESS

(defclass less-asset (css-asset)
  ((less-imports :type list)))

(defgeneric less-imports% (asset))
(defgeneric less-imports (asset))
(defgeneric less-sources (asset))

(defmethod asset-class-extensions ((class (eql 'less-asset)))
  '(.less))

(defmethod less-imports% ((asset less-asset))
  (let* ((head (cons nil nil))
	 (tail head))
    (regex-lines "(?:^|\\b)@import\\s+\"([^\"]+)\"\\s*;"
		 (pathname (asset-source-path asset))
		 :match (lambda (whole i)
			  (declare (ignore whole))
			  (when-let ((a (find-asset i 'less-asset)))
			    (unless (find (asset-path a) (cdr head)
					  :test #'string=
					  :key #'asset-path)
			      (setf tail (setf (cdr tail)
					       (cons a nil)))))))
    (cdr head)))

(defmethod less-imports ((asset less-asset))
  (let* ((source-path (pathname (asset-source-path asset)))
	 (write-date (file-write-date source-path))
	 (imports (when (slot-boundp asset 'less-imports)
		    (slot-value asset 'less-imports))))
    (if (and imports (= write-date (car imports)))
	(cdr imports)
	(cdr (setf (slot-value asset 'less-imports)
		   (cons write-date (less-imports% asset)))))))

(defmethod less-sources ((asset less-asset))
  (let* ((head (cons nil nil))
	 (tail head))
    (labels ((iter (a)
	       (unless (find (asset-path a) (cdr head)
			     :test #'string= :key #'asset-path)
		 (setf tail (setf (cdr tail) (cons a nil)))
		 (mapc #'iter (less-imports a)))))
      (iter asset))
    (cdr head)))

#+nil
(defun less-sources (asset)
  (if (slot-boundp asset 'less-sources)
      #1=(slot-value asset 'less-sources)
      (setf #1#
	    (let* ((head (cons asset nil))
		   (tail head))
	      (labels ((collect (item)
			 (setf tail (setf (cdr tail) (cons item nil))))
		       (have (item)
			 (find item head :test #'string= :key #'asset-path))
		       (match (whole i)
			 (declare (ignore whole))
			 (unless (have i)
			   (when-let ((a (find-asset i 'less-asset)))
			     (collect a)
			     (parse a))))
		       (parse (a)
			 (regex-lines "(?:^|\\b)@import\\s+\"([^\"]+)\"\\s*;"
				      (pathname (asset-source-path a))
				      :match #'match)))
		(parse asset)
		head)))))

(defmethod asset-write-date ((asset less-asset))
  (loop for a in (less-sources asset)
     maximize (file-write-date (asset-source-path a))))

(defmethod process-asset ((asset less-asset)
			  (output stream))
  (let ((true-assets-dirs (cache-1 (eq *assets-dirs*)
			    (mapcar #'truename (assets-dirs))))
	(path (truename (asset-source-path asset))))
    (less path
	  (list :paths true-assets-dirs	:filename path)
	  (list :yuicompress (not (debug-p (or :css :less :assets))))
	  output))
  (values))
