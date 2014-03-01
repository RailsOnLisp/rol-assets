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

;;  Compile

(defmethod json:encode-json ((object pathname) &optional stream)
  (json:encode-json (namestring object) stream))

(defun less (src-path parser-options css-options &optional out)
  (let* ((fmt "~
var path = require('path'),
    fs = require('fs'),
    sys = require('util'),
    os = require('os');
var less = require('less');
var src = ~A;
var parser_opts = ~A;
var css_opts = ~A;

var print_error = function (e) {
  less.writeError(e);
  process.exit(2);
}
var print_tree = function (e, tree) {
  if (e)
    print_error(e);
  try {
    var css = tree.toCSS(css_opts);
    process.stdout.write(css);
  } catch (e) {
    print_error(e);
  }
}
var parse_data = function (e, data) {
  if (e)
    print_error(e);
  try {
    new(less.Parser)(parser_opts).parse(data, print_tree)
  } catch (e) {
    print_error(e);
  }
}
try {
  fs.readFile(path.resolve(process.cwd(), src), 'utf8', parse_data);
} catch (e) {
  print_error(e);
}
")
	 (js (format nil fmt
		     (json:encode-json-to-string src-path)
		     (json:encode-json-plist-to-string parser-options)
		     (json:encode-json-plist-to-string css-options))))
    #+nil(format *error-output* "~%~A~%" js)
    (exec-js:from-string js :safely nil :out out)))

;;  LESS

(defclass less-asset (css-asset)
  ((less-imports :type list)))

(defmethod asset-class-extensions ((class (eql 'less-asset)))
  '(.less))

(defun less-imports% (asset)
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

(defgeneric less-imports (asset))

(defmethod less-imports ((asset less-asset))
  (let* ((source-path (pathname (asset-source-path asset)))
	 (write-date (file-write-date source-path))
	 (imports (when (slot-boundp asset 'less-imports)
		    (slot-value asset 'less-imports))))
    (if (and imports (= write-date (car imports)))
	(cdr imports)
	(cdr (setf (slot-value asset 'less-imports)
		   (cons write-date (less-imports% asset)))))))

(defgeneric less-sources (asset))

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
	  (list :yuicompress (not *debug*))
	  output))
  (values))
