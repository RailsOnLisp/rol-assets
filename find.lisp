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

;;  Finding assets

;;  FIXME: This could be improved by listing all files in assets
;;  FIXME: directories to work the paths in memory.

(defun find-in-assets (type dir name ext assets)
  (find-if (lambda (asset)
	     (declare (type asset asset))
	     (and (typep asset type)
		  (string= name (asset-name asset))
		  (or (not (string= dir (asset-source-dir asset)))
		      (string= ext (asset-source-ext asset)))))
	   assets))

#+nil
(fmakunbound 'find-assets)

(defgeneric find-assets (type dir name ext assets))

;;  Resolve name, possibly wild

(defmethod find-assets ((type class)
			(dir string)
			(name string)
			(ext symbol)
			assets)
  (let ((absolute-dir (truename dir))
	(assets assets))
    (dolist (path (reverse (directory (str dir name ext))))
      (unless (char= #\. (char (pathname-name path) 0))
	(let* ((name.ext (enough-namestring (truename path) absolute-dir))
	       (name (if ext
			 (subseq name.ext 0 (- (length name.ext)
					       (length (string ext))))
			 name.ext)))
	  (unless (find-in-assets type dir name ext assets)
	    (push (make-instance type
				 :name name
				 :source-dir dir
				 :source-ext ext)
		  assets)))))
    assets))

;;    Loop through extensions

(defmethod find-assets ((type class)
			(dir string)
			(name string)
			(extensions cons)
			assets)
  (reduce (lambda (assets ext)
	    (declare (type symbol ext))
	    (find-assets type dir name ext assets))
	  extensions
	  :initial-value assets))

(defmethod find-assets ((type class)
			(dir string)
			(name string)
			(ext null)
			assets)
  (when (setq ext (asset-class-extensions type))
    (find-assets type dir name ext assets)))

;;    Loop through dirs

(defmethod find-assets ((type class)
			(directories cons)
			(name string)
			ext
			assets)
  (reduce (lambda (assets dir)
	    (declare (type string dir))
	    (find-assets type dir name ext assets))
	  directories
	  :initial-value assets))

(defmethod find-assets (type (dir null) name ext assets)
  (let ((dir (assets-dirs)))
    (when dir
      (find-assets type dir name ext assets))))

;;    Resolve class

(defmethod find-assets ((type symbol) dir name ext assets)
  (let ((class (if (keywordp type)
		   (find-class (find-symbol (str type "-ASSET")))
		   (find-class type))))
    (unless class
      (error "Unknown asset type : ~S" type))
    (find-assets (the class class) dir name ext assets)))

;;  Asset spec

(defmacro with-asset-spec (spec (name ext) &body body)
  `(let (,name ,ext)
     (cl-ppcre:register-groups-bind (n e)
	 ("^\\s*(.*?)(?:\\.([^./]+))?\\s*$" ,spec)
       (setf ,name n ,ext (when e (intern-extension e))))
     (let ((,name ,name) (,ext ,ext))
       ,@body)))

(defun find-assets-from-spec (spec &optional class assets)
  (labels ((assets-matching (class name ext)
	     (if class
		 (let ((new (find-assets class nil name
					 (asset-class-extensions class)
					 assets)))
		   (unless (eq new assets)
		     new))
		 (when ext
		   (some (lambda (class) (assets-matching class name ext))
			 (extension-asset-classes ext))))))
    (or (assets-matching class spec nil)
	(with-asset-spec spec (name ext)
	  (when (if class
		    (find ext (asset-class-extensions class))
		    ext)
	    (assets-matching class name ext)))
	assets)))

(defun find-assets-from-specs (specs &optional class assets)
  (reduce (lambda (assets spec)
	    (find-assets-from-spec spec class assets))
	  specs
	  :initial-value assets))

;;  Asset cache

(defvar *asset-cache* (make-hash-table :test 'equal))

(defun clear-asset-cache ()
  (clrhash *asset-cache*))

(defmacro asset-cache-get (key)
  `(gethash ,key *asset-cache*))

(defun find-asset (spec &optional class)
  (or #1=(asset-cache-get `(find-asset ,spec ,class))
      (setf #1# (let ((ext (when-let ((type (pathname-type spec)))
			     (intern-extension type)))
		      (assets (find-assets-from-spec spec class)))
		  (or (find ext assets :key #'asset-source-ext :test #'eq)
		      (first assets))))))

;;  Now that we can find an asset from a string spec we can also
;;  add some sugar coating to asset methods.

(defmethod asset-ext ((spec string))
  (asset-ext (find-asset spec)))

(defmethod asset-url ((spec string))
  (asset-url (find-asset spec)))

(defmethod asset-path ((spec string))
  (asset-path (find-asset spec)))

(defmethod asset-write-date ((spec string))
  (asset-write-date (find-asset spec)))

(defmethod asset-source-path ((spec string))
  (asset-source-path (find-asset spec)))

(defmethod asset-sources ((spec string))
  (asset-sources (find-asset spec)))

(defmethod asset-include (output
			  context
			  (spec string)
			  &rest args &key &allow-other-keys)
  (apply #'asset-include output context (find-asset spec) args))

(defmethod compile-asset ((spec string) output)
  (compile-asset (find-asset spec) output))
