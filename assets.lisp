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

;;  Misc

(defun empty-p (string)
  (or (null string)
      (not (cl-ppcre:scan "\\S" string))))

(eval-when (:compile-toplevel :load-toplevel)
  (let ((cache-nil (gensym "CACHE-NIL-")))

    (defmacro cache-1 ((test key) &body body)
      "Cache one value of BODY. TEST identifies KEY is cached."
      (let ((cache (gensym "CACHE-")))
	`(let ((,cache (load-time-value (cons ',cache-nil nil))))
	   (if (,test (car ,cache) ,key)
	       (cdr ,cache)
	       (setf (car ,cache) ,key
		     (cdr ,cache) (progn ,@body))))))))

;;  Config

(defvar *debug* nil)

(defvar *default-assets-dirs*
  '("lib/*/triangle/assets/*/"
    "lib/triangle/*/triangle/assets/*/"
    "app/assets/*/"
    "assets/*/"))

(defparameter *assets-dirs*
  *default-assets-dirs*)

(defvar *default-precompiled-assets*
  '("app.css"
    "app.js"
    "**/*.jpeg"
    "**/*.jpg"
    "**/*.png"
    "**/*.svg"
    "**/*.eot"
    "**/*.ttf"
    "**/*.woff"))

(defparameter *precompiled-assets*
  *default-precompiled-assets*)

(defvar *asset-url-prefix* "/assets/")

(defvar *asset-path-prefix* "public/assets/")

;;  Config stanzas

(defun assets-dir (pathspec)
  (let* ((namestring (enough-namestring pathspec))
	 (path (if (char= #\/ (last-elt namestring))
		   namestring
		   (str namestring "/"))))
    (pushnew path *assets-dirs* :test #'string=)))

(defun precompiled-asset (asset-name)
  (pushnew asset-name *precompiled-assets* :test #'string=))

;;  Read config

(defun assets-dirs ()
  (cache-1 (eq *assets-dirs*)
    (directories (reverse *assets-dirs*))))

;;  Asset class

(defclass asset ()
  ((name :initarg :name
	 :reader asset-name
	 :type string)
   (source-dir :initarg :source-dir
	       :accessor asset-source-dir
	       :type string)
   (source-ext :initarg :source-ext
	       :reader asset-source-ext
	       :type keyword)))

(defgeneric asset-ext (asset))

(defmethod asset-ext ((asset asset))
  (asset-source-ext asset))

(defun asset-url (asset)
  (declare (type asset asset))
  (let ((name (asset-name asset))
	(ext (asset-ext asset)))
    (str *asset-url-prefix* name (when ext ".") ext)))

(defun asset-path (asset)
  (declare (type asset asset))
  (let ((name (asset-name asset))
	(ext (asset-ext asset)))
    (str *asset-path-prefix* name (when ext ".") ext)))

(defun asset-source-path (asset)
  (declare (type asset asset))
  (with-slots (name source-dir source-ext) asset
    (str source-dir name (when source-ext ".") source-ext)))

(defun asset-debug-path (asset)
  (declare (type asset asset))
  (with-slots (name source-dir source-ext) asset
    (str source-dir name ".debug" (when source-ext ".") source-ext)))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type t)
    (ignore-errors (format stream "~S" (asset-path asset)))
    (ignore-errors (format stream " ~S" (asset-source-path asset)))))

;;  Asset class -> extensions

#+nil
(fmakunbound 'asset-class-extensions)

(defgeneric asset-class-extensions (asset-class))

(defmethod asset-class-extensions ((any symbol))
  nil)

(defmethod asset-class-extensions ((class class))
  (asset-class-extensions (class-name class)))

(defmethod asset-class-extensions ((asset asset))
  (asset-class-extensions (class-of asset)))

;;  Asset classes

;;    Image

(defclass image-asset (asset) ())

(defmethod asset-class-extensions ((class (eql 'image-asset)))
  (extensions #:gif #:ico #:jpeg #:jpg #:png #:svg #:svgz))

;;    Font

(defclass font-asset (asset) ())

(defmethod asset-class-extensions ((class (eql 'font-asset)))
  (extensions #:eot #:ttf #:woff))

;;    Preprocessed assets

(defclass preprocessed-asset (asset) ())

;;    CSS

(defclass css-asset (preprocessed-asset) ())

(defmethod asset-ext ((asset css-asset))
  (extension #:css))

(defmethod asset-class-extensions ((class (eql 'css-asset)))
  (extensions #:css #:less))

;;    JS

(defclass js-asset (preprocessed-asset) ())

(defmethod asset-ext ((asset js-asset))
  (extension #:js))

(defmethod asset-class-extensions ((class (eql 'js-asset)))
  (extensions #:js))
