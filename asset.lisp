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

;;  FIXME: bug on renamed or deleted asset file still in cache

(in-package :RoL-assets)

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
	       :type extension)
   (sources :type list)
   (path :type string)
   (source-path :type string)
   (url :type string)
   (digest :type string
           :initform nil
           :accessor asset-digest)))

(defgeneric asset-ext (asset))
(defgeneric asset-url (asset))
(defgeneric asset-path (asset))
(defgeneric asset-source-path (asset))
(defgeneric asset-include (output context asset &key &allow-other-keys))

(defgeneric asset-sources (asset))
(defgeneric asset-sources% (asset))
(defgeneric asset-write-date (asset))
(defgeneric compile-asset (asset output))
(defgeneric digest-asset (asset path))

;;  Base implementation

(defmethod asset-ext ((asset asset))
  (asset-source-ext asset))

(defmethod asset-digest ((asset asset))
  (or (slot-value asset 'digest)
      (let ((asset-path (expand-uri nil *assets-path-template*
                                    :name (asset-name asset)
                                    :ext (subseq (string-downcase (asset-ext asset))
                                                 1))))
        (when-let ((link (readlink asset-path)))
          (cl-ppcre:register-groups-bind (name digest ext)
              ("([^/]+)[.]([^./]+)([.][^./]+)$" link)
            (when (and (string= name (asset-name asset))
                       (string-equal ext (asset-ext asset)))
              (setf (asset-digest asset) digest)
              (slot-makunbound asset 'url)
              (slot-makunbound asset 'path)
              digest))))))

(defmethod mime-type ((asset asset))
  (mime-type (asset-ext asset)))

(defmethod asset-url ((asset asset))
  (expand-uri nil *assets-url-template*
              :name (asset-name asset)
              :digest (asset-digest asset)
              :ext (subseq (string-downcase (asset-ext asset))
                           1)))

(defmethod asset-path ((asset asset))
  (expand-uri nil *assets-path-template*
              :name (asset-name asset)
              :digest (asset-digest asset)
              :ext (subseq (string-downcase (asset-ext asset)) 1)))

(defmethod asset-source-path ((asset asset))
  (if (slot-boundp asset 'source-path)
      #1=(slot-value asset 'source-path)
      (setf #1# (with-slots (name source-dir source-ext) asset
		  (str source-dir name source-ext)))))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type t)
    (ignore-errors (format stream "~S" (asset-path asset)))
    (ignore-errors (format stream " ~S" (asset-source-path asset)))))

(defmethod asset-write-date ((assets cons))
  (loop
     for a in assets
     for path = (asset-source-path a)
     unless (probe-file path) do (return -1)
     maximize (file-write-date path)))

(defmethod asset-sources% ((asset asset))
  (list asset))

(defmethod asset-sources ((asset asset))
  (flet ((miss ()
	   (let ((sources (asset-sources% asset)))
	     (setf (slot-value asset 'sources)
		   (cons (asset-write-date sources) sources))
	     sources)))
    (if (slot-boundp asset 'sources)
	(destructuring-bind (cached-date &rest cached-sources)
	    (slot-value asset 'sources)
	  (if (= cached-date (asset-write-date cached-sources))
	      cached-sources
	      (miss)))
	(miss))))

(defmethod asset-write-date ((asset asset))
  (asset-write-date (asset-sources asset)))

(defmethod compile-asset ((asset asset) (output stream))
  (let ((path (asset-source-path asset)))
    ;;(msg "CP ~A" path)
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (copy-stream in output)))
  nil)

(defmethod compile-asset ((asset asset) (output pathname))
  (ensure-directories-exist output)
  (let ((path (asset-source-path asset)))
    ;;(msg "CP ~A" path)
    (copy-files path output :replace t :update t))
  nil)

(defmethod asset-include ((output null)
			  context
			  asset
			  &rest args &key &allow-other-keys)
  (with-output-to-string (stream)
    (apply #'asset-include stream context asset args)))

;;  Asset class -> extensions

(defgeneric asset-class-extensions (asset-class))

(defmethod asset-class-extensions ((any symbol))
  nil)

(defmethod asset-class-extensions ((class class))
  (asset-class-extensions (class-name class)))

(defmethod asset-class-extensions ((asset asset))
  (asset-class-extensions (class-of asset)))

;;  Extension -> asset classes

(defun extension-asset-classes (extension
				&optional (class (find-class 'asset)))
  (declare (type extension extension)
	   (type class class))
  (when extension
    (labels ((add (classes a)
	       (reduce #'add (closer-mop:class-direct-subclasses a)
		       :initial-value (cons a classes)))
	     (matching-class (a)
	       (if (find extension (asset-class-extensions a))
		   (add nil a)
		   (some #'matching-class
			 (closer-mop:class-direct-subclasses a)))))
      (or (matching-class class)
	  `(,class)))))
