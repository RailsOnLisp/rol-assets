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
(defgeneric asset-url (asset))
(defgeneric asset-path (asset))
(defgeneric asset-source-path (asset))
(defgeneric asset-include (output context asset &key &allow-other-keys))
(defgeneric compile-asset (asset output))

;;  Base implementation

(defmethod asset-ext ((asset asset))
  (asset-source-ext asset))

(defmethod asset-url ((asset asset))
  (let ((name (asset-name asset))
	(ext (asset-ext asset)))
    (str *asset-url-prefix* name (when ext ".") ext)))

(defmethod asset-path ((asset asset))
  (let ((name (asset-name asset))
	(ext (asset-ext asset)))
    (str *asset-path-prefix* name (when ext ".") ext)))

(defmethod asset-source-path ((asset asset))
  (with-slots (name source-dir source-ext) asset
    (str source-dir name (when source-ext ".") source-ext)))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type t)
    (ignore-errors (format stream "~S" (asset-path asset)))
    (ignore-errors (format stream " ~S" (asset-source-path asset)))))

(defmethod compile-asset ((asset asset) (output stream))
  (let ((path (asset-source-path asset)))
    (msg "CP ~A" path)
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (copy-stream in output)))
  nil)

(defmethod compile-asset ((asset asset) (output pathname))
  (ensure-directories-exist output)
  (let ((path (asset-source-path asset)))
    (msg "CP ~A" path)
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

;;  Extension -> asset class

(defun extension-asset-class (extension
			      &optional (class (find-class 'asset)))
  (declare (type symbol extension)
	   (type class class))
  (when extension
    (labels ((matching-asset-class (c)
	       (if (find extension (asset-class-extensions c))
		   c
		   (some #'matching-asset-class
			 (closer-mop:class-direct-subclasses c)))))
      (or (matching-asset-class class)
	  class))))
