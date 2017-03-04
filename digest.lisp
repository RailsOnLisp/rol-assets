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

(defvar *digest* :sha1)

(defun digest-file (path)
  (let ((n (ironclad:octets-to-integer (ironclad:digest-file *digest* path))))
    (cl-base64:integer-to-base64-string n :uri t)))

(defmethod digest-asset ((asset asset) (path pathname))
  (setf (asset-digest asset) (digest-file path))
  (let ((digest-path (asset-path asset)))
    (when (probe-file digest-path)
      (sb-posix:unlink digest-path))
    (sb-posix:link path digest-path)
    #+nil(sb-posix:unlink path)
    #+nil(sb-posix:symlink (make-pathname :name (pathname-name digest-path)
                                     :type (pathname-type digest-path))
                      path)
    digest-path))
