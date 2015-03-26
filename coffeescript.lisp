;;
;;  LowH Triangle Assets  -  Asset pipeline
;;
;;  Copyright 2012,2014 Thomas de Grivel <billitch@gmail.com>
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

;;  Coffeescript -> JS

(defclass coffeescript-asset (js-asset) ())

(defmethod asset-class-extensions ((class (eql 'coffeescript-asset)))
  '(.coffee))

;;  Compile

(defun coffeescript (in out)
  (let ((err (make-string-output-stream)))
    (unwind-protect
	 (sb-ext:run-program "coffee -cs" '()
			     :input in
			     :output out
			     :error err
			     :search t)
      (close err))))

(defmethod process-asset ((asset coffeescript-asset)
			  (output stream))
  (with-input-from-file/utf-8 (in (asset-source-path asset))
    (coffeescript in output))
  (force-output output)
  (values))
