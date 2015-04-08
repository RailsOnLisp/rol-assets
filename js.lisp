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

;;    JS

(defclass js-asset (preprocessed-asset) ())

(defmethod asset-ext ((asset js-asset))
  '.js)

(defmethod asset-class-extensions ((class (eql 'js-asset)))
  '(.js))

(defmethod asset-include ((output stream)
			  (context (eql :html))
			  (asset js-asset)
			  &key &allow-other-keys)
  (write-string "<script src=\"" output)
  (write-string (quote-html (asset-url asset)) output)
  (write-string "\" type=\"text/javascript\"></script>
" output)
  (values))

;;  Compile

(defun jsmin (in out)
  (let ((err (make-string-output-stream)))
    (unwind-protect
	 (sb-ext:run-program "jsmin" '()
			     :input in
			     :output out
			     :error err
			     :search t)
      (close err))))

(defmethod process-asset ((asset js-asset)
			  (output stream))
  (with-input-from-file/utf-8 (js (asset-source-path asset))
    (copy-stream js output))
  (force-output output)
  (values))
