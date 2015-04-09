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

;;  FIXME bug on bare .css files ?

(in-package :RoL-assets)

;;  CSS

(defclass css-asset (preprocessed-asset) ())

(defmethod asset-ext ((asset css-asset))
  '.css)

(defmethod asset-class-extensions ((class (eql 'css-asset)))
  '(.css))

(defmethod asset-include ((output stream)
			  (context (eql :html))
			  (asset css-asset)
			  &key &allow-other-keys)
  (write-string "<link rel=\"stylesheet\" href=\"" output)
  (write-string (quote-html (asset-url asset)) output)
  (write-string "\" type=\"text/css\" />
" output)
  (values))

(defmethod include-asset ((asset css-asset)
			  (output stream))
  (format output "@import url('~A');~%" (asset-url asset)))
