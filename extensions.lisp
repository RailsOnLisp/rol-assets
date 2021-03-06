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

;;  Extensions package

(defun intern-extension (name)
  (let ((sym (intern (string-upcase (if (char= #\. (char name 0))
                                        name
                                        (concatenate 'string "." name)))
                     :RoL-extensions)))
    (export sym :RoL-extensions)
    sym))

(defun extension-p (thing)
  (and (symbolp thing)
       (eq #.(find-package :RoL-extensions)
           (symbol-package thing))))

(deftype extension ()
  `(and symbol (satisfies extension-p)))
