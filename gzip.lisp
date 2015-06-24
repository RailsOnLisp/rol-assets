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

(defvar *gzipped-extensions* '(.css .js .svg))

(defun gzip (input &optional (output (str input ".gz")))
  (with-temporary-file (tmp :element-type '(unsigned-byte 8)
                            :prefix output)
    (external-program:run "gzip" (list "-c")
                          :input input
                          :output tmp)
    (sb-posix:link tmp output)
    output))

(defgeneric gzip-asset (asset path))

(defmethod gzip-asset ((asset asset) path)
  (when (find (asset-ext asset) *gzipped-extensions* :test #'eq)
    (gzip path)))
