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

;;  Precompile

(defun locate-precompiled-assets ()
  (find-assets-from-specs *precompiled-assets*))

(defun precompile ()
  (msg "Precompile")
  (with-msg-indent (1)
    (force-output)
    (dolist (asset (locate-precompiled-assets))
      (let ((output-path (asset-path asset)))
        (msg "~A" output-path)
        (let ((pathname (pathname output-path)))
          (with-msg-indent (1)
            (compile-asset asset pathname))
          (setq pathname (digest-asset asset pathname))
          (msg "~A" pathname)
          (when (setq pathname (gzip-asset asset pathname))
            (msg "~A" pathname)))))))
