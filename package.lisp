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

(in-package :cl-user)

(defpackage :lowh.triangle.extensions
  (:nicknames :L>ext)
  #.(let (symbols)
      (when (find-package :L>ext)
	(do-external-symbols (s :L>ext)
	  (push s symbols)))
      `(:export ,@symbols)))

(defpackage :lowh.triangle.assets
  (:nicknames :L>assets)
  (:use :cl :debug :alexandria :L>ext :L>files :L>uri :str)
  (:export
   ;;  Config
   #:*assets-url-template*
   #:*assets-path-template*
   #:*assets-dirs*
   #:assets-dir
   #:*precompiled-assets*
   #:precompiled-asset
   ;;  Extensions
   #:extension
   #:extension-p
   #:intern-extension
   ;;  MIME types
   #:mime-type
   ;;  Classes
   #:asset
   #:font-asset
   #:image-asset
   #:preprocessed-asset
   #:css-asset
   #:js-asset
   ;;  Observers
   #:asset-path
   #:asset-url
   #:asset-sources
   #:asset-write-date
   #:assets-dirs
   #:find-asset
   #:find-assets-from-spec
   #:find-assets-from-specs
   #:locate-precompiled-assets
   ;;  Cache
   #:clear-asset-cache
   ;;  Rendering
   #:quote-html
   #:process-asset
   #:asset-include
   ;;  Precompile
   #:debug-msg
   #:msg
   #:with-msg-indent
   #:compile-asset
   #:precompile
   #:generator
   #:generate))
