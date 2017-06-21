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

(in-package :cl-user)

(defmacro defpackage-rol-extensions ()
  (let (symbols)
    (when (find-package :RoL-extensions)
      (do-external-symbols (s :RoL-extensions)
        (push s symbols)))
    `(defpackage :RoL-extensions
       (:nicknames :RoL-ext :L>ext :lowh.triangle.extensions)
       (:export ,@symbols
                #:.woff #:.woff2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage-rol-extensions))

(defpackage :RoL-assets
  (:nicknames :L>assets :lowh.triangle.assets)
  (:use :cl :debug :alexandria :RoL-ext :RoL-files :RoL-uri :str)
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
   #:less-asset
   ;;  Observers
   #:asset-name
   #:asset-ext
   #:asset-digest
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
