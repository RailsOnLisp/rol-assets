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

(in-package #:cl-user)

(defpackage #:RoL-assets.system
  (:use #:cl #:asdf))

(in-package #:RoL-assets.system)

(asdf:defsystem :RoL-assets
  :name "RoL-assets"
  :author "Thomas de Grivel <thomas@lowh.net>"
  :version "0.1"
  :description "Asset pipeline"
  :depends-on ("alexandria"
               "cfg"
               "cl-base64"
	       "cl-debug"
	       "cl-fad"
               "cl-uglify-js"
	       "closer-mop"
	       "cl-json"
	       "exec-js"
               "external-program"
               "ironclad"
               "positional"
               "re"
	       "rol-files"
	       "rol-uri"
	       "str")
  :components
  ((:file "package")
   (:file "config"     :depends-on ("package"))
   (:file "extensions" :depends-on ("package"))
   (:file "html"       :depends-on ("package"))
   (:file "lib"        :depends-on ("package"))
   (:file "mime-types" :depends-on ("extensions"))
   (:file "generate"   :depends-on ("lib"))
   (:file "asset"      :depends-on ("config" "mime-types" "lib"))
   (:file "digest"     :depends-on ("asset"))
   (:file "find"       :depends-on ("asset"))
   (:file "font"       :depends-on ("asset"))
   (:file "gzip"       :depends-on ("asset"))
   (:file "json"       :depends-on ("asset"))
   (:file "preprocess" :depends-on ("find"))
   (:file "image"      :depends-on ("asset" "html"))
   (:file "css"        :depends-on ("preprocess" "html"))
   (:file "less"       :depends-on ("css"))
   (:file "js"         :depends-on ("preprocess" "html"))
   (:file "precompile" :depends-on ("find" "digest" "gzip"))))
