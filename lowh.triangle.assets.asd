;;
;;  LowH Triangle Assets  -  Asset pipeline
;;
;;  Copyright 2012 Thomas de Grivel <thomas@lowh.net>
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(defpackage :lowh.triangle.assets.system
  (:use :cl :asdf))

(in-package :lowh.triangle.assets.system)

(asdf:defsystem :lowh.triangle.assets
  :name "lowh.triangle.assets"
  :author "Thomas de Grivel <thomas@lowh.net>"
  :version "0.1"
  :description "Asset pipeline"
  :depends-on ("alexandria"
	       "cl-fad"
	       "closer-mop"
	       "cl-json"
	       "exec-js"
	       "lowh.triangle.files"
	       "lowh.triangle.uri")
  :components
  ((:file "package")
   (:file "config"     :depends-on ("package"))
   (:file "lib"        :depends-on ("package"))
   (:file "extensions" :depends-on ("package"))
   (:file "asset"      :depends-on ("extensions" "lib"))
   (:file "find"       :depends-on ("asset"))
   (:file "font"       :depends-on ("asset"))
   (:file "preprocess" :depends-on ("find"))
   (:file "css"        :depends-on ("preprocess"))
   (:file "image"      :depends-on ("preprocess"))
   (:file "js"         :depends-on ("preprocess"))))
