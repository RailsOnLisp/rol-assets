;;
;;  Assets  -  Asset pipeline
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

(in-package :lowh.triangle.assets)

;;  Config

(defvar *debug* nil)

(defvar *default-assets-dirs*
  '("lib/*/triangle/assets/*/"
    "lib/triangle/*/triangle/assets/*/"
    "app/assets/*/"
    "vendor/assets/*/"
    "assets/*/"))

(defparameter *assets-dirs*
  *default-assets-dirs*)

(defvar *default-precompiled-assets*
  '("app.css"
    "app.js"
    "**/*.jpeg"
    "**/*.jpg"
    "**/*.png"
    "**/*.svg"
    "**/*.eot"
    "**/*.ttf"
    "**/*.woff"))

(defparameter *precompiled-assets*
  *default-precompiled-assets*)

(defvar *assets-url-template* "/assets{/name}{.ext}")
(defvar *assets-path-template* "public/assets{/name}{.ext}")

;;  Config stanzas

(defun assets-dir (pathspec)
  (let* ((namestring (enough-namestring pathspec))
	 (path (if (char= #\/ (last-elt namestring))
		   namestring
		   (str namestring "/"))))
    (pushnew path *assets-dirs* :test #'string=)))

(defun precompiled-asset (asset-name)
  (pushnew asset-name *precompiled-assets* :test #'string=))

;;  Read config

(defun assets-dirs ()
  (directories (reverse *assets-dirs*)))
