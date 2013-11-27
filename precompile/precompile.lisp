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

(in-package :lowh.triangle.assets)

;;  Pipelines

(defgeneric compile-asset (asset output))

(defmethod compile-asset ((asset asset) (output stream))
  (let ((path (asset-source-path asset)))
    (msg "CP ~A" path)
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (copy-stream in output)))
  nil)

(defmethod compile-asset ((asset asset) (output pathname))
  (ensure-directories-exist output)
  (let ((path (asset-source-path asset)))
    (msg "CP ~A" path)
    (copy-files path output :replace t :update t))
  nil)

(defmethod compile-asset ((asset preprocessed-asset) (output stream))
  (let ((assets (preprocess-asset asset)))
    (cond
      ((find :assets *debug*)
       (format output "var triangle_include = (function() {
	var includes = [];
	var head = document.getElementsByTagName('head')[0];
	var inc = function () {
		if (includes.length == 0) return;
		var s = document.createElement('script');
		s.type = 'text/javascript';
		s.onreadystatechange = function () {
			if (this.readyState == 'complete') inc();
		}
		s.onload = inc;
		s.src = includes.pop();
		console.log('triangle include ', s.src);
		head.appendChild(s);

	};
	var f = function () { console.log('done loading includes'); };
	return function (x) {
		if (x === true) {
			includes.reverse();
			inc();
		} else {
			includes.push(x);
		}
	};
})();
")
       (dolist (a assets)
	 (msg "P ~A" (asset-source-path a))
	 (cond ((eq asset a) (process-asset a output))
	       (t (include-asset a output)
		  (copy-files (asset-source-path a)
			      (asset-path a)
			      :replace t :update t))))
       (format output "triangle_include(true);~%"))
      (t (dolist (a assets)
	   (msg "P ~A" (asset-source-path a))
	   (process-asset a output))))))

(defmethod compile-asset ((asset preprocessed-asset) (output pathname))
  (ensure-directories-exist output)
  (let ((assets (preprocess-asset asset)))
    (when (or (not (file-exists-p output))
	      (some (lambda (asset)
		      (file-more-recent-p (asset-source-path asset)
					  output))
		    assets))
      (with-output-to-file/utf-8 (out output)
	(compile-asset asset out)))))

;;  Precompile

(defun locate-precompiled-assets ()
  (find-assets-from-specs *precompiled-assets*))

(defun precompile ()
  (msg "Precompile")
  (with-msg-indent (1)
    (dolist (asset (locate-precompiled-assets))
      (let ((output-path (asset-path asset)))
	(msg "~A" output-path)
	(with-msg-indent (1)
	  (compile-asset asset (pathname output-path)))))))
