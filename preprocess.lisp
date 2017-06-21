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

;;  Preprocessed assets

(defclass preprocessed-asset (asset) ())

;;  Preprocessing an asset gives a list of assets to compile.

(defun preprocess/require (asset specs assets)
  (let ((found (find-assets-from-specs specs (class-of asset))))
    (unless found
      (error "Asset not found : ~S~&Required by ~S" specs asset))
    (reduce (lambda (assets asset)
              (preprocess/asset asset assets))
            found
            :initial-value assets)))

(defun preprocess/comment (asset comment assets)
  #+nil(debug-msg "Comment ~S" comment)
  (or (cl-ppcre:register-groups-bind (command arguments)
          ("^\\W*=\\s*(\\w+)(\\s+\\S+)*\\s*$" comment)
        (let ((arg-list (rest (cl-ppcre:split "\\s+" arguments))))
          (cond ((string= "require" command)
                 (setf assets (preprocess/require asset arg-list assets)))
                (t
                 (warn "Unknown preprocessor command : ~A ~S"
                       command arg-list)
                 assets))))
      assets))

(defun match-comment-start-and-end (start end)
  (when (find-if (lambda (match)
                   (and (string= start (car match))
                        (string= end (cdr match))))
                 '(("/*" . "*/")))
    t))

(defun preprocess/stream (asset stream assets &optional stack)
  (let ((line (read-line stream nil))
        start comment end)
    (or (when line
          (cl-ppcre:register-groups-bind (s c e)
              ("^\\s*(/\\*|//)?(.*?)(?:(\\*/).*)?$" line)
            (setf start s comment c end e))
          (unless (empty-p start)
            (push start stack))
          (unless (empty-p comment)
            (if stack
                (setf assets (preprocess/comment asset comment assets))
                (setf line nil)))
          (when (string= "//" (first stack))
            (pop stack))
          (when (and stack end (match-comment-start-and-end
                                (first stack) end))
            (pop stack)
            (unless stack
              (setf line nil)))
          (when line
            (preprocess/stream asset stream assets stack)))
        (cons asset assets))))

(defun preprocess/asset (asset assets)
  (let ((path (asset-source-path asset)))
    ;;(msg "PP ~A" path)
    (with-msg-indent (1)
      (with-input-from-file/utf-8 (input path)
        (preprocess/stream asset input assets)))))

(defmethod asset-sources% ((asset preprocessed-asset))
  (nreverse (preprocess/asset asset nil)))

;;  Compile preprocessed assets

(defmethod compile-asset ((asset preprocessed-asset) (output stream))
  (let ((assets (asset-sources asset)))
    (loop for a in assets
       ;;do (msg "P ~A" (asset-source-path a))
       do (process-asset a output))))

(defmethod compile-asset ((asset preprocessed-asset) (output pathname))
  (ensure-directories-exist output)
  (let ((assets (asset-sources asset)))
    (when (or (not (file-exists-p output))
              (some (lambda (asset)
                      (file-more-recent-p (asset-source-path asset)
                                          output))
                    assets))
      (with-output-to-file/utf-8 (stream output)
        (compile-asset asset stream)))))
