#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem vgmplay
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "gpl3"
  :description "An Video Game Music player written in Common Lisp"
  :homepage "https://github.com/BohongHuang/cl-agbplay"
  :bug-tracker "https://github.com/BohongHuang/cl-agbplay/issues"
  :source-control (:git "https://github.com/BohongHuang/cl-agbplay.git")
  :serial t
  :build-operation program-op
  :build-pathname "vgmplay"
  :entry-point "vgmplay:run-application"
  :components ((:file "vgmplay"))
  :depends-on (#:asdf #:alexandria #:cl-agbplay #:cl-agbplay.cffi #:cl-raylib #:cl-raygui #:cl-wave-file-writer))
