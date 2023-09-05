#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem vgmplay
  :defsystem-depends-on (:deploy)
  :version "1.1.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "gpl3"
  :description "An Video Game Music player written in Common Lisp"
  :homepage "https://github.com/bohonghuang/cl-agbplay"
  :bug-tracker "https://github.com/bohonghuang/cl-agbplay/issues"
  :source-control (:git "https://github.com/bohonghuang/cl-agbplay.git")
  :serial t
  :build-operation "deploy-op"
  :build-pathname "vgmplay"
  :entry-point "vgmplay:main"
  :components ((:file "vgmplay"))
  :depends-on (#:asdf #:alexandria #:cl-agbplay #:cl-agbplay.cffi #:claw-raylib #:cl-wave-file-writer)) 
