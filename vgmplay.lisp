;;;; vgmplay.lisp

;;;; Copyright (C) 2022 Bohong Huang
;;;; 
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(defpackage vgmplay
  (:use #:cl #:alexandria #:agbplay #:cffi #:cffi-ops)
  (:export #:main))

(in-package #:agbplay)

(defun export-wave (rom positions loop-count muted-tracks filename &optional notify-function)
  (loop :for (song-name . position) :in positions
        :for position-index :from 0
        :do (let* ((context (make-player-context :rom rom :config (make-config :max-loops loop-count))))
              (funcall (or notify-function #'identity) (/ position-index (length positions))) 
              (player-context-init-song context position)
              (cl-wave-file-writer:with-writer (write-sample ((merge-pathnames (format nil "~A.wav" song-name) (ensure-directories-exist (merge-pathnames (format nil "~A/" filename) (ensure-directories-exist "Export/"))))
                                                              :channel-count 2
                                                              :sample-width-bits 16
                                                              :sample-rate 48000))
                (loop :with samples-per-buffer := (sound-mixer-samples-per-buffer (player-context-sound-mixer context))
                      :with track-audio := (make-array 0 :element-type '(simple-array sample (*)))
                      :do (setf track-audio (player-context-process context track-audio))
                      :until (player-context-end-p context)
                      :do (loop :for i :below samples-per-buffer
                                :for left := 0.0 :and right := 0.0
                                :do (loop :for track :across track-audio
                                          :for muted-p :across muted-tracks
                                          :unless muted-p
                                            :do (incf left (sample-left (aref track i)))
                                                (incf right (sample-right (aref track i))))
                                    (write-sample left)
                                    (write-sample right)))))))

(in-package #:vgmplay)

(defconstant +buffer-size+ 1600)
(defconstant +window-width+ 800)
(defconstant +window-height+ 480)

(defmacro foreign-pointer-let ((&rest bindings) &body body)
  (labels ((recur (bindings)
             (if bindings
                 (with-gensyms (pointer)
                   (destructuring-bind (varname type var)
                       (car bindings)
                     `(with-foreign-pointer (,pointer (foreign-type-size ,type))
                        (setf (mem-ref ,pointer ,type) ,var)
                        (symbol-macrolet ((,varname (mem-ref ,pointer ,type))
                                          (,(symbolicate varname '#:&) (cobj:pointer-cpointer
                                                                        ,pointer
                                                                        ',(cobj::cobject-class-definition-class
                                                                           (cobj::find-cobject-class-definition (cffi::ensure-parsed-base-type :int))))))
                          ,(recur (cdr bindings)))
                        (setf ,var (mem-ref ,pointer ,type)))))
                 `(progn ,@body))))
    (recur bindings)))

(defun draw-notes (pos-x pos-y note-vector key-width key-height border-width border-color &optional prev-note-vector)
  (declare (optimize (speed 3))
           (type (signed-byte 32) pos-x pos-y key-width key-height border-width)
           (type (simple-array bit) note-vector)
           (type (or (simple-array bit) null) prev-note-vector))
  (flet ((white-key-p (note)
           (declare (type (unsigned-byte 8) note))
           (setf note (mod note 12))
           (when (>= note 5)
             (decf note 5))
           (evenp note)))
    (assert (evenp (+ key-width border-width)))
    (let ((background-width (+ border-width (* (+ border-width key-width) (the (unsigned-byte 8)
                                                                               (loop :for i :below (length note-vector)
                                                                                     :count (white-key-p i))))))
          (background-height (+ key-height (* border-width 2))))
      (unless prev-note-vector (raylib:draw-rectangle pos-x pos-y background-width background-height border-color))
      (let ((key-alist (loop :with draw-y :of-type (signed-byte 32) := border-width
                             :for i :below 128
                             :for draw-x :of-type (signed-byte 32) := border-width :then (+ draw-x (if (eql (white-key-p i) (white-key-p (1- i)))
                                                                                                       (+ border-width key-width)
                                                                                                       (truncate (+ border-width key-width) 2)))
                             :for white-key-p := (white-key-p i)
                             :when (or (not prev-note-vector)
                                       (/= (aref prev-note-vector i) (aref note-vector i))
                                       (and (not white-key-p)
                                            (or (/= (aref prev-note-vector (1+ i)) (aref note-vector (1+ i)))
                                                (/= (aref prev-note-vector (1- i)) (aref note-vector (1- i))))))
                               :collect (list white-key-p
                                              (+ pos-x draw-x) (+ pos-y draw-y)
                                              key-width (if white-key-p key-height (truncate key-height 2))
                                              (if (zerop (aref note-vector i)) (if white-key-p raylib:+white+ raylib:+black+) raylib:+red+)))))
        (when prev-note-vector (replace prev-note-vector note-vector))
        (loop :for (white-key-p . draw-rectangle-args) :in key-alist
              :when white-key-p
                :do (apply #'raylib:draw-rectangle draw-rectangle-args))
        (loop :for (white-key-p . draw-rectangle-args) :in key-alist
              :unless white-key-p
                :do (apply #'raylib:draw-rectangle draw-rectangle-args))
        (values background-width background-height)))))

(defun main-loop ()
  (declare (special player song-id song-id-spinner-edit-p rom-combo-box-index export-thread show-open-file-window-p open-file-list-scroll-index
                    open-file-list-select-index open-file-list current-file export-max-loop-count export-muted-track-p
                    export-all-p show-export-window-p export-progress export-max-loop-count-edit-p
                    notes-render-texture note-vectors border-color temp-rectangle temp-vector2))
  (locally (declare (type (unsigned-byte #.(* (cffi:foreign-type-size :int) 8)) song-id)
                    (type fixnum open-file-list-select-index)
                    (type (vector bit-vector) note-vectors))
    (let ((rectangle temp-rectangle)
          (vector2 temp-vector2))
      (macrolet ((raylib:make-rectangle (&key x y width height)
                   `(progn
                      (setf (raylib:rectangle-x rectangle) ,x
                            (raylib:rectangle-y rectangle) ,y
                            (raylib:rectangle-width rectangle) ,width
                            (raylib:rectangle-height rectangle) ,height)
                      rectangle))
                 (raylib:make-vector2 (&key x y)
                   `(progn
                      (setf (raylib:vector2-x vector2) ,x
                            (raylib:vector2-y vector2) ,y)
                      vector2)))
        (raylib:with-drawing
          (raylib:clear-background raylib:+raywhite+)
          (if (or show-open-file-window-p show-export-window-p) (raygui:lock) (raygui:unlock))
          (let ((pos-x 675)
                (pos-y 20))
            (declare (type fixnum pos-x pos-y))
            (block player-widget
              (raylib:draw-text "VGMPlay" (- pos-x 15) pos-y 30 border-color)
              (incf pos-y 40)
              (raylib:draw-text "v1.1.0" (+ pos-x 65) pos-y 20 border-color)
              (incf pos-y 40)
              (when (plusp
                     (raygui:button (raylib:make-rectangle :x (coerce pos-x 'single-float)
                                                           :y (coerce pos-y 'single-float)
                                                           :width 100.0 :height 25.0)
                                    (raygui:icon-text 5 "Open")))
                (setf open-file-list (directory (merge-pathnames "*.gba" (ensure-directories-exist "ROM/"))))
                (setf show-open-file-window-p t))
              (incf pos-y 30)
              (unless player (return-from player-widget nil))
              (foreign-pointer-let ((value :int song-id))
                (flet ((switch-song ()
                         (agbplayer-set-song player (setf value (clamp value 0 (1- (agbplayer-number-songs player)))))
                         (setf (fill-pointer note-vectors) 0)))
                  (when (plusp
                         (raygui:spinner (raylib:make-rectangle :x (coerce pos-x 'single-float)
                                                                :y (coerce pos-y 'single-float)
                                                                :width 100.0 :height 25.0)
                                         "No." value& 0 (1- (agbplayer-number-songs player)) song-id-spinner-edit-p))
                    (unless (setf song-id-spinner-edit-p (not song-id-spinner-edit-p))
                      (switch-song)))
                  (unless (or song-id-spinner-edit-p (= song-id value))
                    (switch-song))))
              (incf pos-y 30)
              (when (plusp
                     (raygui:button (raylib:make-rectangle :x (coerce pos-x 'single-float)
                                                           :y (coerce pos-y 'single-float)
                                                           :width 100.0 :height 25.0)
                                    (if (or (not (agbplayer-playing-p player)) (agbplayer-paused-p player))
                                        (raygui:icon-text 131 "Play") (raygui:icon-text 132 "Pause"))))
                (if (and (not (agbplayer-paused-p player)) (agbplayer-playing-p player))
                    (agbplayer-pause player)
                    (agbplayer-play player)))
              (incf pos-y 30)
              (when (plusp
                     (raygui:button (raylib:make-rectangle :x (coerce pos-x 'single-float)
                                                           :y (coerce pos-y 'single-float)
                                                           :width 100.0 :height 25.0)
                                    (raygui:icon-text 7 (if (bt2:thread-alive-p export-thread)
                                                            (progn (raygui:set-state #.(foreign-enum-value 'raygui:state :disabled))
                                                                   (format nil "~A/~A" (car export-progress) (cdr export-progress)))
                                                            "Export"))))
                (setf show-export-window-p t))
              (raygui:set-state #.(foreign-enum-value 'raygui:state :normal))
              (incf pos-y 30))
            (when (plusp
                   (raygui:button
                    (raylib:make-rectangle :x (coerce pos-x 'single-float)
                                           :y (coerce pos-y 'single-float)
                                           :width 100.0 :height 25.0)
                    (raygui:icon-text 159 "Exit")))
              (throw 'exit nil))
            (incf pos-y 30)
            (raylib:draw-text (format nil "Copyright (C) 2022-2023~%       coco24, ipatix") (- pos-x 10) 450 10 border-color))
          (when player
            (let ((key-width 6)
                  (key-height 22)
                  (key-border 2)
                  (pos-x 4)
                  (pos-y 8))
              (declare (type fixnum key-width key-height key-border pos-x pos-y))
              (when (zerop (length note-vectors))
                (raylib:with-texture-mode notes-render-texture
                  (raylib:clear-background raylib:+blank+)))
              (loop :for track :across (agbplay::sequence-tracks
                                        (agbplay::player-context-sequence
                                         (agbplay::player-context
                                          (agbplay::agbplayer-player player))))
                    :for loudness :across (agbplay::player-track-loudness (agbplay::agbplayer-player player))
                    :for track-index :of-type fixnum :from 0
                    :do (let ((notes-x (+ pos-x 32))
                              (track-button-x (+ pos-x 4))
                              (pos-y (+ pos-y key-border (the fixnum (* track-index (the fixnum (+ key-height (the fixnum (* key-border 4)))))))))
                          (declare (type fixnum notes-x track-button-x pos-y))
                          (raygui:set-state (if (agbplay::player-track-mute-p (agbplay::agbplayer-player player) track-index)
                                                #.(foreign-enum-value 'raygui:state :normal)
                                                #.(foreign-enum-value 'raygui:state :pressed)))
                          (when (plusp
                                 (raygui:button (raylib:make-rectangle :x (coerce track-button-x 'single-float)
                                                                       :y (coerce (+ pos-y key-border) 'single-float)
                                                                       :width (coerce key-height 'single-float)
                                                                       :height (coerce key-height 'single-float))
                                                (format nil "~A" track-index)))
                            (setf (agbplay::player-track-mute-p (agbplay::agbplayer-player player) track-index)
                                  (not (agbplay::player-track-mute-p (agbplay::agbplayer-player player) track-index))))
                          (raygui:set-state #.(foreign-enum-value 'raygui:state :normal))
                          (let ((pos-x (+ (the fixnum (+ pos-x 32))
                                          (the fixnum (raylib:with-texture-mode notes-render-texture
                                                        (draw-notes notes-x pos-y
                                                                    (agbplay::track-active-notes track)
                                                                    key-width key-height key-border
                                                                    border-color
                                                                    (if (< track-index (length note-vectors))
                                                                        (aref note-vectors track-index)
                                                                        (tagbody (vector-push-extend (copy-array (agbplay::track-active-notes track)) note-vectors))))))))
                                (loud-l (agbplay::loudness-calculator-volume-left loudness))
                                (loud-r (agbplay::loudness-calculator-volume-right loudness))
                                (max-height (+ key-height (* key-border 2)))
                                (width 4))
                            (declare (type fixnum pos-x max-height width)
                                     (type single-float loud-l loud-r))
                            (let ((height-l (min (the fixnum (truncate (* 2.5 loud-l max-height))) max-height))
                                  (height-r (min (the fixnum (truncate (* 2.5 loud-r max-height))) max-height)))
                              (raylib:draw-rectangle pos-x (- (+ pos-y max-height) height-l) width height-l (cond
                                                                                                              ((<= height-l (truncate max-height 2)) raylib:+green+)
                                                                                                              ((<= height-l (* 3 (truncate max-height 4))) raylib:+orange+)
                                                                                                              (t raylib:+red+)))
                              (raylib:draw-rectangle (+ pos-x width) (- (+ pos-y max-height) height-r) width height-r (cond
                                                                                                                        ((<= height-r (truncate max-height 2)) raylib:+green+)
                                                                                                                        ((<= height-r (* 3 (truncate max-height 4))) raylib:+orange+)
                                                                                                                        (t raylib:+red+)))))))
              (raylib:draw-texture-rec (raylib:render-texture-texture notes-render-texture)
                                       (raylib:make-rectangle :x 0.0 :y (coerce +window-height+ 'single-float)
                                                              :width (coerce +window-width+ 'single-float)
                                                              :height (coerce (- +window-height+) 'single-float))
                                       (raylib:make-vector2 :x 0.0 :y 0.0) raylib:+white+))
            (let ((loud-l (agbplay::loudness-calculator-volume-left (agbplay::player-master-loudness (agbplay::agbplayer-player player))))
                  (loud-r (agbplay::loudness-calculator-volume-right (agbplay::player-master-loudness (agbplay::agbplayer-player player))))
                  (max-width 400)
                  (height 6))
              (declare (type fixnum max-width height)
                       (type single-float loud-l loud-r))
              (let ((width-l (min (the fixnum (truncate (* 2.0 loud-l max-width))) max-width))
                    (width-r (min (the fixnum (truncate (* 2.0 loud-r max-width))) max-width)))
                (raylib:draw-rectangle (- 400 width-l) 0 width-l height (cond
                                                                          ((<= width-l (truncate max-width 2)) raylib:+green+)
                                                                          ((<= width-l (* 3 (truncate max-width 4))) raylib:+orange+)
                                                                          (t raylib:+red+)))
                (raylib:draw-rectangle 400 0 width-r height (cond
                                                              ((<= width-r (truncate max-width 2)) raylib:+green+)
                                                              ((<= width-r (* 3 (truncate max-width 4))) raylib:+orange+)
                                                              (t raylib:+red+))))))
          (when show-open-file-window-p
            (raygui:unlock)
            (let ((window-rect (raylib:make-rectangle :x (coerce (truncate (raylib:get-screen-width) 4) 'single-float)
                                                      :y (coerce (truncate (raylib:get-screen-height) 4) 'single-float)
                                                      :width (coerce (truncate (raylib:get-screen-width) 2) 'single-float)
                                                      :height (coerce (truncate (raylib:get-screen-height) 2) 'single-float)))
                  (padding-left 8)
                  (padding-right 8)
                  (padding-top 32)
                  (padding-bottom 48))
              (when (switch ((raygui:message-box window-rect "Open File" "" "Open;Cancel"))
                      (0 t)
                      (1 (ignore-errors
                          (when (not (minusp open-file-list-select-index))
                            (when player (agbplayer-stop player))
                            (setf player (make-agbplayer :rom (setf current-file (nth open-file-list-select-index open-file-list))))
                            (setf song-id 0)
                            (setf (fill-pointer note-vectors) 0))))
                      (2 t))
                (setf show-open-file-window-p nil))
              (foreign-pointer-let ((scroll-index :int open-file-list-scroll-index))
                (foreign-pointer-let ((select-index :int open-file-list-select-index))
                  (raygui:list-view (raylib:make-rectangle :x (+ (raylib:rectangle-x window-rect) (coerce padding-left 'single-float))
                                                           :y (+ (raylib:rectangle-y window-rect) (coerce padding-top 'single-float))
                                                           :width (- (raylib:rectangle-width window-rect) (coerce (+ padding-left padding-right) 'single-float))
                                                           :height (- (raylib:rectangle-height window-rect) (coerce (+ padding-top padding-bottom) 'single-float)))
                                    (format nil "~{~A~^;~}" (mapcar #'pathname-name open-file-list))
                                    scroll-index&
                                    select-index&)))))
          (when show-export-window-p
            (raygui:unlock)
            (let* ((window-rect (raylib:make-rectangle :x (coerce (truncate (raylib:get-screen-width) 4) 'single-float)
                                                       :y (coerce (truncate (raylib:get-screen-height) 4) 'single-float)
                                                       :width (coerce (truncate (raylib:get-screen-width) 2) 'single-float)
                                                       :height (coerce (truncate (raylib:get-screen-height) 2) 'single-float)))
                   (padding-left 8)
                   (padding-top 32)
                   (pos-x (+ (raylib:rectangle-x window-rect) (coerce padding-left 'single-float)))
                   (pos-y (+ (raylib:rectangle-y window-rect) (coerce padding-top 'single-float))))
              (when (switch ((raygui:message-box window-rect "Export" "" "Export;Cancel"))
                      (0 t)
                      (1 (let ((rom (agbplay::player-rom (agbplay::agbplayer-player player)))
                               (progress export-progress)
                               (muted-tracks (if export-muted-track-p
                                                 (make-array (length (agbplay::player-muted-tracks (agbplay::agbplayer-player player)))
                                                             :element-type 'boolean :initial-element nil)
                                                 (copy-seq (agbplay::player-muted-tracks (agbplay::agbplayer-player player)))))
                               (positions (if export-all-p
                                              (loop :for i :below (agbplayer-number-songs player)
                                                    :collect (cons i (agbplay::song-table-song-position (agbplay::agbplayer-song-table player) i)))
                                              (list (cons song-id (agbplay::song-table-song-position (agbplay::agbplayer-song-table player) song-id)))))
                               (current-file current-file)
                               (loop-count export-max-loop-count)
                               (song-count (agbplayer-number-songs player))
                               (export-all-p export-all-p))
                           (setf export-thread
                                 (bt2:make-thread
                                  (lambda ()
                                    (agbplay::export-wave rom
                                                          positions
                                                          loop-count
                                                          muted-tracks
                                                          (pathname-name current-file)
                                                          (lambda (percent)
                                                            (declare (type rational percent))
                                                            (setf (car progress) (if export-all-p (the fixnum (* percent song-count)) percent)
                                                                  (cdr progress) (if export-all-p song-count 1)))))
                                  :trap-conditions nil
                                  :name "EXPORT-THREAD"))))
                      (2 t))
                (setf show-export-window-p nil))
              (foreign-pointer-let ((value :int export-max-loop-count))
                (when (plusp
                       (raygui:spinner (raylib:make-rectangle :x (coerce (+ pos-x 88) 'single-float)
                                                              :y (coerce pos-y 'single-float)
                                                              :width 100.0 :height 25.0)
                                       "Max loop count: " value& 1 100 export-max-loop-count-edit-p))
                  (setf song-id-spinner-edit-p (not song-id-spinner-edit-p))))
              (incf pos-y 40)
              (foreign-pointer-let ((value :bool export-all-p))
                (raygui:check-box (raylib:make-rectangle :x (coerce pos-x 'single-float)
                                                         :y (coerce pos-y 'single-float)
                                                         :width 12.0 :height 12.0)
                                  "Export all songs" value&))
              (incf pos-y 20)
              (foreign-pointer-let ((value :bool export-muted-track-p))
                (raygui:check-box (raylib:make-rectangle :x (coerce pos-x 'single-float)
                                                         :y (coerce pos-y 'single-float)
                                                         :width 12.0 :height 12.0)
                                  "Export muted tracks" value&))
              (incf pos-y 20))))))))

(defun main ()
  (raylib:with-window ("VGMPlay" (+window-width+ +window-height+))
    (let* ((player nil)
           (buffer (make-shareable-byte-vector (* +buffer-size+ 4 2)))
           (show-open-file-window-p nil)
           (open-file-list-scroll-index 0)
           (open-file-list-select-index -1)
           (open-file-list nil)
           (current-file nil)
           (export-thread (bt2:make-thread (lambda ())))
           (song-id 0)
           (song-id-spinner-edit-p nil)
           (show-export-window-p nil)
           (export-max-loop-count 1)
           (export-max-loop-count-edit-p nil)
           (export-progress (cons 0 0))
           (export-all-p nil)
           (export-muted-track-p nil)
           (rom-combo-box-index 0)
           (notes-render-texture (raylib:load-render-texture +window-width+ +window-height+))
           (note-vectors (make-array 0 :fill-pointer 0 :adjustable t :element-type 'bit-vector))
           (border-color (raylib:get-color (agbplay::signed->unsigned/bits
                                            (raygui:get-style #.(foreign-enum-value 'raygui:control :default)
                                                              #.(foreign-enum-value 'raygui:control-property :border-color-normal))
                                            32)))
           (temp-rectangle (raylib:make-rectangle))
           (temp-vector2 (raylib:make-rectangle)))
      (declare (special player song-id song-count song-id-spinner-edit-p rom-combo-box-index export-thread open-file-list
                        show-open-file-window-p open-file-list-scroll-index open-file-list-select-index current-file export-max-loop-count
                        export-muted-track-p export-all-p show-export-window-p export-max-loop-count-edit-p export-progress
                        notes-render-texture note-vectors border-color temp-rectangle temp-vector2))
      (raylib:set-target-fps 60)
      (raylib:set-audio-stream-buffer-size-default +buffer-size+)
      (raylib:with-audio-device
        (raylib:with-audio-stream (stream (48000 32 2))
          (unwind-protect
               (catch 'exit
                 (raylib:play-audio-stream stream)
                 (raygui:set-font (raylib:get-font-default))
                 (loop :until (raylib:window-should-close)
                       :do (progn
                             (with-pointer-to-vector-data (buffer-pointer buffer)
                               (when (and player (raylib:is-audio-stream-processed stream))
                                 (agbplayer-take-sample-buffer-into-pointer player buffer-pointer +buffer-size+)
                                 (raylib:%update-audio-stream (& stream) buffer-pointer +buffer-size+)))
                             (main-loop))))
            (when player
              (agbplayer-stop player)
              (agbplayer-take-sample-buffer-into-byte-vector player buffer +buffer-size+))
            (when (bt2:thread-alive-p export-thread)
              (bt2:destroy-thread export-thread))
            (raylib:stop-audio-stream stream)))))))
