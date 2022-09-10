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
  (:use #:cl #:alexandria #:agbplay #:raylib #:raygui #:cffi #:bordeaux-threads)
  (:export #:main))

(in-package #:agbplay)

(defun export-wave (rom positions loop-count muted-tracks filename &optional notify-function)
  (loop :for (song-name . position) :in positions
        :for position-index :from 0
        :do (let* ((context (make-player-context :rom rom :config (make-config :max-loops loop-count)))
                   (*player-context* context)
                   (track-audio (make-vector 0 :element-type '(vector sample)))
                   (writer (cl-wave-file-writer:make-writer :filename (merge-pathnames (format nil "~A.wav" song-name) (ensure-directories-exist (merge-pathnames (format nil "~A/" filename) (ensure-directories-exist "Export/"))))
                                                            :channel-count 2
                                                            :sample-width :16bit
                                                            :sample-rate 48000))
                   (open-file (getf writer :open-file))
                   (write-sample (getf writer :write-sample))
                   (close-file (getf writer :close-file)))
              (funcall (or notify-function #'identity) (/ position-index (length positions)))
              (player-context-init-song context position)
              (funcall open-file)
              (let ((samples-per-buffer (sound-mixer-samples-per-buffer (player-context-sound-mixer context))))
                (loop (progn
                        (setf track-audio (player-context-process context track-audio))
                        (when (player-context-end-p context)
                          (return))
                        (loop :for i :below samples-per-buffer
                              :do (let ((left 0.0)
                                        (right 0.0))
                                    (loop :for track :across track-audio
                                          :for muted-p :across muted-tracks
                                          :unless muted-p
                                            :do (incf left (sample-left (aref track i)))
                                                (incf right (sample-right (aref track i))))
                                    (funcall write-sample left)
                                    (funcall write-sample right))))))
              (funcall close-file))))

(in-package #:vgmplay)

(defconstant +buffer-size+ 2048)

(defmacro foreign-pointer-let ((&rest bindings) &body body)
  (labels ((recur (bindings)
             (if bindings
                 (with-gensyms (pointer)
                   (destructuring-bind (varname type var)
                       (car bindings)
                     `(with-foreign-pointer (,pointer (foreign-type-size ,type))
                        (setf (mem-ref ,pointer ,type) ,var)
                        (symbol-macrolet ((,varname (mem-ref ,pointer ,type))
                                          (,(symbolicate varname '#:&) ,pointer))
                          ,(recur (cdr bindings)))
                        (setf ,var (mem-ref ,pointer ,type)))))
                 `(progn ,@body))))
    (recur bindings)))

(defun draw-notes (pos-x pos-y note-vector key-width key-height border-width border-color)
  (declare (optimize (speed 3))
           (type (signed-byte 32) pos-x pos-y key-width key-height border-width)
           (type (simple-array bit) note-vector))
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
      (draw-rectangle pos-x pos-y background-width background-height border-color)
      (let ((key-alist (loop :with draw-y :of-type (signed-byte 32) := border-width
                             :for note :of-type bit :across note-vector
                             :for i :from 0
                             :for draw-x :of-type (signed-byte 32) := border-width :then (+ draw-x (if (eql (white-key-p i) (white-key-p (1- i)))
                                                                                                       (+ border-width key-width)
                                                                                                       (truncate (+ border-width key-width) 2)))
                             :collect (let ((white-key-p (white-key-p i)))
                                        (list white-key-p (+ pos-x draw-x) (+ pos-y draw-y)
                                              key-width (if white-key-p key-height (truncate key-height 2))
                                              (if (zerop note) (if white-key-p +white+ +black+) +red+))))))
        (loop :for (white-key-p . draw-rectangle-args) :in key-alist
              :when white-key-p
                :do (apply #'draw-rectangle draw-rectangle-args))
        (loop :for (white-key-p . draw-rectangle-args) :in key-alist
              :unless white-key-p
                :do (apply #'draw-rectangle draw-rectangle-args))
        (values background-width background-height)))))

(defun main-loop ()
  (declare (special player song-id song-id-spinner-edit-p rom-combo-box-index export-thread show-open-file-window-p open-file-list-scroll-index open-file-list-select-index open-file-list current-file export-max-loop-count export-muted-track-p export-all-p show-export-window-p export-progress export-max-loop-count-edit-p))
  (let ((border-color (get-color (agbplay::signed->unsigned/bits (gui-get-style +default+ +border-color-normal+) 32))))
    (with-drawing
      (clear-background +raywhite+)
      (if (or show-open-file-window-p show-export-window-p) (gui-lock) (gui-unlock))
      (let ((pos-x 675)
            (pos-y 20))
        (block player-widget
          (draw-text "VGMPlay" (- pos-x 15) pos-y 30 border-color)
          (incf pos-y 40)
          (draw-text "v1.0.0" (+ pos-x 65) pos-y 20 border-color)
          (incf pos-y 40)
          (when (gui-button (make-rectangle :x pos-x :y pos-y :width 100 :height 25)
                            (gui-icon-text 5 "Open"))
            (setf open-file-list (directory (merge-pathnames "*.gba" (ensure-directories-exist "ROM/"))))
            (setf show-open-file-window-p t))
          (incf pos-y 30)
          (unless player (return-from player-widget nil))
          (foreign-pointer-let ((value :int song-id))
            (when (gui-spinner (make-rectangle :x pos-x :y pos-y :width 100 :height 25) "No." value& 0 (1- (agbplayer-number-songs player)) (if song-id-spinner-edit-p 1 0))
              (unless (setf song-id-spinner-edit-p (not song-id-spinner-edit-p))
                (agbplayer-set-song player (setf value (clamp value 0 (1- (agbplayer-number-songs player)))))))
            (unless (or song-id-spinner-edit-p (= song-id value))
              (agbplayer-set-song player (setf value (clamp value 0 (1- (agbplayer-number-songs player)))))))
          (incf pos-y 30)
          (when (gui-button (make-rectangle :x pos-x :y pos-y :width 100 :height 25)
                            (if (or (not (agbplayer-playing-p player)) (agbplayer-paused-p player)) (gui-icon-text 131 "Play") (gui-icon-text 132 "Pause")))
            (if (and (not (agbplayer-paused-p player)) (agbplayer-playing-p player))
                (agbplayer-pause player)
                (agbplayer-play player)))
          (incf pos-y 30)
          (when (gui-button (make-rectangle :x pos-x :y pos-y :width 100 :height 25)
                            (gui-icon-text 7 (if (thread-alive-p export-thread)
                                                 (progn (gui-set-state +gui-state-disabled+)
                                                        (format nil "~A/~A" (car export-progress) (cdr export-progress)))
                                                 "Export")))
            (setf show-export-window-p t))
          (gui-set-state +gui-state-normal+)
          (incf pos-y 30))
        (when (gui-button (make-rectangle :x pos-x :y pos-y :width 100 :height 25)
                          (gui-icon-text 159 "Exit"))
          (throw 'exit nil))
        (incf pos-y 30)
        (draw-text (format nil "Copyright (C) 2022~%    coco24, ipatix") pos-x 450 10 border-color))
      (when player
        (let ((key-width 6)
              (key-height 22)
              (key-border 2)
              (pos-x 4)
              (pos-y 8))
          (loop :for track :across (agbplay::sequence-tracks
                                    (agbplay::player-context-sequence
                                     (agbplay::player-context
                                      (agbplay::agbplayer-player player))))
                :for loudness :across (agbplay::player-track-loudness (agbplay::agbplayer-player player))
                :for track-index :from 0
                :do (let ((notes-x (+ pos-x 32))
                          (track-button-x (+ pos-x 4))
                          (pos-y (+ pos-y key-border (* track-index (+ key-height (* key-border 4))))))
                      (gui-set-state (if (agbplay::player-track-mute-p (agbplay::agbplayer-player player) track-index)
                                         +gui-state-normal+ +gui-state-pressed+))
                      (when (gui-button (make-rectangle :x track-button-x :y (+ pos-y key-border)
                                                        :width key-height :height key-height)
                                        (format nil "~A" track-index))
                        (setf (agbplay::player-track-mute-p (agbplay::agbplayer-player player) track-index)
                              (not (agbplay::player-track-mute-p (agbplay::agbplayer-player player) track-index))))
                      (gui-set-state +gui-state-normal+)
                      (let ((pos-x (+ (+ pos-x 32)
                                      (draw-notes notes-x pos-y
                                                  (agbplay::track-active-notes track)
                                                  key-width key-height key-border
                                                  border-color)))
                            (loud-l (agbplay::loudness-calculator-volume-left loudness))
                            (loud-r (agbplay::loudness-calculator-volume-right loudness))
                            (max-height (+ key-height (* key-border 2)))
                            (width 4))
                        (let ((height-l (min (truncate (* 2.5 loud-l max-height)) max-height))
                              (height-r (min (truncate (* 2.5 loud-r max-height)) max-height)))
                          (draw-rectangle pos-x (- (+ pos-y max-height) height-l) width height-l (cond
                                                                                                   ((<= height-l (truncate max-height 2)) +green+)
                                                                                                   ((<= height-l (* 3 (truncate max-height 4))) +orange+)
                                                                                                   (t +red+)))
                          (draw-rectangle (+ pos-x width) (- (+ pos-y max-height) height-r) width height-r (cond
                                                                                                             ((<= height-r (truncate max-height 2)) +green+)
                                                                                                             ((<= height-r (* 3 (truncate max-height 4))) +orange+)
                                                                                                             (t +red+))))))))
        (let ((loud-l (agbplay::loudness-calculator-volume-left (agbplay::player-master-loudness (agbplay::agbplayer-player player))))
              (loud-r (agbplay::loudness-calculator-volume-right (agbplay::player-master-loudness (agbplay::agbplayer-player player))))
              (max-width 400)
              (height 6))
          (let ((width-l (min (truncate (* 2.0 loud-l max-width)) max-width))
                (width-r (min (truncate (* 2.0 loud-r max-width)) max-width)))
            (draw-rectangle (- 400 width-l) 0 width-l height (cond
                                                               ((<= width-l (truncate max-width 2)) +green+)
                                                               ((<= width-l (* 3 (truncate max-width 4))) +orange+)
                                                               (t +red+)))
            (draw-rectangle 400 0 width-r height (cond
                                                   ((<= width-r (truncate max-width 2)) +green+)
                                                   ((<= width-r (* 3 (truncate max-width 4))) +orange+)
                                                   (t +red+))))))
      (when show-open-file-window-p
        (gui-unlock)
        (let ((window-rect (make-rectangle :x (truncate (get-screen-width) 4)
                                           :y (truncate (get-screen-height) 4)
                                           :width (truncate (get-screen-width) 2)
                                           :height (truncate (get-screen-height) 2)))
              (padding-left 8)
              (padding-right 8)
              (padding-top 32)
              (padding-bottom 48))
          (when (switch ((gui-message-box window-rect
                                          "Open File"
                                          ""
                                          "Open;Cancel"))
                  (0 t)
                  (1 (when (not (minusp open-file-list-select-index))
                       (when player (agbplayer-stop player))
                       (setf song-id 0)
                       (setf player (make-agbplayer (setf current-file (nth open-file-list-select-index open-file-list))
                                                    :sample-buffer-size (* +buffer-size+ 2)))))
                  (2 t))
            (setf show-open-file-window-p nil))
          (foreign-pointer-let ((scroll-index :int open-file-list-scroll-index))
            (setf open-file-list-select-index
                  (gui-list-view (make-rectangle :x (+ (rectangle-x window-rect) padding-left)
                                                 :y (+ (rectangle-y window-rect) padding-top)
                                                 :width (- (rectangle-width window-rect) (+ padding-left padding-right))
                                                 :height (- (rectangle-height window-rect) (+ padding-top padding-bottom)))
                                 (format nil "~{~A~^;~}" (mapcar #'pathname-name open-file-list))
                                 scroll-index&
                                 open-file-list-select-index)))))
      (when show-export-window-p
        (gui-unlock)
        (let* ((window-rect (make-rectangle :x (truncate (get-screen-width) 4)
                                            :y (truncate (get-screen-height) 4)
                                            :width (truncate (get-screen-width) 2)
                                            :height (truncate (get-screen-height) 2)))
               (padding-left 8)
               (padding-top 32)
               (pos-x (+ (rectangle-x window-rect) padding-left))
               (pos-y (+ (rectangle-y window-rect) padding-top)))
          (when (switch ((gui-message-box window-rect
                                          "Export"
                                          ""
                                          "Export;Cancel"))
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
                             (make-thread (lambda ()
                                            (agbplay::export-wave rom
                                                                  positions
                                                                  loop-count
                                                                  muted-tracks
                                                                  (pathname-name current-file)
                                                                  (lambda (percent)
                                                                    (setf (car progress) (if export-all-p (* percent song-count) (* percent 1)))
                                                                    (setf (cdr progress) (if export-all-p song-count 1)))))
                                          :name "EXPORT-THREAD"))))
                  (2 t))
            (setf show-export-window-p nil))
          (foreign-pointer-let ((value :int export-max-loop-count))
            (when (gui-spinner (make-rectangle :x (+ pos-x 88) :y pos-y :width 100 :height 25)
                               "Max loop count: " value& 1 100 (if export-max-loop-count-edit-p 1 0))
              (setf song-id-spinner-edit-p (not song-id-spinner-edit-p))))
          (incf pos-y 40)
          (setf export-all-p
                (gui-check-box (make-rectangle :x pos-x
                                               :y pos-y
                                               :width 12
                                               :height 12)
                               "Export all songs"
                               (if export-all-p 1 0)))
          (incf pos-y 20)
          (setf export-muted-track-p
                (gui-check-box (make-rectangle :x pos-x
                                               :y pos-y
                                               :width 12
                                               :height 12)
                               "Export muted tracks"
                               (if export-muted-track-p 1 0)))
          (incf pos-y 20))))))

(defun main ()
  #+sbcl
  (sb-ext:disable-debugger)
  (with-window (800 480 "VGMPlay")
    (let* ((player nil)
           (buffer (make-shareable-byte-vector (* +buffer-size+ 4 2)))
           (show-open-file-window-p nil)
           (open-file-list-scroll-index 0)
           (open-file-list-select-index -1)
           (open-file-list nil)
           (current-file nil)
           (export-thread (make-thread (lambda ())))
           (song-id 0)
           (song-id-spinner-edit-p nil)
           (show-export-window-p nil)
           (export-max-loop-count 1)
           (export-max-loop-count-edit-p nil)
           (export-progress (cons 0 0))
           (export-all-p nil)
           (export-muted-track-p nil)
           (rom-combo-box-index 0))
      (declare (special player song-id song-count song-id-spinner-edit-p rom-combo-box-index export-thread open-file-list show-open-file-window-p open-file-list-scroll-index open-file-list-select-index current-file export-max-loop-count export-muted-track-p export-all-p show-export-window-p export-max-loop-count-edit-p export-progress))
      (set-target-fps 60)
      (set-audio-stream-buffer-size-default +buffer-size+)
      (with-audio-device
        (with-audio-stream (stream 48000 32 2)
          (unwind-protect
               (catch 'exit
                 (play-audio-stream stream)
                 (raygui:gui-set-font (raylib:get-font-default))
                 (loop :until (window-should-close)
                       :do (progn
                             (with-pointer-to-vector-data (buffer-pointer buffer)
                               (when (and player (is-audio-stream-processed stream))
                                 (agbplayer-take-buffer-into-pointer player buffer-pointer +buffer-size+)
                                 (update-audio-stream stream buffer-pointer +buffer-size+)))
                             (main-loop))))
            (when player
              (agbplayer-stop player)
              (agbplayer-take-buffer-into-byte-vector player buffer +buffer-size+))
            (when (thread-alive-p export-thread)
              (destroy-thread export-thread))
            (stop-audio-stream stream)))))))
