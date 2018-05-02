;;;; volume.lisp

(in-package #:volume)

;;; Volume module for StumpWM.
;;;
;;; Code:

;; For some reason this locks up the entire window manager sometimes
(defun get-volume ()
  (let* ((regex ".*\\[([0-9]+)%\\].*\\[(on|off)\\]\\n")
         (amixer-output (run-shell-command "amixer -D default" t))
         (percent-status (nth-value 1 (cl-ppcre:scan-to-strings
                                       regex
                                       amixer-output))))
    (if (string= (aref percent-status 1) "off")
        "[Mute]"
        (aref percent-status 0))))

(defun safe-get-volume ()
  (handler-case
      (with-timeout (0.1)
        (get-volume))
    (timeout-error ()
      "TIMEOUT")))

(defun fmt-vol (ml)
  (declare (ignore ml))
  (safe-get-volume))

(add-screen-mode-line-formatter #\V #'fmt-vol)

;;; End of file
