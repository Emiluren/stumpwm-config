(in-package :stumpwm)

(load-module "battery-portable")
(load-module "volume")
(load-module "ttf-fonts")

;; Use the menu key as prefix
(set-prefix-key (kbd "Menu"))

;; Make focus follow mouse
(setf *mouse-focus-policy* :sloppy)

;; Prevent windows like Emacs from getting a giant border
(setf *window-border-style* :thin)

;; Keep windows where they are when creating new frames
(setf *new-frame-action* :empty)

;; The default font suddenly made everything invisible
(set-font (make-instance 'xft:font
                         :family "Anonymous Pro"
                         :subfamily "Regular"
                         :antialias t))

(defparameter *time-command*
  "date '+%R' | tr -d '\\n'")

(defun fmt-time (ml)
  (declare (ignore ml))
  (run-shell-command *time-command* t))

(add-screen-mode-line-formatter #\T #'fmt-time)

;; turn on the mode line for all heads
(setf *screen-mode-line-format* "[%n] %v ^>Vol: %V | Bat: %B | %T")

(defcommand enable-mode-line-on-all () ()
  (dolist (screen *screen-list*)
    (dolist (head (screen-heads screen))
      (enable-mode-line screen head t))))

(enable-mode-line-on-all)

;; Create a swank server so we can connect to it through slime
(require :swank)
(let ((server-running nil))
  (defcommand swank () ()
    "Toggle the swank server on/off"
    (if server-running
        (progn
          (swank:stop-server 4005)
          (message "Stopping swank.")
          (setf server-running nil))
        (progn
          (swank:create-server :port 4005
                               :style swank:*communication-style*
                               :dont-close t)
          (message "Starting swank on localhost:4005.")
          (setf server-running t)))))

;; Root key bindings
;; Change the default terminal to urxvt
(define-key *root-map* (kbd "c") "exec urxvt -e fish")
(define-key *root-map* (kbd "C-c") "exec urxvt")

;; F11 doesn't really work with fn
(define-key *root-map* (kbd "z") "fullscreen")

;; Use other window instead of pull-hidden-other
(define-key *root-map* (kbd "Menu") "other-in-frame")

;; Top key bindings
;; Media keys
(define-key *top-map* (kbd "XF86AudioLowerVolume")
  "exec amixer -D pulse sset Master 3%-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume")
  "exec amixer -D pulse sset Master 3%+")
(define-key *top-map* (kbd "XF86AudioMute")
  "exec amixer -D pulse sset Master toggle")

(define-key *top-map* (kbd "XF86MonBrightnessDown")
  "exec xbacklight -dec 5")
(define-key *top-map* (kbd "XF86MonBrightnessUp")
  "exec xbacklight -inc 5")

;; Stop Fn key from interfering with describe-key in Emacs
(defcommand do-nothing () ())
(define-key *top-map* (kbd "XF86WakeUp") "do-nothing")

;; Vi keys for moving around
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-l") "move-focus right")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-k") "move-focus up")

(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-L") "move-window right")
(define-key *top-map* (kbd "s-J") "move-window down")
(define-key *top-map* (kbd "s-K") "move-window up")

;; skippy-xd is a sort of window menu
(define-key *top-map* (kbd "s-g") "exec skippy-xd")

;; Sometimes udev events are not triggered on connect/disconnect
;; Running xrandr forces them
(define-key *top-map* (kbd "s-y") "exec xrandr")

;; Sys_req is Shift-Printscreen
(define-key *top-map* (kbd "Sys_Req") "exec ~/bin/screenshot")

