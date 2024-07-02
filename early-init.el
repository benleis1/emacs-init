;;; Early setup

;; defer gc - we will restore back at the end of init.el
(setq gc-cons-threshold most-positive-fixnum)

;; Position and size frame
(setq default-frame-alist
       '((height . 45)
         (width . 150)
         (left . 200)
         (top . 300)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (tool-bar-lines . 0)
	 (when window-system
           ;; Setting the face in here prevents flashes of
           ;; color as the theme gets activated but don't override in terminal mode because themes don't work
	   ;; the same there and background will get stuck at dark.
           (background-color . "#000000")
           (ns-appearance . dark)
	   (ns-transparent-titlebar . t))
	 ))
