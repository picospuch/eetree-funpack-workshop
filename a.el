(setq test-string
"PRED: 1, 0.03, 0.97, 0.00
LATENCY: 136
")

(defun sensortile-result (p o)
  (show-result
   (let* ((tokens (split-string o "[ \n:,]+"))
          (alpha (string-to-number (nth 2 tokens)))
          (beta (string-to-number (nth 3 tokens)))
          (gamma (string-to-number (nth 4 tokens))))
     (print (list tokens alpha beta gamma))
     (cond ((> alpha 0.6) "Doh")
           ((> beta 0.6) "listening")
           ((> gamma 0.6) "Whistle")
           ("Knock")))))

(sensortile-result nil test-string)

(defun show-result (s)
  (with-current-buffer "*sensortilebox-result*"
    (erase-buffer)
    (insert (propertize s 'face '(:foreground "red" :height 1000)))))

(show-result "Whistle")


(defun main ()
  (with-current-buffer (get-buffer-create "*sensortilebox-result*")
    (text-mode)
    (display-buffer (current-buffer)))
  (setq sensortilebox-process (make-serial-process :port "/dev/cu.usbmodemFFFFFFFEFFFF1" :speed 115200 :filter 'sensortile-result)))

(main)

(delete-process sensortilebox-process)
