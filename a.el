;;; this file implements an http-post stream data processing application.
;; copyright (c) 2021 picospuch

;;; patch url lib
(defvar p/unitv-connections (make-hash-table :test 'equal
						   :size 17)
  "A hash table of unitv connections. (:key process-buffer :value boolean)")

(defvar p/unitv-person-count '(0 0 0)
  "(total masked non-masked")

(defun is-unitv-connection (connection)
  (and (process-buffer connection)
       (gethash (process-buffer connection) p/unitv-connections)))

(defun p/url-generic-parse-url-advice (proc data)
  (if (is-unitv-connection proc)
      (p/parse-result proc data)))

(advice-mapc
 (lambda (a p)
   (print a)
   (advice-remove 'url-http-generic-filter a))
 'url-http-generic-filter)

(advice-add 'url-http-generic-filter :after 'p/url-generic-parse-url-advice)

(advice-mapc
 (lambda (a p)
   (print a))
 'url-http-generic-filter)

;;; callback
(defun p/response-status (&rest args)
  (print (buffer-string)))

(setq test '((num . 4) (obj . [((prob . 0.877928495) (x . 280) (y . 128) (w . 75) (h . 72) (type . "face")) ((prob . 0.733726382) (x . 201) (y . 185) (w . 76) (h . 72) (type . "face_masked")) ((prob . 0.726006985) (x . 166) (y . 135) (w . 63) (h . 55) (type . "face")) ((prob . 0.486062407) (x . 312) (y . 240) (w . 79) (h . 72) (type . "face"))]) (running . "Object Recognition")))

(defun p/parse-result (proc data)
  (and (/= (length data) 0)
       (condition-case err
           (progn
             (let ((data (json-read-from-string data)))
               ;;(print data)
               (setq p/unitv-person-count
                     (cons
                      ;; staff number
                      (alist-get 'num data)
                      (let ((person-list (alist-get 'obj data)))
                        (list (seq-reduce (lambda (count person)
                                            ;; masked staff number
                                            (if (equal (alist-get 'type person) "face_masked")
                                                (+ count 1)
                                              count))
                                          person-list 0)
                              (seq-reduce (lambda (count person)
                                            ;; non-masked staff number
                                            (if (equal (alist-get 'type person) "face")
                                                (+ count 1)
                                              count))
                                          person-list 0))))))
             (print p/unitv-person-count))
         (json-error (princ (format "%s" err))))))

(defun start-analysing ()
  (let ((url-request-method "POST")
	(url "http://unitv2.py:8888/func/result"))
    (puthash
     (url-http
      (url-generic-parse-url url)
      'p/response-status nil)
     t
     p/unitv-connections)))

(defun stop-analysing ()
  (maphash (lambda (k v)
	     (if (get-buffer-process k)
		 (delete-process k)))
	     p/unitv-connections)
  (clrhash p/unitv-connections))

;;(start-analysing)
;;(stop-analysing)

(defun alert (s)
  (with-current-buffer "*unitv-result*"
    (erase-buffer)
    (insert (propertize s 'face '(:foreground "red" :height 350)))))

(defun alert-setup ()
  (get-buffer-create "*unitv-result*"))


