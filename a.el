(defun temperature-action (t-list)
	(let ((tpt (/ (+ (car t-list) (* (cadr t-list) 256)) 100.0)))
		(message
		 (format "Current Temperature is %.2f \u00b0C"
						 tpt))
		(if (> tpt 20)
				(bluetooth-set-led t)
			(bluetooth-set-led nil))))
	
(defun bluetooth-get-temperature ()
	(interactive)
	(dbus-call-method-asynchronously
	 :system "org.bluez"
	 "/org/bluez/hci0/dev_84_2E_14_31_BA_39/service0035/char0036"
	 "org.bluez.GattCharacteristic1"
	 "ReadValue"
	 'temperature-action
	 '((:dict-entry "offset" (:variant :uint16 0))))
	)

(defun bluetooth-set-led (s)
	(dbus-call-method
	 :system "org.bluez"
	 "/org/bluez/hci0/dev_84_2E_14_31_BA_39/service003e/char0044"
	 "org.bluez.GattCharacteristic1"
	 "WriteValue"
	 (list :byte (if s #x1 #x0))
	 '((:dict-entry "offset" (:variant :uint16 0))))
	(if s (message "led is on")
		(message "led is off")))

(run-at-time nil 0.5 'bluetooth-get-temperature)

;; (bluetooth-set-led nil)

;; (setq dbus-debug nil)

(dbus-ping :system "org.bluez" 3)
