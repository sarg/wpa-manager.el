;;; wpa-manager.el --- Manage wpa_supplicant via the D-Bus interface

;; Copyright (C) 2023 by Sergey Trofimov

;; Author: Sergey Trofimov <sarg@sarg.org.ru>
;; Version: 0.1
;; URL: https://github.com/sarg/wpa-manager.el
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; by default access to wpa_supplicant dbus is restricted to root only
;; add this snippet to /etc/dbus-1
;; <busconfig>
;; <policy user='myuser'>
;; <allow own='fi.w1.wpa_supplicant1'/>
;; <allow send_destination='fi.w1.wpa_supplicant1'/>
;; <allow send_interface='fi.w1.wpa_supplicant1'/>
;; <allow receive_sender='fi.w1.wpa_supplicant1' receive_type='signal'/>
;; </policy>
;; </busconfig>

;;; Code:
(require 'dbus)
(require 'cl-lib)

(defconst wpa-manager--service "fi.w1.wpa_supplicant1")
(defconst wpa-manager--service-interface "fi.w1.wpa_supplicant1.Interface")
(defconst wpa-manager--service-path "/fi/w1/wpa_supplicant1")
(defvar-local wpa-manager--interface-path nil)

(defun wpa-manager--dbus-get-props (object-path interface-name &optional prop)
  "Return all properties at OBJECT-PATH in INTERFACE-NAME.
If PROP is non-nil, return it only."
  (if prop
      (dbus-get-property :system wpa-manager--service object-path (concat wpa-manager--service interface-name) prop)
    (dbus-get-all-properties :system wpa-manager--service object-path (concat wpa-manager--service interface-name))))

(defun wpa-manager--dbus-call (method callback &rest args)
  "Call dbus METHOD with CALLBACK and ARGS supplied."
  (apply #'dbus-call-method-asynchronously
   :system wpa-manager--service wpa-manager--interface-path wpa-manager--service-interface method callback args))

(defun wpa-manager--list-entries ()
  "List last-scanned access-points."
  (let ((current-bss (wpa-manager--dbus-get-props wpa-manager--interface-path ".Interface" "CurrentBSS"))
        (bss-list (wpa-manager--dbus-get-props wpa-manager--interface-path ".Interface" "BSSs")))

    (setq tabulated-list-entries
          (cl-loop for bss in bss-list
                   for props = (wpa-manager--dbus-get-props bss ".BSS")
                   for ssid = (dbus-byte-array-to-string (alist-get "SSID" props nil nil #'equal))
                   for freq = (alist-get "Frequency" props nil nil #'equal)
                   for signal = (alist-get "Signal" props nil nil #'equal)
                   for bssid = (string-join (mapcar (lambda (n) (format "%02X" n))
                                                    (alist-get "BSSID" props nil nil #'equal))
                                            ":")
                   collect (list bss (vector
                                      (if (string= bss current-bss)
                                          (propertize ssid 'face 'bold)
                                        ssid)
                                      bssid
                                      (number-to-string freq)
                                      (number-to-string signal)))))))

(defun wpa-manager-scan ()
  "Start scanning for access points."
  (interactive)
  (wpa-manager--dbus-call "Scan"
                  (lambda () (message "Scan requested"))
                  '(:array (:dict-entry "Type" (:variant :string "active")))))

(defun wpa-manager--select-network (network)
  "Select and connect to a NETWORK."
  (wpa-manager--dbus-call "SelectNetwork" nil :object-path network))

(defun wpa-manager-connect (psk)
  "Create network entry for the currently selected access point.
Connect to it using wpa-psk method using pre-shared key PSK."
  (interactive "MPassword: ")

  (let* ((bss (tabulated-list-get-id))
         (props (wpa-manager--dbus-get-props bss ".BSS"))
         (ssid (dbus-byte-array-to-string (alist-get "SSID" props nil nil #'equal)))
         (rsn (alist-get "RSN" props nil nil #'equal))
         (key-mgmt (caar (alist-get "KeyMgmt" rsn nil nil #'equal))))

    (cl-assert (seq-contains-p key-mgmt "wpa-psk" #'string=) nil
               "Only wpa-psk is supported for now, available methods: %s" key-mgmt)

    (wpa-manager--dbus-call "AddNetwork" #'wpa-manager--select-network
                            `(:array
                              (:dict-entry "ssid" (:variant :string ,ssid))
                              (:dict-entry "psk" (:variant :string ,psk))))))

(defvar-local wpa-manager--scan-signal nil)
(define-derived-mode wpa-manager-mode tabulated-list-mode
  "WPA Supplicant"
  "Major mode for managing WPA supplicant."
  (setq tabulated-list-format [("SSID" 24 t) ("BSSID" 32 t) ("Freq" 6 t) ("Signal" 6 t)]
        tabulated-list-entries nil
        tabulated-list-padding 0
        tabulated-list-sort-key (cons "Signal" nil))

  (setq-local wpa-manager--interface-path
              ;; TODO: allow selecting interface
              (car (wpa-manager--dbus-get-props wpa-manager--service-path "" "Interfaces")))

  (setq wpa-manager--scan-signal
        (dbus-register-signal
         :system wpa-manager--service
         wpa-manager--interface-path
         wpa-manager--service-interface
         "ScanDone" (lambda (success) (message "Scan finished: %s" success) (revert-buffer))))

  (add-hook 'tabulated-list-revert-hook #'wpa-manager--list-entries nil t)
  (add-hook 'kill-buffer-hook (lambda () (dbus-unregister-object wpa-manager--scan-signal)) nil t)

  (wpa-manager--list-entries)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode))

(defvar wpa-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map [?s] #'wpa-manager-scan)
    (define-key map [?c] #'wpa-manager-connect)
    map))

(defun wpa-manager ()
  "Manage wpa_supplicant."
  (interactive)
  (dbus-ping :system wpa-manager--service 5000)
  (with-current-buffer (switch-to-buffer "*WPA Manager*")
    (wpa-manager-mode))
  nil)

(provide 'wpa-manager)
;;; wpa-manager.el ends here
