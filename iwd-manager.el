;;; iwd-manager.el --- Manage IWD via the D-Bus interface -*- lexical-binding: t; -*-

;; Copyright (C) 2023 by Sergey Trofimov

;; Author: Sergey Trofimov <sarg@sarg.org.ru>
;; Version: 0.1
;; URL: https://github.com/sarg/wpa-manager.el
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:
;;; A dbus-based client for iNet Wireless Daemon.
;;; Supports connecting to PSK networks.

;;; Code:
(require 'dbus)
(require 'cl-lib)

(defconst iwd-manager--service "net.connman.iwd")
(defconst iwd-manager--path "/net/connman/iwd")
(defconst iwd-manager--agent-path "/iwd/agent")
(defconst iwd-manager--agent-manager-interface "net.connman.iwd.AgentManager")
(defconst iwd-manager--station-interface "net.connman.iwd.Station")

(defvar-local iwd-manager--cached-objects nil
  "Cache for D-Bus managed objects to avoid repeated calls.")
(put 'iwd-manager--cached-objects 'permanent-local t)

(defvar-local iwd-manager--device nil
  "Wireless interface to manage.")
(put 'iwd-manager--device 'permanent-local t)

(defvar-local iwd-manager--registered-objects '())
(put 'iwd-manager--registered-objects 'permanent-local t)

(defsubst iwd-manager--interface (obj iface)
  "Return IFACE of OBJ."
  (car (alist-get iface (cadr obj) nil nil #'string=)))

(defsubst iwd-manager--find-obj (path)
  "Find cached dbus object for given PATH."
  (assoc path iwd-manager--cached-objects #'string=))

(defun iwd-manager--list-entries ()
  "List last-scanned access-points."
  (setq iwd-manager--cached-objects
        (dbus-get-all-managed-objects :system iwd-manager--service "/"))

  (unless iwd-manager--device
    (when-let* ((device
                 (seq-find
                  (lambda (obj) (assoc iwd-manager--station-interface (cadr obj) #'string=))
                  iwd-manager--cached-objects))

                (device-props (iwd-manager--interface device "net.connman.iwd.Device")))
      (setq iwd-manager--device device)
      (setq mode-name (concat "IWD: " (alist-get "Name" device-props nil nil #'string=)))))

  (cl-loop
   with ordered =
   (dbus-call-method
    :system iwd-manager--service (car iwd-manager--device) iwd-manager--station-interface
    "GetOrderedNetworks")
   for net in ordered
   collect (let* ((net-path (car net))
                  (obj (iwd-manager--find-obj net-path))
                  (props (iwd-manager--interface obj "net.connman.iwd.Network"))
                  (signal (/ (cadr net) 100))
                  (ssid (alist-get "Name" props nil nil #'string=))
                  (connected (alist-get "Connected" props nil nil #'string=)))
             (list obj
                   (vector
                    (propertize ssid 'face (when connected 'bold))
                    (format "%d dBm" signal))))
   into entries
   finally (setq tabulated-list-entries entries)))

(defun iwd-manager-scan ()
  "Start scanning for access points."
  (interactive nil iwd-manager-mode)
  (dbus-call-method
   :system iwd-manager--service
   (car iwd-manager--device) iwd-manager--station-interface
   "Scan")
  (message "Scanning for networks..."))

(defun iwd-manager--request-passphrase (network-path)
  "Request passphrase for network identified by NETWORK-PATH."
  (when-let* ((obj (iwd-manager--find-obj network-path))
              (props (iwd-manager--interface obj "net.connman.iwd.Network"))
              (ssid (alist-get "Name" props nil nil #'string=)))
    (condition-case err
        (read-passwd (format "Passphrase for %s: " ssid))
      (quit (list :error "net.connman.iwd.Agent.Error.Canceled"
                  (error-message-string err))))))

(defun iwd-manager--register-agent ()
  "Register the agent with IWD."
  (add-to-list
   'iwd-manager--registered-objects
   (dbus-register-method
    :system nil iwd-manager--agent-path "net.connman.iwd.Agent"
    "RequestPassphrase" #'iwd-manager--request-passphrase
    'dont-register))

  (dbus-call-method
   :system iwd-manager--service iwd-manager--path iwd-manager--agent-manager-interface
   "RegisterAgent" :object-path iwd-manager--agent-path)

  (let ((buffer (current-buffer)))
    (dolist (i '(("/" . "InterfacesAdded")
                 ("/" . "InterfacesRemoved")
                 (nil . "PropertiesChanged")))
      (add-to-list
       'iwd-manager--registered-objects
       (dbus-register-signal
        :system iwd-manager--service (car i)
        dbus-interface-objectmanager (cdr i)
        (lambda (&rest _)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (revert-buffer)))))))))

(defun iwd-manager--unregister-agent ()
  "Unregister the agent from IWD."
  (dolist (o iwd-manager--registered-objects)
    (dbus-unregister-object o))

  (setq iwd-manager--registered-objects '())
  (dbus-call-method
   :system iwd-manager--service iwd-manager--path iwd-manager--agent-manager-interface
   "UnregisterAgent" :object-path iwd-manager--agent-path))

(defun iwd-manager-delete-network (&rest _)
  "Forget NETWORK."
  (interactive nil iwd-manager-mode)
  (if-let* ((obj (tabulated-list-get-id))
            (props (iwd-manager--interface obj "net.connman.iwd.Network"))
            ((alist-get "Connected" props nil nil #'string=))
            (net (alist-get "KnownNetwork" props nil nil #'string=)))
      (dbus-call-method
       :system iwd-manager--service net "net.connman.iwd.KnownNetwork"
       "Forget")
    (user-error "Not a known network")))

(defun iwd-manager-connect ()
  "Connect to the currently selected access point."
  (interactive nil iwd-manager-mode)
  (let* ((obj (tabulated-list-get-id))
         (network-path (car obj)))
    (dbus-call-method-asynchronously
     :system iwd-manager--service network-path "net.connman.iwd.Network"
     "Connect" nil)))

;;;###autoload
(define-derived-mode iwd-manager-mode tabulated-list-mode
  "IWD"
  "Major mode for managing IWD."
  :interactive nil

  (setq tabulated-list-format [("SSID" 24 t) ("Signal" 6 t)]
        tabulated-list-entries nil
        tabulated-list-padding 0)

  (add-hook 'tabulated-list-revert-hook #'iwd-manager--list-entries nil t)
  (add-hook 'kill-buffer-hook #'iwd-manager--unregister-agent nil t)

  (iwd-manager--list-entries)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode))

(defvar iwd-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map [?s] #'iwd-manager-scan)
    (define-key map [?c] #'iwd-manager-connect)
    (define-key map [?D] #'iwd-manager-delete-network)
    map))

;;;###autoload
(defun iwd-manager ()
  "Manage wifi connections through IWD."
  (interactive)
  (dbus-ping :system iwd-manager--service 5000)
  (with-current-buffer (switch-to-buffer "*IWD Manager*")
    (unless iwd-manager--registered-objects
      (iwd-manager--register-agent))
    (iwd-manager-mode))
  nil)

(provide 'iwd-manager)
;;; iwd-manager.el ends here
