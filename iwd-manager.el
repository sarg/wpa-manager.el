;;; iwd-manager.el --- Manage IWD via the D-Bus interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025 by Sergey Trofimov
;; SPDX-License-Identifier: Unlicense

;; Author: Sergey Trofimov <sarg@sarg.org.ru>
;; Version: 0.3
;; URL: https://github.com/sarg/wpa-manager.el
;; Package-Requires: ((emacs "26.1") (promise "1.1"))

;;; Commentary:
;; A dbus-based client for iNet Wireless Daemon. Supports connecting to PSK networks.
;;
;; To display connection state in mode-line, call `iwd-manager-register-agent' once and
;; add `iwd-manager-mode-line-string' is to `global-mode-string'.
;; Customize `iwd-manager-mode-line-formatter' to change how it looks like.

;;; Code:
(require 'dbus)
(require 'promise)
(require 'notifications)
(require 'cl-lib)

(defgroup iwd-manager nil
  "IWD manager."
  :prefix "iwd-manager-"
  :group 'applications)

(defcustom iwd-manager-mode-line-formatter #'iwd-manager--format-mode-line-default
  "Function that returns IWD status text for `global-mode-string'."
  :type 'function
  :group 'iwd-manager)

(defconst iwd-manager--buffer-name "*IWD Manager*")
(defconst iwd-manager--service "net.connman.iwd")
(defconst iwd-manager--path "/net/connman/iwd")
(defconst iwd-manager--agent-path "/iwd/agent")
(defconst iwd-manager--agent-manager-interface "net.connman.iwd.AgentManager")
(defconst iwd-manager--device-interface "net.connman.iwd.Device")
(defconst iwd-manager--adapter-interface "net.connman.iwd.Adapter")
(defconst iwd-manager--station-interface "net.connman.iwd.Station")
(defconst iwd-manager--network-interface "net.connman.iwd.Network")

(defvar iwd-manager-cached-objects nil
  "Cache for D-Bus managed objects to avoid repeated calls.")

(defvar iwd-manager-device nil
  "Wireless interface to manage.")

(defvar iwd-manager-device-state '(:state unknown)
  "State of the managed device.")

(defvar iwd-manager--registered-objects '()
  "Dbus objects owned by IWD manager.")

(defsubst iwd-manager--interface (obj iface)
  "Return IFACE of OBJ."
  (cadr (assoc-string iface (cadr obj))))

(defsubst iwd-manager--find-obj (path)
  "Find cached dbus object for given PATH."
  (assoc-string path iwd-manager-cached-objects))

(defun iwd-manager--select-device ()
  "Select device to manage."
  (interactive)
  (or
   (seq-find
    (lambda (obj) (iwd-manager--interface obj iwd-manager--device-interface))
    iwd-manager-cached-objects)
   (user-error "No devices found")))

(defun iwd-manager--list-networks ()
  "List last-scanned access-points."
  (cl-loop
   with ordered =
   (unless (eq 'off (plist-get iwd-manager-device-state :state))
     (dbus-call-method
      :system iwd-manager--service (car iwd-manager-device) iwd-manager--station-interface
      "GetOrderedNetworks"))
   for net in ordered
   collect (let* ((obj (iwd-manager--find-obj (car net)))
                  (props (iwd-manager--interface obj iwd-manager--network-interface))
                  (signal (/ (cadr net) 100))
                  (ssid (cdr (assoc-string "Name" props)))
                  (connected (cdr (assoc-string "Connected" props))))
             (list obj
                   (vector
                    (propertize (or ssid "<Hidden>")
                                'face (cond
                                       (connected 'bold)
                                       ((not ssid) 'shadow)))
                    (format "%d dBm" signal))))))

(defun iwd-manager-scan ()
  "Start scanning for access points."
  (interactive nil iwd-manager-mode)
  (dbus-call-method
   :system iwd-manager--service
   (car iwd-manager-device) iwd-manager--station-interface
   "Scan")
  (message "Scanning for networks..."))

(defun iwd-manager--request-passphrase (network-path)
  "Request passphrase for network identified by NETWORK-PATH."
  (when-let* ((obj (iwd-manager--find-obj network-path))
              (props (iwd-manager--interface obj iwd-manager--network-interface))
              (ssid (cdr (assoc-string "Name" props))))
    (condition-case err
        (read-passwd (format "Passphrase for %s: " ssid))
      (quit (list :error "net.connman.iwd.Agent.Error.Canceled"
                  (error-message-string err))))))

(defvar-local iwd-manager--refresh-timer (timer-create))
(put 'iwd-manager--refersh-timer 'permanent-local t)

(defvar iwd-manager-state-changed-hook
  (list
   #'iwd-manager--fetch-state
   #'iwd-manager--refresh-buffer
   #'iwd-manager--refresh-mode-line)
  "Functions to run when IWD state changes.")

(defun iwd-manager--refresh-buffer ()
  "Refresh IWD manager buffer."
  (when-let* ((buffer (get-buffer iwd-manager--buffer-name))
              (buffer-live-p buffer))
    (with-current-buffer buffer
      (revert-buffer))))

(defun iwd-manager--fetch-state ()
  "Fetch current IWD state via DBUS."
  (setq iwd-manager-cached-objects
        (dbus-get-all-managed-objects :system iwd-manager--service "/"))
  (iwd-manager--update-device-state))

;;;###autoload
(defun iwd-manager-register-agent ()
  "Register the agent with IWD."
  (unless iwd-manager--registered-objects
    (dbus-ping :system iwd-manager--service 5000)

    (add-to-list
     'iwd-manager--registered-objects
     (dbus-register-method
      :system nil iwd-manager--agent-path "net.connman.iwd.Agent"
      "RequestPassphrase" #'iwd-manager--request-passphrase
      'dont-register))

    (dbus-call-method
     :system iwd-manager--service iwd-manager--path iwd-manager--agent-manager-interface
     "RegisterAgent" :object-path iwd-manager--agent-path)

    (dolist (i `(("/" ,dbus-interface-objectmanager "InterfacesAdded")
                 ("/" ,dbus-interface-objectmanager "InterfacesRemoved")
                 (nil ,dbus-interface-properties "PropertiesChanged")))
      (add-to-list
       'iwd-manager--registered-objects
       (dbus-register-signal
        :system iwd-manager--service
        (nth 0 i) (nth 1 i) (nth 2 i)
        (lambda (&rest _)
          (ignore-errors (cancel-timer iwd-manager--refresh-timer))
          (setq iwd-manager--refresh-timer
                (run-with-timer
                 0.2 nil
                 (lambda ()
                   (run-hooks 'iwd-manager-state-changed-hook))))))))

    (run-hooks 'iwd-manager-state-changed-hook)))

(defun iwd-manager--cleanup ()
  "Wind down iwd-manager."
  (ignore-errors (cancel-timer iwd-manager--refresh-timer))

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
            (props (iwd-manager--interface obj iwd-manager--network-interface))
            ((cdr (assoc-string "Connected" props)))
            (net (cdr (assoc-string "KnownNetwork" props))))
      (dbus-call-method
       :system iwd-manager--service net "net.connman.iwd.KnownNetwork"
       "Forget")
    (user-error "Not a known network")))

(defun iwd-manager-disconnect ()
  "Disconnect from current access point."
  (interactive nil iwd-manager-mode)
  (dbus-call-method
   :system iwd-manager--service (car iwd-manager-device) iwd-manager--station-interface
   "Disconnect"))

(defun iwd-manager-connect ()
  "Connect to the currently selected access point."
  (interactive nil iwd-manager-mode)
  (let* ((obj (tabulated-list-get-id))
         (net-props (iwd-manager--interface obj iwd-manager--network-interface))
         (ssid (cdr (assoc-string "Name" net-props)))
         (network-path (car obj)))
    (promise-chain
        (promise-new
         (lambda (resolve reject)
           (condition-case err
               (progn
                 (dbus-call-method
                  :system iwd-manager--service network-path iwd-manager--network-interface
                  "Connect")
                 (funcall resolve t))
             (dbus-error (funcall reject (cdr err))))))
      (then (lambda (_)
              (notifications-notify
               :title (concat "IWD: " ssid)
               :body "Connected")))
      (catch (lambda (err)
               (notifications-notify
                :title (concat "IWD: " ssid)
                :body (cadr err)))))))

(define-derived-mode iwd-manager-mode tabulated-list-mode
  "IWD"
  "Major mode for managing IWD."
  :interactive nil

  (setq tabulated-list-format [("SSID" 24 t) ("Signal" 6 t)]
        tabulated-list-entries #'iwd-manager--list-networks
        tabulated-list-padding 0)

  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode))

(defun iwd-manager-toggle-power ()
  "Toggle adapter power."
  (interactive)
  (let* ((adapter
          (thread-last
            (iwd-manager--interface iwd-manager-device iwd-manager--device-interface)
            (assoc-string "Adapter")
            (cdr)
            (iwd-manager--find-obj)))
         (props (iwd-manager--interface adapter iwd-manager--adapter-interface)))
    (dbus-set-property
     :system iwd-manager--service
     (car adapter)
     "net.connman.iwd.Adapter"
     "Powered" (not (cdr (assoc-string "Powered" props))))))

(defvar iwd-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map [?s] #'iwd-manager-scan)
    (define-key map [?p] #'iwd-manager-toggle-power)
    (define-key map [?c] #'iwd-manager-connect)
    (define-key map [?d] #'iwd-manager-disconnect)
    (define-key map [?D] #'iwd-manager-delete-network)
    map))

(defvar iwd-manager-mode-line-string nil
  "Indicator of connection state for mode-line.")
(put 'iwd-manager-mode-line-string 'risky-local-variable t)

(defun iwd-manager--update-device-state ()
  "Update device state."
  (setq iwd-manager-device
        (if iwd-manager-device
            (iwd-manager--find-obj (car iwd-manager-device))
          (iwd-manager--select-device)))

  (let* ((device-props (iwd-manager--interface iwd-manager-device iwd-manager--device-interface))
         (station-props (iwd-manager--interface iwd-manager-device iwd-manager--station-interface))
         (scanning? (and (cdr (assoc-string "Scanning" station-props)) t))
         (powered? (cdr (assoc-string "Powered" device-props)))
         (state (if powered? (intern (cdr (assoc-string "State" station-props))) 'off))
         (connected-network
          (and (eq state 'connected)
               (thread-first
                 (cdr (assoc-string "ConnectedNetwork" station-props))
                 (iwd-manager--find-obj)
                 (iwd-manager--interface iwd-manager--network-interface)))))
    (setq iwd-manager-device-state
          (list
           :name (cdr (assoc-string "Name" device-props))
           :state state
           :ssid (cdr (assoc-string "Name" connected-network))
           :scanning scanning?))))

(cl-defun iwd-manager--format-mode-line-default (&key state ssid scanning &allow-other-keys)
  "Default mode-line formatter.
Shows STATE and SCANNING as an icon followed by SSID."
  (concat " "
          (cond
           (scanning "🔁")
           ((eq state 'connected) "🛜")
           (t "❌"))
          (if ssid (concat " " ssid) nil)))

(defun iwd-manager--refresh-mode-line ()
  "Update global mode-line string with current device state."
  (setq iwd-manager-mode-line-string
        (apply iwd-manager-mode-line-formatter iwd-manager-device-state))
  (force-mode-line-update 'all))

(defvar iwd-manager--mode-info
  '(:eval (iwd-manager--mode-info)))
(put 'iwd-manager--mode-info 'risky-local-variable t)

(defun iwd-manager--mode-info ()
  "Update the mode info display."
  (cl-destructuring-bind (&key name state scanning &allow-other-keys)
      iwd-manager-device-state
    (format " [%s %s%s]" name state (if scanning ", scanning" ""))))

;;;###autoload
(defun iwd-manager ()
  "Manage wifi connections through IWD."
  (interactive)
  (iwd-manager-register-agent)
  (with-current-buffer (switch-to-buffer iwd-manager--buffer-name)
    (iwd-manager-mode)
    (cl-pushnew iwd-manager--mode-info mode-line-process))
  nil)

(provide 'iwd-manager)
;;; iwd-manager.el ends here
