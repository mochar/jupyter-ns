;; -*- lexical-binding: t -*-

;;; Requires

(require 'cl-lib)
(require 'subr-x)
(require 'jupyter)

;;; Variables

(defvar-local jupyter-ns-space nil
  "Name of the current active namespace.")

(defvar-local jupyter-ns-spaces nil
  "List of the name of the available namespaces.")

(defvar-local jupyter-ns-comm-id nil
  "Comm id of the jupyter_ns plugin.")

;;; Utils

(defun jupyter-ns--repl-clients ()
  "Return all jupyter client objects with alive REPL buffer."
  (cl-loop for client in (jupyter-all-objects 'jupyter--clients)
           when (and (object-of-class-p client 'jupyter-repl-client)
                     (buffer-live-p (oref client buffer)))
           collect client))

;;; Eval

;; NOTE Don't really use these anymore..

(defun jupyter-ns-eval (code)
  "Eval CODE and make clean up result."
  (when-let* ((result (jupyter-eval code))
              ;; Cleanup Jupyter's text/plain format wraps Python strings in
              ;; single quotes (e.g., '["key1", "key2"]'). We must trim the
              ;; surrounding quotes and fix any escapes.
              (result (replace-regexp-in-string 
                       "\\\\'" "'" 
                       (string-trim result "^'" "'$"))))
    result))

(defun jupyter-ns-eval-load-extension ()
  (jupyter-eval "%load_ext jupyter_ns"))

(defun jupyter-ns-eval-loaded-p ()
  (let* ((result (jupyter-ns-eval "from IPython import get_ipython; 'jupyter_ns' in get_ipython().extension_manager.loaded")))
    (string= result "True")))

(defun jupyter-ns-eval-spaces ()
  "Return list of available namespaces."
  (let* ((code "import json; json.dumps(list(__spaces__.keys()))")
         (result (jupyter-ns-eval code))
         (json-array-type 'list))
    (json-read-from-string result)))

(defun jupyter-ns-eval-space ()
  "Return name of active namespace."
  (jupyter-ns-eval "__space__"))

;;; Comm

(defun jupyter-ns-comm-id ()
  "Request comm info and parse comm id as string."
  (jupyter-run-with-client jupyter-current-client
    (jupyter-mlet* ((reply (jupyter-reply
                            (jupyter-comm-info-request
                             :target-name "jupyter_ns"
                             :handlers nil))))
      (if-let ((comms (jupyter-message-get reply :comms)))
          (thread-first comms
                        car
                        symbol-name
                        (substring 1)
                        jupyter-return)
        (jupyter-return nil)))))

(defun jupyter-ns--comm-send-msg (data &optional handlers)
  "Send DATA as message data to the jupyter_ns comm. Return message data."
  (jupyter-run-with-client jupyter-current-client
    (jupyter-mlet*
        ((msgs (jupyter-messages 
                (jupyter-comm-msg :id jupyter-ns-comm-id
                                  :data data
                                  :handlers t))))
      (thread-first
        (jupyter-find-message "comm_msg" msgs)
        (jupyter-message-get :data)
        jupyter-return))))

(defun jupyter-ns-state ()
  "Request and return current state."
  (let ((data (jupyter-ns--comm-send-msg '())))
    (map-elt data :state)))

(defun jupyter-ns-send-command (command &optional ns)
  (jupyter-ns--comm-send-msg (list :command command :ns ns) t))

(defun jupyter-ns-update-state (&optional state client)
  (setq state (or state (jupyter-ns-state)))
  (setq client (or client jupyter-current-client))
  (when (and client state)
    (with-current-buffer (oref client buffer)
      (setq-local jupyter-ns-space (plist-get state :active))
      (setq-local jupyter-ns-spaces (seq-into (plist-get state :all) 'list))
      nil)))
    
(defun jupyter-ns-comm-watcher (client msg)
  "Listen for incoming comm_msg broadcasts from jupyter_ns."
  (let ((msg-type (jupyter-message-type msg))
        (content  (jupyter-message-content msg)))
    (if (not (and (string= msg-type "comm_msg")
                  (string= (plist-get content :comm_id)
                           (buffer-local-value 'jupyter-ns-comm-id (oref client buffer)))))
        nil ; Return nil, run other handlers
      
      (map-let (:source :event :ns :state) (plist-get content :data)
        (jupyter-ns-update-state state client)
        (pcase event
          ("init"
           ;; We initialize too late to recieve this msg, so we handle it in
           ;; the setup code.
           nil)
          ("new"
           nil)
          ("switch"
           nil)
          ("kill"
           nil)))
      t)))

;;; Minor mode

(defun jupyter-ns-setup (&optional client)
  (setq client (or client jupyter-current-client))
  (jupyter-add-hook client 'jupyter-iopub-message-hook #'jupyter-ns-comm-watcher)
  (with-current-buffer (oref client buffer)
    (when (setq-local jupyter-ns-comm-id (jupyter-ns-comm-id))
      (jupyter-ns-update-state nil client))))

(defun jupyter-ns-cleanup (&optional client)
  (setq client (or client jupyter-current-client))
  (jupyter-remove-hook client 'jupyter-iopub-message-hook #'jupyter-ns-comm-watcher)
  ;; TODO Is this cleanup necessary??
  (with-current-buffer (oref client buffer)
    (setq-local jupyter-ns-comm-id nil)
    (setq-local jupyter-ns-space nil)
    (setq-local jupyter-ns-spaces nil)))

(defun jupyter-ns-handle-restart (orig-fun &optional client)
  (let* ((client (or client (jupyter-repl--get-client))))
    (jupyter-ns-cleanup client)
    (funcall orig-fun client)
    (jupyter-ns-setup client)))

;;;###autoload
(define-minor-mode jupyter-ns-mode
  "Minor mode to track jupyter-ns namespaces."
  ;; :group 'jupyter-ns
  :global t
  (cond
   (jupyter-ns-mode
    (add-hook 'jupyter-repl-mode-hook #'jupyter-ns-setup)
    (advice-add 'jupyter-repl-restart-kernel :around #'jupyter-ns-handle-restart)
    (dolist (client (jupyter-ns--repl-clients))
      (jupyter-ns-setup client))
    )
   (t
    (remove-hook 'jupyter-repl-mode-hook #'jupyter-ns-setup)
    (advice-remove 'jupyter-repl-restart-kernel #'jupyter-ns-handle-restart)
    (dolist (client (jupyter-ns--repl-clients))
      (jupyter-ns-cleanup client))
    )))

;;; Commands

;;;###autoload
(defun jupyter-ns-switch ()
  (interactive)
  (let* ((space (completing-read "Switch space: " jupyter-ns-spaces)))
    (jupyter-ns-send-command "switch" space)))

;;;###autoload
(defun jupyter-ns-kill ()
  (interactive)
  (let* ((space (completing-read "Kill space: " jupyter-ns-spaces)))
    (when (yes-or-no-p (format "Kill space \'%s\'?" space))
      (jupyter-ns-send-command "kill" space))))

;;; Footer

(provide 'jupyter-ns)

