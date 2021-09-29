(require 'cl-lib)

(defvar signel-cli-user "+447507736899")
(defvar signel-contact-names '(("+7854766547" . "q")
                               ("+346xxxxxxxx" . "anna")))

(defun signel--contact-name (src)
  (or (alist-get src signel-contact-names nil nil #'string-equal) src))

(defvar signel-cli-exec "signal-cli")

(defun signel--proc-buffer ()
  (get-buffer-create "*signal-cli*"))

(defun signel-signal-cli-buffer ()
  (get-buffer "*signal-cli*"))

(defun signel-signal-cli-process ()
  (when-let ((proc (get-buffer-process (signel-signal-cli-buffer))))
    (and (process-live-p proc) proc)))

(defun signel-start ()
  "Start the underlying signal-cli process if needed."
  (interactive)
  (if (signel-signal-cli-process)
      (message "signal-cli is already running!")
    (let ((b (signel--proc-buffer)))
      (make-process :name "signal-cli"
                    :buffer b
                    :command `(,signel-cli-exec
                               "-u"
                               ,signel-cli-user
                               "daemon" "--json")
                    :filter #'signel--filter)
      (message "Listening to signals!"))))

(defun signel--parse-json (str)
  (if (> emacs-major-version 26)
      (json-parse-string str
                         :null-object nil
                         :false-object nil
                         :object-type 'alist
                         :array-type 'list)
    (json-read-from-string str)))

(defun signel--msg-contents (str)
  (alist-get 'envelope (ignore-errors (signel--parse-json str))))

(defun signel--msg-source (msg) (alist-get 'source msg))

(defun signel--msg-data (msg)
  (alist-get 'message (alist-get 'dataMessage msg)))

(defun signel--msg-timestamp (msg)
  (if-let (msecs (alist-get 'timestamp msg))
      (format-time-string "%H:%M" (/ msecs 1000))
    ""))

;; emacs 26 compat
(defun signel--not-false (x)
  (and (not (eq :json-false x)) x))

(defun signel--msg-receipt (msg)
  (alist-get 'receiptMessage msg))

(defun signel--msg-is-receipt (msg)
  (signel--not-false (alist-get 'isReceipt msg)))

(defun signel--msg-receipt-timestamp (msg)
  (when-let (msecs (alist-get 'when (signel--msg-receipt msg)))
    (format-time-string "%H:%M" (/ msecs 1000))))

(defun signel--msg-is-delivery (msg)
  (when-let ((receipt (signel--msg-receipt msg)))
    (signel--not-false (alist-get 'isDelivery msg))))

(defun signel--msg-is-read (msg)
  (when-let ((receipt (signel--msg-receipt msg)))
    (signel--not-false (alist-get 'isRead msg))))

(defvar signel--line-buffer "")

(defun signel--filter (proc str)
  (signel--ordinary-insertion-filter proc str)
  (let ((str (concat signel--line-buffer str)))
    (if-let ((msg (signel--msg-contents str)))
        (let ((source (signel--msg-source msg))
              (stamp (signel--msg-timestamp msg))
              (data (signel--msg-data msg))
              (rec-stamp (signel--msg-receipt-timestamp msg)))
          (setq signel--line-buffer "")
          (when source
            (signel--update-chat-buffer source data stamp rec-stamp msg)))
      (setq signel--line-buffer
            (if (string-match-p ".*\n$" str) "" str)))))

(defun signel--ordinary-insertion-filter (proc string)
  (when (and proc (buffer-live-p (process-buffer proc)))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defvar-local signel-user nil)

(defun signel--contact-buffer (source)
  (let* ((name (format "*%s" (signel--contact-name source)))
         (buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
        (signel-chat-mode)
        (setq-local signel-user source)
        (insert signel-prompt)))
    buffer))

(defvar signel-prompt ": ")

(define-derived-mode signel-chat-mode fundamental-mode "Signal"
  "Major mode for Signal chats."
  (when (boundp 'next-line-add-newlines)
    (set (make-local-variable 'next-line-add-newlines) nil))
  (setq line-move-ignore-invisible t)
  (set (make-local-variable 'paragraph-separate)
       (concat "\C-l\\|\\(^" (regexp-quote signel-prompt) "\\)"))
  (set (make-local-variable 'paragraph-start)
       (concat "\\(" (regexp-quote signel-prompt) "\\)"))
  (setq-local completion-ignore-case t))

(defgroup signel nil "Signel")

(defface signel-contact '((t :weight bold))
  "Face for contact names."
  :group 'signel)

(defface signel-timestamp '((t :foreground "grey70"))
  "Face for timestamp names."
  :group 'signel)

(defface signel-notice '((t :inherit signel-timestamp))
  "Face for delivery notices."
  :group 'signel)

(defface signel-prompt '((t :weight bold))
  "Face for the input prompt marker."
  :group 'signel)

(defface signel-user '((t :foreground "orangered"))
  "Face for sent messages."
  :group 'signel)

(defface signel-notification '((t :foreground "burlywood"))
  "Face for notifications shown by tracking, when available."
  :group 'signel)

(defun signel--contact (name)
  (propertize name 'face 'signel-contact))

(defun signel--timestamp (&rest p)
  (propertize (apply #'concat p) 'face 'signel-timestamp))

(defun signel--notice (notice)
  (propertize notice 'face 'signel-notice))

(defun signel--insert-prompt ()
  (let ((inhibit-read-only t)
        (p (point)))
    (insert signel-prompt)
    (set-text-properties p (- (point) 1)
                         '(face signel-prompt
                           read-only t front-sticky t rear-sticky t))))

(defun signel--delete-prompt ()
  (when (looking-at-p (regexp-quote signel-prompt))
    (let ((inhibit-read-only t))
      (delete-char (length signel-prompt)))))

(defun signel--delete-last-prompt ()
  (goto-char (point-max))
  (when (re-search-backward (concat "^" (regexp-quote signel-prompt)))
    (signel--delete-prompt)))

(defcustom signel-report-deliveries nil
  "Whether to show message delivery notices."
  :group 'signel
  :type 'boolean)

(defcustom signel-report-read t
  "Whether to show message read notices."
  :group 'signel
  :type 'boolean)

(defun signel--prompt-and-notify ()
  (signel--insert-prompt)
  (when (fboundp 'tracking-add-buffer)
    (tracking-add-buffer (current-buffer) '(signel-notification))))

(defun signel--needs-insert-p (data stamp rec-stamp msg)
  (or data
      (and (or rec-stamp stamp)
           (not (string= source signel-cli-user))
           (or signel-report-deliveries
               (and signel-report-read (signel--msg-is-read msg))))))

(defun signel--update-chat-buffer (source data stamp rec-stamp msg)
  (when (signel--needs-insert-p data stamp rec-stamp msg)
    (when-let ((b (signel--contact-buffer source)))
      (with-current-buffer b
        (signel--delete-last-prompt)
        (if data
            (let ((p (point)))
              (insert (signel--timestamp "[" stamp "] ")
                      (signel--contact (signel--contact-name source))
                      signel-prompt
                      data
                      "\n")
              (fill-region p (point)))
          (let ((is-read (signel--msg-is-read msg)))
            (insert (signel--timestamp "*" (or rec-stamp stamp) "* ")
                    (signel--notice (if is-read "(read)" "(delivered)"))
                    "\n")))
        (signel--prompt-and-notify)
        (end-of-line)))))

(defun signel--send-message (user msg)
  (dbus-call-method :session "org.asamk.Signal" "/org/asamk/Signal"
                    "org.asamk.Signal" "sendMessage"
                    :string msg
                    '(:array)
                    :string user))

(defun signel-send ()
  "Read text inserted in the current buffer after the last prompt and send it.

The recipient of the message is looked up in a local variable set
when the buffer was created."
  (interactive)
  (goto-char (point-max))
  (beginning-of-line)
  (let* ((p (point))
         (plen (length signel-prompt))
         (msg (buffer-substring (+ p plen) (point-max))))
    (signel--delete-prompt)
    (signel--send-message signel-user msg)
    (insert (signel--timestamp (format-time-string "(%H:%M) ")))
    (fill-region p (point-max))
    (goto-char (point-max))
    (set-text-properties p (point) '(face signel-user))
    (insert "\n")
    (signel--insert-prompt)))

(define-key signel-chat-mode-map "\C-m" #'signel-send)

(defun signel-query (contact)
  "Start a conversation with a signal contact."
  (interactive (list (completing-read "Signal to: "
                                      (mapcar #'cdr-safe signel-contact-names))))
  (let ((phone (alist-get contact
                          (cl-pairlis (mapcar #'cdr signel-contact-names)
                                      (mapcar #'car signel-contact-names))
                          nil nil #'string-equal)))
    (when (not phone)
      (error "Unknown contact %s" contact))
    (pop-to-buffer (signel--contact-buffer phone))))
