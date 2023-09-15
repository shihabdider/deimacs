;;; deimacs.el --- Use Martian inside Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Use Martian inside Emacs
;;

;;; Code:

(require 'cl-lib)
(require 'let-alist)
(require 'subr-x)

(require 'openai)
(require 'lv)
(require 'ht)
(require 'markdown-mode)
(require 'spinner)

(defgroup deimacs nil
  "Use Martian inside Emacs."
  :prefix "deimacs-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/shihabdider/deimacs"))

(defcustom deimacs-model "gpt-3.5-turbo"
  "Model to use for chat."
  :type 'string
  :group 'deimacs)

(defcustom deimacs-max-tokens 2000
  "The maximum number of tokens to generate in the completion."
  :type 'integer
  :group 'deimacs)

(defcustom deimacs-temperature 1.0
  "What sampling temperature to use."
  :type 'number
  :group 'deimacs)

(defcustom deimacs-top-p 1.0
  "Nucleus sampling parameter."
  :type 'number
  :group 'deimacs)

(defcustom deimacs-input-method 'window
  "The method receive input."
  :type '(choice (const :tag "Read from minibuffer" minibuffer)
                 (const :tag "Read inside new window" window))
  :group 'deimacs)

(defcustom deimacs-window-prompt "Type response here..."
  "Text inserted when window is created."
  :type 'string
  :group 'deimacs)

(defcustom deimacs-inhibit-input-afterward t
  "Stop input after sending one."
  :type 'boolean
  :group 'deimacs)

(defcustom deimacs-spinner-type 'moon
  "The type of the spinner."
  :type '(choice (const :tag "Key to variable `spinner-types'" symbol)
                 (const :tag "Vector of characters" vector))
  :group 'openai)

(defcustom deimacs-display-tokens-info t
  "Non-nil we display tokens information for each request."
  :type 'boolean
  :group 'deimacs)

(defcustom deimacs-priority 100
  "Overlays' priority."
  :type 'integer
  :group 'deimacs)

(defcustom deimacs-animate-text t
  "Display text gradually instead of output it all at once."
  :type 'boolean
  :group 'deimacs)

(defcustom deimacs-animate-fps 5
  "Frame per seconds to display text animation."
  :type 'integer
  :group 'deimacs)

(defconst deimacs-buffer-name-format "*deimacs: <%s>*"
  "Name of the buffer to use for the `deimacs' instance.")

(defvar deimacs-instances (ht-create)
  "List of instances, each pair is consist of (index . buffer).")

(defvar-local deimacs-instance nil
  "Instance data for each buffer.")

(defvar-local deimacs-chat-history nil
  "The chat history use to send request.")

(defvar-local deimacs-requesting-p nil
  "Non-nil when requesting; waiting for the response.")

(defvar-local deimacs-spinner nil
  "Spinner.")

(defvar-local deimacs-data (ht-create)
  "Store other information other than messages.")

(defvar-local deimacs--display-pointer 0
  "Display pointer for each message to display.")

(defvar-local deimacs--text-pointer 1
  "Text pointer for text animation.")

(defvar-local deimacs-text-timer nil
  "Text timer for text animation.")

(defvar-local deimacs-animating-p nil
  "Non-nil when animating text scroll.")

(defface deimacs-user
  '((t :inherit font-lock-builtin-face))
  "Face used for user."
  :group 'deimacs)

(defface deimacs-tip
  '((t :foreground "#848484"))
  "Face used for tip."
  :group 'deimacs)

(defface deimacs-info
  '((t :height 0.8 :foreground "#999999"))
  "Face added to codemetrics display."
  :group 'deimacs)

;;
;;; Externals

(declare-function string-pixel-width "ext:subr-x.el")
(declare-function shr-string-pixel-width "ext:shr.el")

;;
;;; Util

(defmacro deimacs--with-no-redisplay (&rest body)
  "Execute BODY without any redisplay execution."
  (declare (indent 0) (debug t))
  `(let ((inhibit-redisplay t)
         (inhibit-modification-hooks t)
         after-focus-change-function
         buffer-list-update-hook
         display-buffer-alist
         window-configuration-change-hook
         window-scroll-functions
         window-size-change-functions
         window-state-change-hook)
     ,@body))

;; TODO: Use function `string-pixel-width' after 29.1
(defun deimacs--string-pixel-width (str)
  "Return the width of STR in pixels."
  (if (fboundp #'string-pixel-width)
      (string-pixel-width str)
    (require 'shr)
    (shr-string-pixel-width str)))

(defun deimacs--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (deimacs--string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defun deimacs--align (&rest lengths)
  "Align sideline string by LENGTHS from the right of the window."
  (list (* (window-font-width)
           (+ (apply #'+ lengths) (if (display-graphic-p) 1 2)))))

(defun deimacs--kill-buffer (buffer-or-name)
  "Like function `kill-buffer' (BUFFER-OR-NAME) but in the safe way."
  (when-let ((buffer (get-buffer buffer-or-name)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun deimacs--cancel-timer (timer)
  "Cancel TIMER."
  (when (timerp timer)
    (cancel-timer timer)))

(defun deimacs--pop-to-buffer (buffer-or-name)
  "Wrapper to function `pop-to-buffer'.

Display buffer from BUFFER-OR-NAME."
  (pop-to-buffer buffer-or-name `((display-buffer-in-direction)
                                  (dedicated . t))))

(defun deimacs-busy-p ()
  "Return non-nil if session is busy."
  (or deimacs-requesting-p deimacs-animating-p))

(defun deimacs-user ()
  "Return the current user."
  (if (string-empty-p openai-user)
      "user"  ; this is free?
    openai-user))

;;
;;; Instances

(defmacro deimacs-with-instance (instance &rest body)
  "Execute BODY within INSTANCE."
  (declare (indent 1))
  `(when-let* ((buffer (and ,instance
                            (get-buffer (cdr ,instance))))
               ((buffer-live-p buffer)))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         ,@body))))

(defun deimacs--live-instances ()
  "Return a list of live instances."
  (let ((live-instances))
    (ht-map (lambda (_ buffer)
              (when (and (get-buffer buffer)
                         (buffer-live-p buffer))
                (push buffer live-instances)))
            deimacs-instances)
    (reverse live-instances)))

(defun deimacs--shown-instances ()
  "Return a list of live instances that are displayed on the screen."
  (let ((live-instances (deimacs--live-instances))
        (shown-instances))
    (dolist (instance live-instances)
      (when (get-buffer-window instance)
        (push instance shown-instances)))
    (reverse shown-instances)))

(defun deimacs--new-index ()
  "Find killed instance before giving new index."
  (let ((target))
    (cl-some (lambda (index)
               (let ((buffer (ht-get deimacs-instances index)))
                 (when (or (not (get-buffer buffer))
                           (not (buffer-live-p buffer)))  ; if buffer is killed
                   (setq target index)
                   t)))
             (ht-keys deimacs-instances))
    (unless target                                ; No killed instance?
      (setq target (ht-size deimacs-instances)))  ; Create a new one!
    target))

(defun deimacs-restart ()
  "Restart session."
  (interactive)
  (when (eq major-mode #'deimacs-mode)
    (let* ((instance deimacs-instance)
           (index    (car instance))
           (old-name))
      ;; If buffer is alive, kill it!
      (deimacs-with-instance instance
        (setq old-name (buffer-name))
        (kill-this-buffer))
      ;; `old-name' will remain `nil' if buffer is not killed or invalid!
      (when old-name
        (deimacs-register-instance index old-name)
        (switch-to-buffer old-name)))))

;;
;;; Core

(defun deimacs--get-face-height ()
  "Make sure we get the face height."
  (let ((height (face-attribute 'deimacs-info :height)))
    (if (numberp height) height
      1)))

(defun deimacs--create-tokens-overlay (prompt-tokens completion-tokens total-tokens)
  "Display tokens information.

Arguments PROMPT-TOKENS, COMPLETION-TOKENS, and TOTAL-TOKENS are the tokens
information we want to display."
  (when deimacs-display-tokens-info
    (let* ((ov (make-overlay (1- (point)) (1- (point)) nil t t))
           (content (format "prompt %s, completion %s, total: %s"
                            prompt-tokens completion-tokens total-tokens))
           (content-len (* (deimacs--str-len content)
                           (deimacs--get-face-height)))
           (str (concat
                 (propertize " " 'display
                             `((space :align-to (- right ,(deimacs--align (1- content-len))))
                               (space :width 0))
                             `cursor t)
                 (propertize content 'face 'deimacs-info))))
      (overlay-put ov 'deimacs t)
      (overlay-put ov 'priority deimacs-priority)
      (overlay-put ov 'after-string str))))

(defun deimacs--add-tokens (data)
  "Record all tokens information from DATA."
  (let-alist data
    (let-alist .usage
      ;; First we get the current tokens,
      (let ((prompt-tokens     (ht-get deimacs-data 'prompt_tokens 0))
            (completion-tokens (ht-get deimacs-data 'completion_tokens 0))
            (total-tokens      (ht-get deimacs-data 'total_tokens 0))
            (tokens-history    (ht-get deimacs-data 'tokens_history nil)))
        ;; Then we add it up!
        (ht-set deimacs-data 'prompt_tokens     (+ prompt-tokens     .prompt_tokens))
        (ht-set deimacs-data 'completion_tokens (+ completion-tokens .completion_tokens))
        (ht-set deimacs-data 'total_tokens      (+ total-tokens      .total_tokens))
        (ht-set deimacs-data 'tokens_history
                (append tokens-history
                        `((,.prompt_tokens ,.completion_tokens ,.total_tokens))))
        ;; Render it!
        (unless deimacs-animate-text
          (deimacs--create-tokens-overlay .prompt_tokens .completion_tokens .total_tokens))))))

(defun deimacs--add-message (role content)
  "Add a message to history.

The data is consist of ROLE and CONTENT."
  (setq deimacs-chat-history
        (vconcat (or deimacs-chat-history '[])
                 `[((role    . ,role)
                    (content . ,(string-trim content)))])))

(defun deimacs--add-response-messages (data)
  "Add the message to history from DATA, and return the message itself."
  (let-alist data
    (mapc (lambda (choice)
            (let-alist choice
              (let-alist .message
                (deimacs--add-message .role .content))))
          .choices)))

;;
;;; Display

(defun deimacs--render-markdown (content)
  "Render CONTENT in markdown."
  (if (featurep 'markdown-mode)
      (with-temp-buffer
        (insert content)
        (delay-mode-hooks (markdown-mode))
        (ignore-errors (font-lock-ensure))
        (buffer-string))
    content))

(defun deimacs--fill-region (start end)
  "Like function `fill-region' (START to END), improve readability."
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (when (< fill-column (current-column))
        (fill-region (line-beginning-position) (line-end-position)))
      (forward-line 1))))

(defun deimacs--cancel-text-timer ()
  "Cancel text timer."
  (deimacs--cancel-timer deimacs-text-timer)
  (setq deimacs-text-timer nil
        deimacs-animating-p nil))

(defun deimacs--start-text-timer ()
  "Start text timer."
  (deimacs--cancel-text-timer)
  (setq deimacs-text-timer (run-with-timer (/ deimacs-animate-fps 60.0)
                                           nil
                                           #'deimacs--do-text-animatioin
                                           deimacs-instance)
        deimacs-animating-p t))

(defun deimacs--do-text-animatioin (instance)
  "The main loop for text animation for the INSTANCE."
  (deimacs-with-instance instance
    (deimacs--cancel-text-timer)
    (let ((message (elt deimacs-chat-history deimacs--display-pointer)))
      (let-alist message
        (goto-char (point-max))
        (let* ((is-user (string= (deimacs-user) .role))
               (role (format "<%s>:" .role))
               ;; XXX: If messages from user, don't try to render to markdown!
               ;; else, messages from OpenAI will most likely going to be
               ;; markdown so we render it!
               (content (if is-user .content
                          (deimacs--render-markdown .content)))
               (chunk)
               (text-pointer deimacs--text-pointer)
               (done))
          (add-face-text-property 0 (length role) 'deimacs-user nil role)
          (with-temp-buffer  ; Get the chunk!
            ;; --- Standard output ---
            (insert role " " content)
            (insert "\n\n")
            (deimacs--fill-region (point-min) (point-max))
            ;; ---
            (goto-char text-pointer)
            (unless (eobp)
              (forward-word 1)
              (setq chunk (buffer-substring text-pointer (point))
                    text-pointer (point)
                    done (eobp))))  ; update from temp buffer
          (setq deimacs--text-pointer text-pointer)  ; update for local buffer
          (insert chunk)
          ;; Ready for next message!
          (when done
            (let* ((tokens-history (ht-get deimacs-data 'tokens_history))
                   ;; XXX: Divided by 2 since the data is in pair!
                   (index (/ deimacs--display-pointer 2))
                   (history (nth index tokens-history))
                   (prompt-tokens     (nth 0 history))
                   (completion-tokens (nth 1 history))
                   (total-tokens      (nth 2 history)))
              (deimacs--create-tokens-overlay prompt-tokens
                                              completion-tokens
                                              total-tokens))
            (cl-incf deimacs--display-pointer)
            (setq deimacs--text-pointer 1)))))
    (if (< deimacs--display-pointer (length deimacs-chat-history))
        (deimacs--start-text-timer)
      (spinner-stop deimacs-spinner))))

(defun deimacs--display-messages-at-once ()
  "If variable `deimacs-animate-text' is nil, we display messages all at once."
  (while (< deimacs--display-pointer (length deimacs-chat-history))
    (let ((message (elt deimacs-chat-history deimacs--display-pointer)))
      (let-alist message
        (goto-char (point-max))
        (let* ((start (point))
               (is-user (string= (deimacs-user) .role))
               (role (format "<%s>:" .role))
               ;; XXX: If messages from user, don't try to render to markdown!
               ;; else, messages from OpenAI will most likely going to be
               ;; markdown so we render it!
               (content (if is-user .content
                          (deimacs--render-markdown .content))))
          (add-face-text-property 0 (length role) 'deimacs-user nil role)
          (insert role " " content)
          (insert "\n\n")
          (deimacs--fill-region start (point)))))
    (cl-incf deimacs--display-pointer)))

(defun deimacs--display-messages ()
  "Display all messages to latest response."
  (when (zerop deimacs--display-pointer)  ; clear up the tip message
    (erase-buffer))
  (if deimacs-animate-text
      (unless (timerp deimacs-text-timer)  ; when is already running, don't interfere it
        (spinner-start deimacs-spinner)
        (deimacs--start-text-timer))
    (deimacs--display-messages-at-once)))

(defun deimacs-send-response (response)
  "Send RESPONSE to deimacs."
  (let ((user (deimacs-user))
        (instance deimacs-instance))
    (when (string-empty-p response)
      (user-error "[INFO] Invalid response or instruction: %s" response))
    (deimacs--add-message user response)  ; add user's response
    (deimacs-with-instance instance
      (let (deimacs-animate-text)
        (deimacs--display-messages)))        ; display it
    (setq deimacs-requesting-p t)
    (spinner-start deimacs-spinner)
    (openai-chat deimacs-chat-history
                 (lambda (data)
                   (deimacs-with-instance instance
                     (setq deimacs-requesting-p nil)
                     (spinner-stop deimacs-spinner)
                     (unless openai-error
                       (deimacs--add-response-messages data)
                       (deimacs--display-messages)
                       (deimacs--add-tokens data))))
                 :model deimacs-model
                 :max-tokens deimacs-max-tokens
                 :temperature deimacs-temperature
                 :top-p deimacs-top-p
                 :user user)))

(defun deimacs-type-response ()
  "Type response to OpenAI."
  (interactive)
  (cond
   (deimacs-requesting-p
    (message "[BUSY] Waiting for OpanAI to response..."))
   (deimacs-animating-p
    (message "[BUSY] Waiting for animation to finish..."))
   (t
    (cl-case deimacs-input-method
      (`minibuffer
       (deimacs-send-response (read-string "Type response: ")))
      (`window
       (deimacs--start-input deimacs-instance))
      (t
       (user-error "Invalid input method: %s" deimacs-input-method))))))

;;
;;; Input

(defconst deimacs-input-buffer-name "*deimacs-Input*"
  "Buffer name to receive input.")

(defvar deimacs-input-instance nil
  "The current instance; there is only one instance at a time.")

(defun deimacs-input-exit ()
  "Exit the input."
  (interactive)
  (deimacs--kill-buffer deimacs-input-buffer-name))

(defun deimacs--start-input (instance)
  "Start input from INSTANCE."
  (deimacs-input-exit)
  (let ((dir (if (window-parameter nil 'window-side)
                 'bottom 'down))
        (buffer (get-buffer-create deimacs-input-buffer-name)))
    ;; XXX: Without this, the highlighting at the end wouldn't work!?
    (deimacs--with-no-redisplay
      (with-current-buffer buffer
        (deimacs-input-mode)
        (setq deimacs-input-instance instance)
        (erase-buffer)
        (insert deimacs-window-prompt)
        (call-interactively #'set-mark-command)
        (goto-char (point-min))))  ; waiting for deletion
    (pop-to-buffer buffer `((display-buffer-in-direction)
                            (direction . ,dir)
                            (dedicated . t)
                            (window-height . fit-window-to-buffer)))))

(defun deimacs-input-send ()
  "Send the input."
  (interactive)
  (cond
   ((not (eq major-mode #'deimacs-input-mode)) )  ; does nothing
   (deimacs-requesting-p
    (message "[BUSY] Waiting for OpanAI to response..."))
   ((region-active-p)
    (delete-region (region-beginning) (region-end)))
   (t
    (let ((response (buffer-string)))
      (deimacs-with-instance deimacs-input-instance
        (deimacs-send-response response))
      (erase-buffer))
    (when deimacs-inhibit-input-afterward
      (deimacs-input-exit)))))

(defun deimacs-input--post-command ()
  "Execution after input."
  (let ((max-lines (line-number-at-pos (point-max))))
    (fit-window-to-buffer)
    (enlarge-window (- max-lines (window-text-height)))))

(defun deimacs-input-header-line ()
  "The display for input header line."
  (format " [Session] %s" (cdr deimacs-input-instance)))

(defvar deimacs-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "RET") #'deimacs-input-send)
    map)
  "Keymap for `deimacs-mode'.")

(define-derived-mode deimacs-input-mode fundamental-mode "deimacs Input"
  "Major mode for `deimacs-input-mode'.

\\<deimacs-input-mode-map>"
  (setq-local header-line-format `((:eval (deimacs-input-header-line))))
  (add-hook 'post-command-hook #'deimacs-input--post-command nil t))

;;
;;; Info

(defun deimacs--pre-command-once (&rest _)
  "One time pre-command after Easky command."
  ;; XXX: We pass on to next post-command!
  (remove-hook 'pre-command-hook #'deimacs--pre-command-once)
  (add-hook 'post-command-hook #'deimacs--post-command-once))

(defun deimacs--post-command-once ()
  "One time post-command after info command."
  ;; XXX: This will allow us to scroll in the lv's window!
  (unless (equal lv-wnd (selected-window))
    ;; Once we select window other than lv's window, then we kill it!
    (remove-hook 'post-command-hook #'deimacs--post-command-once)
    (lv-delete-window)))

(defun deimacs-info ()
  "Show session information."
  (interactive)
  (when (eq major-mode #'deimacs-mode)
    (lv-message
     (concat
      (format "session: %s" (cdr deimacs-instance)) "\n"
      (format "history size: %s" (length deimacs-chat-history))
      "\n\n"
      (format "prompt_tokens: %s | completion_tokens: %s | total_tokens: %s"
              (ht-get deimacs-data 'prompt_tokens 0)
              (ht-get deimacs-data 'completion_tokens 0)
              (ht-get deimacs-data 'total_tokens 0))
      "\n\n"
      (format "model: %s" deimacs-model) "\n"
      (format "max_tokens: %s" deimacs-max-tokens) "\n"
      (format "temperature: %s" deimacs-temperature) "\n"
      (format "top-p: %s" deimacs-top-p) "\n"
      (format "user: %s" (deimacs-user))))
    ;; Register event to cancel lv window!
    (add-hook 'pre-command-hook #'deimacs--pre-command-once)))

;;
;;; Entry

(defun deimacs-mode--kill-buffer-hook ()
  "Kill buffer hook."
  (ht-clear deimacs-data)
  (spinner-stop deimacs-spinner)
  (deimacs--cancel-text-timer)
  (let ((instance deimacs-instances))
    (when (get-buffer deimacs-input-buffer-name)
      (with-current-buffer deimacs-input-buffer-name
        ;; kill input if it's the right session
        (when (equal instance deimacs-instances)
          (kill-this-buffer))))))

(defun deimacs-header-line ()
  "The display for header line."
  (format " %s[Session] %s  [History] %s  [User] %s"
          (if-let ((frame (spinner-print deimacs-spinner)))
              (concat frame " ")
            "")
          (cdr deimacs-instance)
          (length deimacs-chat-history)
          (deimacs-user)))

(defvar deimacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'deimacs-type-response)
    map)
  "Keymap for `deimacs-mode'.")

(defun deimacs-mode-insert-tip ()
  "Insert tip to output buffer."
  (when (string-empty-p (buffer-string))
    (let ((inhibit-read-only t)
          (tip "Press <return> to start asking questions

`M-x deimacs-info` will print out more information about the current session!
"))
      (add-face-text-property 0 (length tip) 'deimacs-tip nil tip)
      (insert tip))))

(define-derived-mode deimacs-mode fundamental-mode "deimacs"
  "Major mode for `deimacs-mode'.

\\<deimacs-mode-map>"
  (setq-local buffer-read-only t)
  (font-lock-mode -1)
  (add-hook 'kill-buffer-hook #'deimacs-mode--kill-buffer-hook nil t)
  (setq-local header-line-format `((:eval (deimacs-header-line))))
  (setq deimacs-spinner (spinner-create deimacs-spinner-type t))
  (setq-local deimacs-data (ht-create))
  (deimacs-mode-insert-tip))

(defun deimacs-register-instance (index buffer-or-name)
  "Register BUFFER-OR-NAME with INDEX as an instance.

Caution, this will overwrite the existing instance!"
  (ht-set deimacs-instances index (get-buffer-create buffer-or-name))
  (with-current-buffer buffer-or-name
    (deimacs-mode)
    (setq deimacs-instance (cons index (current-buffer)))))

;;;###autoload
(defun deimacs-new ()
  "Run a new instance of deimacs."
  (interactive)
  (let* ((new-index       (deimacs--new-index))
         (new-buffer-name (format deimacs-buffer-name-format new-index)))
    (when (get-buffer new-buffer-name)
      (user-error "Internal Error: creating instance that already exists"))
    (deimacs-register-instance new-index new-buffer-name)
    (deimacs--pop-to-buffer new-buffer-name)))

;;;###autoload
(defun deimacs ()
  "Start deimacs with existing instance, else create a new instance."
  (interactive)
  (let ((live-instances  (deimacs--live-instances))
        (shown-instances (deimacs--shown-instances)))
    (cond (shown-instances
           (deimacs--pop-to-buffer (nth 0 shown-instances)))
          (live-instances
           (deimacs--pop-to-buffer (nth 0 live-instances)))
          (t
           (deimacs-new)))))

(provide 'deimacs)
;;; deimacs.el ends here
