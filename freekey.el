;; C-x -> C-p
;; C-c -> M-p
;;
(require 'cl-lib)

(defvar freekey-override nil
  "Enable mapping of C-x / C-c")

(defvar freekey-mode-specific-prefix "M-p"
  "Key to use when remapping C-c in specific modes")

(defvar freekey-ctl-x-prefix "C-p")

(defun freekey-mode ()
  "This is not a real mode"
  (interactive)
  (message "Freeing your keys...")
  (add-hook 'after-change-major-mode-hook #'freekey-on-mode-change-hook)
  (advice-add 'define-key :around 'freekey-define-key-advice)

  (global-unset-key (kbd "M-p"))
  (global-unset-key (kbd "C-p"))
  (global-unset-key (kbd "M-n"))
  (global-set-key (kbd "M-n") mode-specific-map)
  (global-set-key (kbd "C-p") ctl-x-map)

  (let ((freekey-override t))
    (freekey-kill-key "C-c" t)
    (freekey-kill-key "C-x" t)

    (keymap-global-set "C-x" 'undefined)
    (keymap-global-set "C-c" 'undefined)))

(defun freekey--ensure-local-map ()
  (unless freekey-local-map
    (setq-local freekey-local-map (make-sparse-keymap))))

(defun freekey--prefixes (key prefix)
  (when (sequencep key)
    (let ((elt0 (elt key 0)))
      (cl-typecase elt0
        (number (= elt0 (elt prefix 0)))
        (string (string= elt0 prefix))))))

(defun freekey--reprefix (key prefix)
  (when (sequencep key)
    (vconcat prefix key)))

(defun freekey--forward-binding (key new-prefix binding)
  (let* ((new-prefix (kbd new-prefix))
         (reprefixed (freekey--reprefix unprefixed new-prefix))
         (existing-prefix-mapping (lookup-key keymap new-prefix)))
    (when (and existing-prefix-mapping (not (keymapp existing-prefix-mapping)))
      (message "Warning: %s already mapped to %s" freekey-mode-specific-prefix existing-prefix-mapping)
      (define-key keymap new-prefix nil))
    (define-key keymap reprefixed binding)))

(defun freekey-define-key-advice (old-fn keymap key binding &rest rest)
  (let ((is-c-c (freekey--prefixes key (kbd "C-c")))
        (is-c-x (freekey--prefixes key (kbd "C-x"))))
    ;;(message "define-key %s %s" key (type-of key) (and (sequencep key) (type-of (elt key 0))))
    (if (and (or is-c-c is-c-x) (not freekey-override))
        (when (> (length key) 1) ;; Translate binding if we're not just rebinding C-c / C-x
          (when nil (message "define-key-advice skipping %s %s %s" key binding (eql binding 'undefined)))
          (let* ((unprefixed (cl-subseq key 1)))
            (cond
              (is-c-c (freekey--forward-binding key freekey-mode-specific-prefix binding))
              (is-c-x (freekey--forward-binding key freekey-ctl-x-prefix binding)))))
      (apply old-fn keymap key binding rest))))

(defun freekey-on-mode-change-hook (&rest r)
  (freekey-kill-key "C-c")
  (freekey-kill-key "C-x"))

(when nil
  (define-key (current-global-map) (kbd "C-c") nil)
  (define-key (current-local-map) (kbd "C-c") nil)
  (define-key paredit-mode-map (kbd "C-c") nil)

  (keymap-lookup (current-local-map) "C-x")

  (advice-add 'define-key :around 'define-key-advice)
  ;;(advice-remove 'define-key 'define-key-advice)

  (freekey-kill-key "C-c")

  (keymap-lookup (current-global-map) "C-c")
  (keymap-lookup (current-local-map) "C-c")
  (define-key (current-local-map) (kbd "C-c") nil))

(defun freekey--map-maps (fun map-or-alist &optional name)
  (if (eql 'keymap (car map-or-alist))
      (funcall fun map-or-alist name)
    (when (listp (car map-or-alist))
      (cl-loop for list in map-or-alist
            as mapping = (freekey--recurse-maps fun (cdr list) (car list))
            if mapping collect mapping))))

(defun freekey-kill-key-recursively (map-or-alist key &optional name)
  (if (eql 'keymap (car map-or-alist))
      (let ((freekey-override t))
        (define-key map-or-alist (kbd key) nil t))
    (when (listp (car map-or-alist))
      (cl-loop for list in map-or-alist
            do (freekey-kill-key-recursively (cdr list) key (car list))))))

(defun freekey-kill-key (key &optional globally)
  "KEY is the key to kill; it should not be from (kbd KEY)"
  (cl-loop for list in (list overriding-terminal-local-map
                             overriding-local-map
                             emulation-mode-map-alists
                             minor-mode-overriding-map-alist
                             minor-mode-map-alist
                             (current-local-map))
        do (freekey-kill-key-recursively list key))
  (when globally
    (let ((freekey-override t))
      (global-set-key (kbd key) 'undefined))))


(defun freekey-find-key-recursively (map-or-alist key &optional name)
  (freekey--map-maps (lambda (map name)
                       (when (keymap-lookup map key)
                         (format "%s: %s" name (keymap-lookup map key))))
                     map-or-alist))

(defun freekey-find-key (key)
  (flatten-tree
   (cl-loop for list in (list overriding-terminal-local-map
                              overriding-local-map
                              emulation-mode-map-alists
                              minor-mode-overriding-map-alist
                              minor-mode-map-alist
                              (current-global-map)
                              (current-local-map)
                              (current-local-map))
         as mapping = (freekey-find-key-recursively list key)
         if mapping collect mapping)))
