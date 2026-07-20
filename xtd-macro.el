;;; xtd-macro.el --- General-purpose utility macros -*- lexical-binding: t -*-

;; Author: sam kleinman (tychoish)
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4") (s "1.12"))
;; Keywords: utility, library, elisp.
;; URL: https://github.com/tychoish/xtdlib.el

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Utility macros for hooks, timers, toggles, and annotations -- not
;; tied to any of `f.el', `s.el', or `ht.el' specifically. Depends
;; only on `xtd-s' (for its string-joining helpers), never on the
;; main `xtdlib' file, so it can be required standalone.

;;; Code:

(require 'seq)
(require 'xtd-s)

(declare-function which-key-add-keymap-based-replacements "which-key")

(eval-when-compile
  (require 'cl-lib)
  (require 'bind-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; slow-op -- reporting for operation timing

(defvar slow-op-reporting debug-on-error
  "A toggle that, when enabled is supports more verbose timing reporting.
Turns `with-slow-op-timer' from a noop to reporting on the duration of enclosed operations.")

(defvar slow-op-threshold 0.01
  "Threshold in seconds, or fractions thereof. Controls the behavior of `with-slow-op-timer'. Any operation below this threshold (faster) are ignored. Use this to control verbosity.")

(defmacro with-slow-op-timer (name &rest body)
  "Send a message the BODY operation of NAME takes longer to execute than a hardcoded threshold."
  (declare (indent defun) (debug t))
  `(if (not slow-op-reporting)
       (progn ,@body)
     (let* ((inhibit-message t)
	    (time (current-time))
	    (return-value (progn ,@body))
	    (duration (time-to-seconds (time-since time))))
       (when (> duration slow-op-threshold)
	 (message "[op]: %s: %.06fs" ,name duration))
       return-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; utility macros

(defmacro disabled (&rest body)
  "Wrap BODY so it never executes. Keeps code byte-compilable while effectively commenting it out."
  `(unless 'disabled
     ,@body))

(defmacro with-force-write (&rest body)
  "Temporarily clear `buffer-read-only', evaluate BODY, then restore it to t."
  (declare (indent defun) (debug t))
  `(prog1
       (progn
         (setq buffer-read-only nil)
         ,@body)
     (setq buffer-read-only t)))

(defmacro pos-arg (name &key is)
  "Allow positional arguments to have annotated call-sites."
  (declare (indent defun) (debug t))
  (unless (or (stringp name) (symbolp name))
    (user-error "cannot annotate a positional arg without a name"))
  is)

(defalias 'pa 'pos-arg)

(cl-defmacro add-lazy-init (&key name operation (delay 1))
  (unless operation
    (user-error "cannot specify lazy-init without a function or symbol"))
  (when (and (stringp operation) (not (string-empty-p operation))
	     (intern-soft operation))
    (setq operation (intern operation)))

  (unless (and name (stringp name) (not (string-empty-p (string-trim name)))
	       (not (symbolp operation)))
    (setq name (format "<lazy> %s" (symbol-name operation))))
  `(run-with-idle-timer
    ,delay nil
    (lambda ()
      (with-slow-op-timer ,name
	(funcall ,operation)))))

(cl-defmacro add-one-shot-hook
    (&key name hook function result body form operation
	  ;; flags and options; with defaults
	  (args nil) (local nil) (persist nil) (count 1) (depth 0) (make-unique nil) (cleanup nil) (idle-timer nil))
  "Register a self-removing hook function named NAME on HOOK.
The hook body is specified via one of: FUNCTION (a symbol or lambda), FORM (an
expression), BODY (a list of forms), RESULT (evaluated at expansion time), or
OPERATION (called via funcall/apply with optional ARGS).

After firing COUNT times (default 1) the hook removes itself. Set PERSIST to t
to keep the hook active indefinitely. When IDLE-TIMER is a number, the body
runs in an idle timer of that many seconds rather than directly in the hook.

HOOK may be a symbol, a list of symbols, or the special sentinel
`after-first-frame-created', which routes to `window-setup-hook' in non-daemon
sessions and `server-after-make-frame-hook' in daemon sessions.

DEPTH controls hook insertion depth (default 0). LOCAL makes the hook
buffer-local. MAKE-UNIQUE generates an uninterned symbol to avoid name
collisions. CLEANUP uninterns the generated symbol after the hook fires."
  (let* ((unique-tag (or (when make-unique (gensym "hook-"))
			 (make-symbol "hook")))
	 (count-tag (cond (persist "perpeutal")
			  ((not (numberp count)) (user-error "must specify hook limited count as a number %d" count))
			  ((eq count 1) "one-shot")
			  (:else (format "run-%d-times" count))))
	 (cleanup-symbol (intern (s-join-with-hyphen "one-shot" count-tag name (symbol-name unique-tag))))
	 hooks)

    (when (or (eq hook 'after-first-frame-created)
              (equal hook '(quote after-first-frame-created)))
      (setq hook (if (daemonp)
		     'server-after-make-frame-hook
		   'window-setup-hook)))

    (when (functionp hook)
      (setq hook (funcall hook)))

    (when (symbolp hook)
      (setq hooks (list hook)))

    (when (listp hook)
      (if (null (seq-remove #'symbolp hook))
	  (setq hooks hook)
	(setq hooks (eval hook)))
      (if (symbolp hooks)
	  (setq hooks (list hooks))))

    (unless hooks
      (user-error "must have a symbol, list of symbols or form that evaluates to same for hook [%S]" hooks))

    (let* ((timer-name (format "<one-shot-hook> %s" name))
	   (filtered-hooks (--remove (eq 'quote it) hooks))
	   (resolved-form
	    (or (cond (form
		       form)
		      (body
		       `,@body)
		      (result
		       `,(eval result))
		      (operation
		       (if args
			   `(apply ,operation ,args)
			 `(funcall ,operation)))
		      ((and (symbolp function) (functionp function))
		       (if args
			   `(apply ',function ,args)
			 `(funcall ',function)))
		      ((and (functionp function) (listp function))
		       function)
		      ((symbolp function)
		       (if args
			   `(apply ',function ,args)
			 `(funcall ',function)))
		      ((listp function)
		       function))
		(user-error "could not resolve the hook function from input for %s" name)))
	   (remove-hook-forms
	    (--map `(remove-hook ',it ',cleanup-symbol ,local) filtered-hooks))
	   (cleanup-expr
	    (if (or make-unique cleanup)
		`(unintern ',cleanup-symbol obarray)
	      t))
	   (run-count-var (intern (concat (symbol-name cleanup-symbol) "--run-count"))))

      `(progn
	 (defvar ,run-count-var 0
	   ,(format "Number of times the one-shot hook function `%s' has fired." cleanup-symbol))

	 (defun ,cleanup-symbol ,args
	   ,(format "Self-removing hook function for `%s', registered on %S.
Generated by `add-one-shot-hook'; removes itself from the hook after
%s." name filtered-hooks (if (eq count 1) "one run" (format "%d runs" count)))
	   ,@(if idle-timer
		 `((run-with-idle-timer ,idle-timer nil
					(lambda ()
					  (with-slow-op-timer ,timer-name ,resolved-form)))
		   (cl-incf ,run-count-var)
		   (when (and (not ,persist) (>= ,run-count-var ,count))
		     ,@remove-hook-forms
		     ,cleanup-expr))
	       `((with-slow-op-timer ,timer-name
		   ,resolved-form
		   (cl-incf ,run-count-var)
		   (when (and (not ,persist) (>= ,run-count-var ,count))
		     ,@remove-hook-forms
		     ,cleanup-expr)))))

	 ,@(--map `(add-hook ',it ',cleanup-symbol ,depth ,local) filtered-hooks)))))

(defmacro make-run-hooks-function-for (mode)
  "Define a zero-argument function `run-hooks-for-MODE' that runs `MODE-hook'."
  (let* ((mode-name (symbol-name mode))
	 (hook-name (concat mode-name "-hook"))
	 (function-name (intern (concat "run-hooks-for-" mode-name))))
    `(defun ,function-name nil
       (run-hooks (intern ,hook-name)))))

(cl-defmacro create-toggle-functions (value &optional &key short-name local keymap key)
  "Define turn-on, turn-off, and toggle interactive commands for variable VALUE.
Use SHORT-NAME to override the generated name. LOCAL makes commands use `setq-local'.
Optionally bind the toggle to KEY in KEYMAP."
  (let* ((name (or short-name (symbol-name value)))
	 (suffix (when local "local"))
	 (ops (list
	       `(,(intern (s-join-with-hyphen "turn-on" name suffix)) t)
	       `(,(intern (s-join-with-hyphen "turn-off" name suffix)) nil)
	       `(,(intern (s-join-with-hyphen "toggle" name suffix)) (not ,value))))
	 (setter (if local 'setq-local 'setq)))

    (when (and keymap (not key))
      (user-error "must define both keymap and a key"))

    `(progn
       ,@(--map `(defun ,(car it) ()
		   (interactive)
		   (,setter ,value ,(cadr it)))
		ops)
       ,(when keymap
	  `(bind-key ,key ',(car (nth 2 ops)) ,keymap)))))

(cl-defmacro make-read-extended-command-for-prefix (prefix &optional &key bind-map bind-key key-alias)
  "Define an interactive command that runs `execute-extended-command' filtered to PREFIX.
Only commands whose names begin with PREFIX are offered for completion.
Optionally bind the command to BIND-KEY in BIND-MAP with KEY-ALIAS as the which-key label."
  (declare (indent defun))
  (unless (setq prefix (s-trimmed-or-nil prefix))
    (user-error "cannot build predicate function for '%s'" prefix))
  (unless key-alias
    (setq key-alias (s-join-with-kebab prefix "commands")))

  (setq prefix (s-normalize-symbol-name prefix))

  (let* ((predicate-name (format "read-extended-command-for-%s-prefix-p" prefix))
	 (predicate-symbol (intern predicate-name))
	 (user-command-name (format "execute-extended-%s-command" prefix))
	 (user-command-symbol (intern user-command-name)))
    `(prog1
	 (defun ,user-command-symbol ()
	   ,(format "Read extentend command but filtered for only those beginning with prefix `%s'." prefix)
	   (interactive)
	   (let ((read-extended-command-predicate #',predicate-symbol))
	     (with-suppressed-warnings ((interactive-only execute-extended-command))
	       (execute-extended-command nil))))

       (defun ,predicate-symbol (command _)
	 ,(format "Predicate for `read-extended-command-predicate' to filter commands returning only those that start with the prefix `%s'" prefix)
	 (string-prefix-p ,prefix (symbol-name command)))
       ,(when bind-key
	  `(progn
	     (bind-keys
	      :map ,(or bind-map 'global-map)
	      (,bind-key . ,user-command-symbol))
	     ,(when key-alias
		`(which-key-add-keymap-based-replacements ,(or bind-map 'global-map) ,bind-key ,key-alias)))))))

(defmacro with-toggle-once (name &rest body)
  "Define a function NAME that executes BODY only the first time it is called.
Subsequent calls are no-ops. Uses an auto-generated toggle variable to guard execution."
  (declare (indent defun) (debug t))
  (let ((operation (or (when (symbolp name) name)
		       (when (stringp name) (intern name))))
	(toggle (intern (s-join-with-hyphen (symbol-name name) "toggle-state"))))

    `(progn
       (defvar ,toggle nil
	 "Toggle variable to avoid re-execution of expensive configuration (like setting environment variables.)")

       (defun ,operation ()
	 (unless ,toggle
	   ,@body
	   (setq ,toggle t))))))

(defmacro with-prefix-arg (arg &rest body)
  "Evaluate BODY with `current-prefix-arg' bound to ARG."
  `(let ((current-prefix-arg ,arg))
     ,@body))

(defmacro with-default-directory (path &rest body)
  "Run the body with `default-directory' set to the path provided"
  (declare (indent defun) (debug t))
  `(let ((default-directory ,path))
     ,@body))

(defmacro with-silence (&rest body)
  "Totally suppress message from either the minibuffer or the *Messages* buffer.."
  (declare (indent defun) (debug t))
  `(let ((inhibit-message t)
         (message-log-max nil))
     ,@body))

(defmacro with-quiet (&rest body)
  "Suppress any messages from appearing in the minibuffer area."
  (declare (indent defun) (debug t))
  `(let ((inhibit-message t))
     ,@body))

(defmacro with-temp-keymap (map &rest body)
  "Bind MAP to a fresh sparse keymap, evaluate BODY, and return the keymap.
Use this to build a keymap programmatically and return it in one expression."
  (declare (indent defun) (debug t))
  `(let ((,map (make-sparse-keymap)))
     ,@body
     ,map))

(cl-defmacro setq-when-nil (variable value &optional &key local)
  "Set VARIABLE to VALUE only if VARIABLE is currently nil. Use :local t for `setq-local'."
  `(unless ,variable
     (,(if local 'setq-local 'setq) ,variable ,value)))

(defmacro with-timer (name &rest body)
  "Report on NAME and the time taken to execute BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06fs" ,name (float-time (time-since time)))))

(defmacro merge-predicate-functions (&rest preds)
  "Return a lambda that applies each predicate in PREDS to its argument with `and'.
Short-circuits on the first predicate that returns nil, consistent with `and' semantics."
  `(lambda (value)
     (and ,@(mapcar (lambda (p) `(funcall #',p value)) preds))))

(provide 'xtd-macro)
;;; xtd-macro.el ends here
