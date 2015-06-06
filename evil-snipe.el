;;; evil-snipe.el --- emulate vim-sneak & vim-seek
;;
;; Copyright (C) 2014-15 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: December 5, 2014
;; Modified: June 6, 2015
;; Version: 2.0.0
;; Keywords: emulation, vim, evil, sneak, seek
;; Homepage: https://github.com/hlissner/evil-snipe
;; Package-Requires: ((evil "1.1.3"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Evil-snipe enables you to search quicker and more precisely in the buffer. It
;; improves on evil-mode's built-in single character motions (f/F/t/T) by adding
;; another key pair for 2-char searches: s and S -- as well as z/Z in operator
;; mode (e.g. following a d or c motion).
;;
;; 2-char searchs are 50x more accurate. You could use / or ?, but it saves you
;; the extra keystroke of pressing enter and is harder to reach than s and S.
;;
;; To enable globally, add the following lines to ~/.emacs:
;;
;;     (require 'evil-snipe)
;;     (evil-snipe-mode 1)
;;
;; To replace evil-mode's f/F/t/T functionality with (1-character) snipe, use:
;;
;;     (evil-snipe-override-mode 1)
;;
;;; Code:

(require 'evil)
(eval-when-compile (require 'cl-lib))

(defgroup evil-snipe nil
  "vim-seek/sneak emulation for Emacs"
  :prefix "evil-snipe-"
  :group 'evil)

(defcustom evil-snipe-enable-highlight t
  "If non-nil, all matches will be highlighted after the initial jump.
Highlights will disappear as soon as you do anything afterwards, like move the
cursor."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-enable-incremental-highlight t
  "If non-nil, each additional keypress will incrementally search and highlight
matches. Otherwise, only highlight after you've finished skulking."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-override-evil nil
  "If non-nil, replace evil's native f/F/t/T/;/, with evil-snipe."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-override-evil-repeat-keys t
  "If non-nil (while `evil-snipe-override-evil' is non-nil) evil-snipe will
override evil's ; and , repeat keys in favor of its own."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-scope 'line
  "Dictates the scope of searches, which can be one of:

    'line    ;; search line after the cursor (this is vim-seek behavior) (default)
    'buffer  ;; search rest of the buffer after the cursor (vim-sneak behavior)
    'visible ;; search rest of visible buffer (Is more performant than 'buffer, but
             ;; will not highlight/jump past the visible buffer)
    'whole-line     ;; same as 'line, but highlight matches on either side of cursor
    'whole-buffer   ;; same as 'buffer, but highlight *all* matches in buffer
    'whole-visible  ;; same as 'visible, but highlight *all* visible matches in buffer"
  :group 'evil-snipe
  :type 'symbol)

(defcustom evil-snipe-repeat-scope 'whole-line
  "Dictates the scope of repeat searches (see `evil-snipe-scope' for possible
settings)"
  :group 'evil-snipe
  :type 'symbol)

(defcustom evil-snipe-count-scope nil
  "Dictates the scope of searches, which can be one of:

    nil          ;; default; treat count as repeat count
    'letters     ;; count = how many characters to expect and search for
    'vertical    ;; find first match within N (visible) columns"
  :group 'evil-snipe
  :type 'symbol)

(defcustom evil-snipe-spillover-scope nil
  "Takes any value `evil-snipe-scope' accepts. If nil, a failed search will
simply fail. If non-nil, snipe will search for more matches within this scope.
It is useful only if set to a broader scope than `evil-snipe-scope'.

This also applies to N>1 COUNT prefixes. E.g. if 3sab fails, it will extend the
scope to `evil-snipe-spillover-scope''s to find a 3rd match."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-repeat-keys t
  "If non-nil, pressing s/S after a search will repeat it. If
`evil-snipe-override-evil' is non-nil, this applies to f/F/t/T as well."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-show-prompt t
  "If non-nil, show 'N>' prompt while sniping."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-smart-case nil
  "By default, searches are case sensitive. If `evil-snipe-smart-case' is
enabled, searches are case sensitive only if search contains capital
letters."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-auto-scroll nil
  "If non-nil, the window will scroll to follow the cursor."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-symbol-groups '()
  "You specify key aliases here, in the format '(KEY REGEX). Any instance of KEY
will be replaced with REGEX.

Here are some examples:

    ;; Alias [ and ] to all types of brackets
    (add-to-list 'evil-snipe-symbol-groups '(?\\] \"[]})]\"))
    (add-to-list 'evil-snipe-symbol-groups '(?\\[ \"[[{(]\"))
    ;; For python style functions
    (add-to-list 'evil-snipe-symbol-groups '(?\\: \"def .+:\"\))"
  :group 'evil-snipe
  :type 'list)

(defvar evil-snipe-auto-disable-substitute t
  "Disables evil's native s/S functionality (substitute) if non-nil. By default
this is t, since they are mostly redundant with other motions. s can be done
via cl and S with cc (or C).

MUST BE SET BEFORE EVIL-SNIPE IS LOADED.")

(defface evil-snipe-first-match-face
  '((t (:inherit isearch)))
  "Face for first match when sniping"
  :group 'evil-snipe)

(defface evil-snipe-matches-face
  '((t (:inherit region)))
  "Face for other matches when sniping"
  :group 'evil-snipe)

;; State vars ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil-snipe--last nil
  "The last search performed.")

(defvar evil-snipe--last-repeat nil
  "Whether the last search was a repeat.")

(defvar evil-snipe--last-direction t
  "Direction of the last search.")

(defvar evil-snipe--consume-match t
  "Whether the search should consume the match or not.")

(defvar evil-snipe--match-count 2
  "Number of characters to match. Can be let-bound to create motions that search
  for N characters. Do not set directly, unless you want to change the default
  number of characters to search.")

(defvar evil-snipe--this-func nil)

(defvar evil-snipe--transient-map-func nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-snipe--case-p (keystr)
  (let ((case-fold-search nil)
        (keystr (if (not (stringp keystr)) (evil-snipe--key-patterns keystr) keystr)))
    (if evil-snipe-smart-case
        (not (string-match-p "[A-Z]" keystr))
      nil)))

(defun evil-snipe--count ()
  (when current-prefix-arg (prefix-numeric-value current-prefix-arg)))

(defun evil-snipe--interactive (&optional how-many)
  (let ((count (evil-snipe--count))
        (evil-snipe--match-count (or how-many 2)))
    (list (evil-snipe--collect-keys count evil-snipe--last-direction))))

(defun evil-snipe--process-key (key)
  (let ((regex-p (assoc key evil-snipe-symbol-groups))
        (keystr (if (characterp key) (char-to-string key) key)))
    (cons keystr
          (if regex-p (elt regex-p 1) (regexp-quote keystr)))))

(defun evil-snipe--process-keys (keys)
  (mapcar 'evil-snipe--process-key keys))

(defun evil-snipe--collect-keys (&optional count forward-p)
  "The core of evil-snipe's N-character searching. Prompts for
`evil-snipe--match-count' characters, which can be incremented by pressing TAB.
Backspace works for correcting yourself too.

If `evil-snipe-count-scope' is 'letters, N = `count', so 5s will prompt you for
5 characters."
  (let* ((count (or count 1))
         (how-many (if (eq evil-snipe-count-scope 'letters)
                       (or (if count (abs count))
                           evil-snipe--match-count)
                     evil-snipe--match-count))
         (data '())
         (i how-many)
         ;; disable this to suppress keys messing with the prompt
         (echo-keystrokes 0)
         regex-p)
    (unless forward-p (setq count (* -1 count)))
    (unwind-protect
        (catch 'abort
          (while (> i 0)
            (let* ((keystr (evil-snipe--keys data))
                   (prompt (if evil-snipe-show-prompt (concat (number-to-string i) ">" keystr) ""))
                   (key (read-event prompt)))
              (cond ((eq key 'tab)                  ; Tab = adds more characters to search
                     (setq i (1+ i)))

                    ((eq key 'return)               ; Enter = search with
                     (if (= i how-many)             ;         current characters
                         (throw 'abort 'repeat)
                       (throw 'abort data)))
                    ((eq key 'escape)               ; Escape/C-g = abort
                     (evil-snipe--pre-command)
                     (throw 'abort 'abort))

                    ;; Otherwise, process key
                    (t (if (eq key 'backspace)      ; if backspace, delete a character
                           (progn
                             (cl-incf i)
                             (let ((data-len (length data)))
                               (if (<= (length data) 1)
                                   (progn (evil-snipe--pre-command)
                                          (throw 'abort 'abort))
                                 (nbutlast data))))
                         ;; Otherwise add it
                         (setq regex-p (assoc key evil-snipe-symbol-groups))
                         (setq data (append data (list (evil-snipe--process-key key))))
                         (cl-decf i))
                       (when evil-snipe-enable-incremental-highlight
                         (evil-snipe--pre-command)
                         (evil-snipe--highlight-all count (evil-snipe--key-patterns data))
                         (add-hook 'pre-command-hook 'evil-snipe--pre-command))))))
          data))))

(defun evil-snipe--bounds (&optional forward-p count)
  "Returns a cons cell containing (beg . end), which represents the search scope
depending on what `evil-snipe-scope' is set to."
  (let* ((point+1 (1+ (point)))
         (evil-snipe-scope (or (if (and count (> (abs count) 1)) evil-snipe-spillover-scope) evil-snipe-scope))
         (bounds (cl-case evil-snipe-scope
                   ('line
                    (if forward-p
                        `(,point+1 . ,(line-end-position))
                      `(,(line-beginning-position) . ,(point))))
                   ('visible
                    (if forward-p
                        `(,point+1 . ,(1- (window-end)))
                      `(,(window-start) . ,(point))))
                   ('buffer
                    (if forward-p
                        `(,point+1 . ,(point-max))
                      `(,(point-min) . ,(point))))
                   ('whole-line
                    `(,(line-beginning-position) . ,(line-end-position)))
                   ('whole-visible
                    `(,(window-start) . ,(1- (window-end))))
                   ('whole-buffer
                    `(,(point-min) . ,(point-max)))
                   (t
                    (error "Invalid scope: %s" evil-snipe-scope))))
         (end (cdr bounds)))
    (when (> (car bounds) end)
      (setq bounds `(,end . ,end)))
    bounds))

(defun evil-snipe--hl (beg end &optional first-p)
  "Highlights region between beg and end. If first-p is t, then use
`evil-snipe-first-p-match-face'"
  (if (and first-p (overlays-in beg end))
      (remove-overlays beg end 'category 'evil-snipe))
  (let ((overlay (make-overlay beg end nil nil nil)))
    (overlay-put overlay 'face (if first-p 'evil-snipe-first-match-face 'evil-snipe-matches-face))
    (overlay-put overlay 'category 'evil-snipe)))

(defun evil-snipe--hl-all (beg end count match)
  "Highlight all instances of `match' ahead of the cursor, or behind it if
`forward-p' is nil."
  (let ((forward-p (> count 0))
        (beg-offset (+ (point-min) beg -1))
        (string (buffer-substring-no-properties beg end))
        (case-fold-search (evil-snipe--case-p match))
        (i 0))
    (while (and (< i (length string))
                (string-match match string i))
      (when (= (% i count) 0)
        ;; TODO Apply column-bound highlighting
        (evil-snipe--hl (+ beg-offset (match-beginning 0))
                       (+ beg-offset (match-end 0))))
      (setq i (1+ (match-beginning 0))))))

(defun evil-snipe--pre-command ()
  "Disables overlays and cleans up after evil-snipe."
  (when evil-snipe-mode
    (remove-overlays nil nil 'category 'evil-snipe))
  (remove-hook 'pre-command-hook 'evil-snipe--pre-command))

(defun evil-snipe--disable-transient-map ()
  "Disable lingering transient map, if necessary."
  (when (and evil-snipe-mode (functionp evil-snipe--transient-map-func))
    (funcall evil-snipe--transient-map-func)
    (setq evil-snipe--transient-map-func nil)))

(defun evil-snipe--transient-map (forward-key backward-key)
  (let ((map (make-sparse-keymap)))
    ;; So ; and , are common to all sub keymaps
    (define-key map ";" 'evil-snipe-repeat)
    (define-key map "," 'evil-snipe-repeat-reverse)
    (when evil-snipe-repeat-keys
      (define-key map forward-key 'evil-snipe-repeat)
      (define-key map backward-key 'evil-snipe-repeat-reverse))
    map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-snipe--seek (count keylist type)
  (when (and keylist (listp keylist))
    (let ((forward-p (> count 0))
          (orig-point (point))
          (keydata (evil-snipe--process-keys keylist))
          (scope (evil-snipe--scope forward-p count))
          (inclusive-p (eq type 'inclusive))
          (evil-op-visual-p (or (evil-operator-state-p) (evil-visual-state-p))))
      ;; Adjust starting point
      (if forward-p (forward-char))
      (unless inclusive-p (if forward-p (forward-char) (backward-char)))
      (unwind-protect
          (if (re-search-forward (evil-snipe--to-keystr keydata)
                                 (if forward-p (cdr scope) (car scope))
                                 t count)
              (let ((beg (match-beginning 0))
                    (end (match-end 0)))
                ;; Set cursor position
                (if forward-p
                    (progn
                      (goto-char (if evil-op-visual-p (1- end) beg))
                      (unless inclusive-p (backward-char (length keylist))))
                  (goto-char (if inclusive-p beg end)))
                ;; Highlight first result
                (when (and (not evil-op-visual-p) evil-snipe-enable-highlight)
                  (evil-snipe--hl beg end t))
                ;; Follow the cursor
                (when evil-snipe-auto-scroll
                  (if (or (> window-start new-orig-point)
                          (< window-end new-orig-point))
                      (evil-scroll-line-to-center (line-number-at-pos))))
                t)
            (goto-char orig-point)
            nil)
        (when evil-snipe-enable-highlight
          (evil-snipe--hl-all (car scope) (cdr scope) count (evil-snipe--to-keystr keylist)))))))

;; TODO Implement evil-snipe--seek-vertical
(defun evil-snipe--seek-block (count keys)
  "Not implemented yet!"
  (error "Not implemented!"))


(cl-defun evil-snipe-seek (count keylist &optional (type inclusive) keymap (forward-p t))
  "Perform a snipe. KEYS is a list of characters provided by <-c> and <+c>
interactive codes. KEYMAP is the transient map to activate afterwards."
  (let ((evil-snipe--count count))
    (setq count (if forward-p (or count 1) (or (- count) -1)))
    (when (cl-case keylist
            ('abort)
            ;; if <enter>, repeat last search
            ('repeat (evil-snipe-repeat count))
            ;; If KEYS is empty
            ('() (user-error "No keys provided!"))
            ;; Otherwise, perform the search
            (t (cl-case evil-snipe-count-scope
                 ;; ('vertical (evil-snipe--seek-block count keylist type forward-p))
                 ('letters  (evil-snipe--seek count keylist type forward-p))
                 (otherwise (evil-snipe--seek count keylist type forward-p)))
               (user-error "Couldn't find %s" (concat keylist))))
      (add-hook 'pre-command 'evil-snipe--pre-command)
      (unless evil-snipe--last-repeat-p
        (setq evil-snipe--last (list count keylist type keymap forward-p)))
      (when keymap
        (setq evil-snipe--transient-map-func (set-transient-map keymap))))))

(evil-define-command evil-snipe-next (&optional count)
  "Repeat the last evil-snipe COUNT times"
  (interactive "<c>")
  (setq count (or count 1))
  (if evil-snipe--last
      (let ((last-count     (or (nth 0 evil-snipe--last) count))
            (last-keylist   (nth 1 evil-snipe--last))
            (last-type      (nth 2 evil-snipe--last))
            (last-keymap    (nth 3 evil-snipe--last))
            (last-forward-p (nth 4 evil-snipe--last))
            (evil-snipe--last-repeat-p t))
        (evil-snipe-seek (* last-count count (if last-forward-p 1 -1))
                         last-keylist last-type last-keymap last-forward-p))
    (user-error "Nothing to repeat")))

(evil-define-command evil-snipe-previous (count)
  "Repeat the inverse of the last evil-snipe `count' times"
  (interactive "<c>")
  (evil-snipe-next (or (if count (- count)) -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Backwards compatibility
(unless (fboundp 'set-transient-map)
  (defalias 'set-transient-map 'set-temporary-overlay-map))

(eval-when-compile
  (defun evil-snipe--motion (key chars type forward-p)
    `(progn
       (evil-define-motion ,(intern (format "evil-snipe-%s" key)) (count keylist)
         :jump t
         :repeat motion
         :type inclusive
         (interactive (evil-snipe--interactive ,chars ,forward-p))
         (evil-snipe-seek count keylist ,type ,(intern (format "evil-snipe-%s-map" key)) ,forward-p))))

  (defun evil-snipe--transient-map (key next prev)
    `(defvar ,(intern (format "evil-snipe-%s-map" key))
       (let ((map (make-sparse-keymap)))
         (define-key map ";" 'evil-snipe-repeat)
         (define-key map "," 'evil-snipe-repeat-reverse)
         (when evil-snipe-repeat-keys
           (define-key map ,next 'evil-snipe-repeat)
           (define-key map ,prev 'evil-snipe-repeat-reverse)))))

  (cl-defmacro evil-snipe-setup (next prev &key chars &key (type 'inclusive))
    (nconc
     (evil-snipe--motion next chars type t)
     (evil-snipe--motion prev chars type nil)
     (evil-snipe--tranient-map next next prev)
     (evil-snipe--tranient-map prev prev next))))

;;;###autoload (autoload 'evil-snipe-s "evil-snipe")
;;;###autoload (autoload 'evil-snipe-S "evil-snipe")
(evil-snipe-setup s S :chars 2 :type inclusive)

;;;###autoload (autoload 'evil-snipe-x "evil-snipe")
;;;###autoload (autoload 'evil-snipe-X "evil-snipe")
(evil-snipe-setup x X :chars 2 :type exclusive)

;;;###autoload (autoload 'evil-snipe-f "evil-snipe")
;;;###autoload (autoload 'evil-snipe-F "evil-snipe")
(evil-snipe-setup f T :chars 1 :type exclusive)

;;;###autoload (autoload 'evil-snipe-t "evil-snipe")
;;;###autoload (autoload 'evil-snipe-T "evil-snipe")
(evil-snipe-setup t T :chars 1 :type inclusive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil-snipe--s/S-actions nil)

(defvar evil-snipe-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key 'motion map "s" 'evil-snipe-s)
    (evil-define-key 'motion map "S" 'evil-snipe-S)
    (evil-define-key 'operator map "z" 'evil-snipe-s)
    (evil-define-key 'operator map "Z" 'evil-snipe-S)
    (evil-define-key 'operator map "x" 'evil-snipe-x)
    (evil-define-key 'operator map "X" 'evil-snipe-X)
    map))

(defvar evil-snipe-override-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key 'motion map "f" 'evil-snipe-f)
    (evil-define-key 'motion map "F" 'evil-snipe-F)
    (evil-define-key 'motion map "t" 'evil-snipe-t)
    (evil-define-key 'motion map "T" 'evil-snipe-T)
    map))


;;;###autoload
(define-minor-mode evil-snipe-mode
  "evil-snipe minor mode."
  :global t
  :lighter " snipe"
  :keymap evil-snipe-mode-map
  :group 'evil-snipe
  (if evil-snipe-mode
      (progn
        (when evil-snipe-auto-disable-substitute
          (setq evil-snipe--s/S-actions (list (lookup-key evil-normal-state-map "s")
                                              (lookup-key evil-normal-state-map "S")))
          (define-key evil-normal-state-map "s" nil)
          (define-key evil-normal-state-map "S" nil))
        (when (fboundp 'advice-add)
          (advice-add 'evil-force-normal-state :before 'evil-snipe--pre-command))
        (add-hook 'evil-insert-state-entry-hook 'evil-snipe--disable-transient-map))
    (when evil-snipe-auto-disable-substitute
      (define-key evil-normal-state-map "s" (car evil-snipe--s/S-funcs))
      (define-key evil-normal-state-map "S" (cdr evil-snipe--s/S-funcs)))
    (when (fboundp 'advice-remove)
      (advice-remove 'evil-force-normal-state 'evil-snipe--pre-command))
    (remove-hook 'evil-insert-state-entry-hook 'evil-snipe--disable-transient-map)
    (evil-snipe-override-mode -1)))

;;;###autoload
(define-minor-mode evil-snipe-override-mode
  "evil-snipe minor mode that overrides evil-mode f/F/t/T/;/, bindings."
  :global t
  :keymap evil-snipe-override-mode-map
  :group 'evil-snipe
  (when evil-snipe-override-evil-repeat-keys
    (evil-define-key 'motion map ";" 'evil-snipe-next)
    (evil-define-key 'motion map "," 'evil-snipe-previous)))


(provide 'evil-snipe)
;;; evil-snipe.el ends here
