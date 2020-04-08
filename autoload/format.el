;;; autoload/format.el -*- lexical-binding: t; -*-

(defvar +format-region-p nil
  "Is non-nil if currently reformatting a selected region, rather than the whole
buffer.")

;;;###autoload
(autoload 'format-all--probe "format-all")

;;
;;; Commands
(defalias '+format/buffer #'format-all-buffer)

;;;###autoload
(defun +format/region (beg end)
  "Runs the active formatter on the lines within BEG and END.
WARNING: this may not work everywhere. It will throw errors if the region
contains a syntax error in isolation. It is mostly useful for formatting
snippets or single lines."
  (interactive "rP")
  (save-restriction
    (narrow-to-region beg end)
    (let ((+format-region-p t))
      (+format/buffer))))

;;;###autoload
(defun +format/region-or-buffer ()
  "Runs the active formatter on the selected region (or whole buffer, if nothing
is selected)."
  (interactive)
  (call-interactively
   (if (use-region-p)
       #'+format/region
     #'+format/buffer)))

;;;###autoload
(defun +format/lsp-format-region-or-buffer ()
  "Format the buffer (or selection) with LSP."
  (interactive)
  (unless (bound-and-true-p lsp-mode)
    (user-error "Not in an LSP buffer"))
  (call-interactively
   (if (poly-region-active-p)
       #'lsp-format-region
     #'lsp-format-buffer)))

;;; format.el ends here