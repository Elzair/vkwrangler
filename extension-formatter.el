;;; extension-formatter.el --- Helper functions to generate wrapper -*- lexical-binding: t -*-

;; Copyright © 2016 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>
;; Version: 0.1
;; Keywords: keywords
;; URL: http://gitlab.com/elzair/project

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains some functions to generate a C header and source file
;; that will retrieve the core (and possibly extended) functions from
;; an instance of the Vulkan API.

;;; Code:

(require 'cl)

;; Helper Functions

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun filter-children (lst name)
  "Return only sub-lists of list LST that have a first element matching NAME."
  (remove-if-not #'(lambda (x)
                     (and (listp x)
                          (not (null x))
                          (eq (car x) name)))
                 lst))

;; Command Parsing Functions

(defun parse-param (param)
    "Parse function parameter PARAM."
  (mapcar #'(lambda (x)
              (cond ((and (listp x)
                          (not (null x)))
                     (nth 2 x))
                    ((stringp x)
                     (intern (chomp x)))))
          (nthcdr 2 param)))

(defun parse-proto (proto)
    "Parse function prototype PROTO."
    (list (intern (nth 2 (assoc 'name (nthcdr 2 proto))))
          (intern (nth 2 (assoc 'type (nthcdr 2 proto))))))

(defun parse-command (cmd)
    "Parse command CMD."
    (let* ((body (nthcdr 2 cmd))
           (proto (parse-proto (assoc 'proto body))))
      (list (nth 0 proto)
            (nth 1 proto)
            (mapcar #'parse-param
                    (filter-children body 'param)))))

(defun parse-commands (reg)
    "Parse all commands in Vulkan Registry REG."
    (let ((cmds (assoc 'commands
                       (nthcdr 2 reg))))
     (mapcar #'parse-command
             (filter-children (nthcdr 2 cmds)
                              'command))))

;; Extension Functions

(defun parse-ext-reqs (reqs)
    "Parse functions required for extension REQS."
    (mapcar #'(lambda (x)
                (intern (cdr (assoc 'name (cadr x)))))
     (filter-children reqs 'command)))

(defun parse-ext (ext)
    "Parse extension EXT."
    (list (intern (cdr (assoc 'name (nth 1 ext))))
          (parse-ext-reqs (assoc 'require ext))))

(defun parse-extensions (reg)
    "Retrieve all extensions from Vulkan Registry REG."
    (mapcar #'(lambda (x) (parse-ext x))
            (filter-children (assoc 'extensions reg)
                             'extension)))

;; Combination Functions

(defun get-ext-cmds (cmds exts)
  "Get all commands from CMDS listed under all extensions EXTS."
  (mapcar #'(lambda (ext)
              (list (nth 0 ext)
                    (mapcar #'(lambda (func)
                           (assoc func cmds))
                       (nth 1 ext))))
          exts))

(defun parse-info (reg)
    "Parse all information from Vulkan Registry REG."
    (get-ext-cmds (parse-commands   reg)
                  (parse-extensions reg)))

(defun get-vulkan-registry (&optional output-file)
  "Retrieve the vulkan registry from the internet and save parsed version as OUTPUT-FILE."
  (interactive "FOutput File: ")
  (let* ((tmp-dir   (make-temp-file "vulkan" t))
         (tmp-fname (concat (file-name-as-directory tmp-dir)
                            "vk.xml")))
    (url-copy-file "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/1.0/src/spec/vk.xml"
                   tmp-fname)
    (let ((contents nil))
      (with-temp-buffer
        (insert-file-contents tmp-fname)
        (setq contents (libxml-parse-xml-region (point-min) (point-max))))
      (when (not (null output-file))
        (save-excursion
          (find-file output-file)
          (insert (pp-to-string contents))
          (save-buffer)
          (kill-buffer)))
      contents)))

;; (defun ext-get-return ()
;;     "Get return value of current function."
;;     (goto-char (line-beginning-position))
;;     (forward-word)
;;     (let ((beg (point))
;;           (end (1- (search-forward "("))))
;;       (intern (chomp (buffer-substring-no-properties beg end)))))

;; (defun ext-get-name ()
;;   "Get name of current function."
;;   (goto-char (line-beginning-position))
;;   (let* ((beg (search-forward "PFN_"))
;;          (end 0))
;;     (forward-word)
;;     (setq end (point))
;;     (intern (buffer-substring-no-properties beg end))))

;; (defun ext-get-args-helper (arg)
;;     "Helper function for `ext-get-args' to map an individual ARG."
;;   (let ((arg-elts (split-string arg)))
;;     (mapcar #'intern arg-elts)))

;; (defun ext-get-args ()
;;   "Get input arguments of current function."
;;   (goto-char (line-beginning-position))
;;   (forward-list)
;;   (let ((beg (1+ (point)))  ; Strip open parenthesis
;;         (end 0))
;;     (forward-list)
;;     (setq end (1- (point))) ; Strip closedparenthesis
;;     (let* ((str  (buffer-substring-no-properties beg end))
;;            (args (split-string str "," t)))
;;       (cl-flet ((helper (arg)
;;                         (mapcar #'intern (split-string arg))))
;;         (mapcar #'ext-get-args-helper args)))))

;; (defun ext-get-function-info ()
;;   "Extract a function signature from file."
;;   (let ((td-pos (search-forward "typedef" nil t)))
;;     (if (and (not (null td-pos))
;;              (< td-pos (point-max)))
;;         (let ((pfn-pos (search-forward "PFN_"
;;                                        (line-end-position)
;;                                        t))
;;               (info nil))
;;           (if (and (not (null pfn-pos))
;;                    (< pfn-pos (point-max)))
;;               (progn
;;                 (setq info
;;                       (list (ext-get-name)
;;                             (ext-get-args)
;;                             (ext-get-return)))
;;                 (goto-char td-pos)
;;                 info)
;;             (progn
;;               (goto-char td-pos)
;;               nil)))
;;       (progn
;;         (goto-char (point-max))
;;         nil))))

;; (defun extract-names-from-header (header-file output-file)
;;   "This function will extract the function names from HEADER-FILE and insert them into OUTPUT-FILE."
;;   (interactive "FHeader File: \nFOutput File:  ")
;;   (find-file header-file)
;;   (goto-char (point-min))
;;   (let ((fn  nil)
;;         (fns nil))
;;     (while (< (point) (point-max))
;;       (setq fn (ext-get-function-info))
;;       (when (not (null fn))
;;         (setq fns (cons fn fns))))
;;     (setq fns (reverse fns))
;;     ; Output function names to `output-file'
;;     (find-file output-file)
;;     (let ((val 0))
;;       (dolist (f fns val)
;;        (insert (pp-to-string f))
;;        (newline)))))

(defun make-header-file-helper (ext)
    "This function will insert the vulkan extension EXT into the header file."
    (let ((ext-name (symbol-name (car ext)))
          (ext-fns  (cddr ext))
          (val      0)
          (tmp-name ""))
      (insert (concat "#ifdef " ext-name))
      (newline)
      (dolist (elt ext-fns val)
        (setq tmp-name (symbol-name elt))
        (insert (concat "PFN_" tmp-name " " tmp-name ";"))
        (newline))
      (insert "#endif")
      (newline)))

(defun make-header-file (api-file header-file)
  "This function will create a C header from the data in API-FILE and write it to HEADER-FILE."
  (interactive "FAPI File: \nFHeader File: ")
  (let* ((header-header '("#ifndef __VKWRANGLER.H__"
                          "#define __VKWRANGLER.H__"
                          "#ifndef VK_NO_PROTOTYPES"
                          "#define VK_NO_PROTOTYPES"
                          "#endif"
                          "#include <vulkan/vulkan.h>"
                          "#ifdef __cplusplus"
                          "extern \"C\" {"
                          "#endif"))
         (header-footer '("void vkWranglerInitInstFns(VkInstance inst, const char* const* extensionNames);"
                          "void vkWranglerInitDevFns(VkDevice dev, const char* const* extensionNames);"
                          "#ifdef __cplusplus"
                          "}"
                          "#endif"
                          "#endif"))
         (api-contents
          (with-temp-buffer
            (insert-file-contents api-file)
            (buffer-string)))
         (api
          (car (read-from-string api-contents))))
    (find-file header-file)
    (let ((val 0)
          (lst nil))
      ; Insert header
      (dolist (elt header-header val)
        (insert elt)
        (newline))
      ; Insert core function definitions
      (setq lst (cdr (assoc 'vulkan-instance-functions api)))
      (dolist (elt lst val)
        (let ((str (symbol-name elt)))
          (insert (concat "PFN_" str " " str ";"))
          (newline)))
      (setq lst (cdr (assoc 'vulkan-device-functions api)))
      (dolist (elt lst val)
        (let ((str (symbol-name elt)))
          (insert (concat "PFN_" str " " str ";"))
          (newline)))
      (setq lst (cdr (assoc 'vulkan-device-functions api)))
      (dolist (elt lst val)
        (let ((str (symbol-name elt)))
          (insert (concat "PFN_" str " " str ";"))
          (newline)))
      ; Insert extension functions
      (setq lst (cddr api))
      (dolist (elt lst val)
        (make-header-file-helper elt))
      ; Insert footer
      (dolist (elt header-footer val)
        (insert elt)
        (newline)))))

(defun make-source-file (api-file src-file)
  "This function will create a C source file from the data in API-FILE and write it to SRC-FILE."
  (interactive "FAPI File: \nFSource File: ")
  (let* ((source-header '("#include <vulkan/vulkan.h>"
                          ))
         (source-footer '())
         (api-contents
          (with-temp-buffer
            (insert-file-contents api-file)
            (buffer-string)))
         (api
          (car (read-from-string api-contents))))
    (find-file src-file)
    (let ((val 0)
          (lst nil))
      ; Insert header
      (dolist (elt source-header val)
        (insert elt)
        (newline))
      ; Insert core function definitions
      (setq lst (cdr (assoc 'vulkan-instance-functions api)))
      (dolist (elt lst val)
        (let ((str (symbol-name elt)))
          (insert (concat "PFN_" str " " str ";"))
          (newline)))
      (setq lst (cdr (assoc 'vulkan-device-functions api)))
      (dolist (elt lst val)
        (let ((str (symbol-name elt)))
          (insert (concat "PFN_" str " " str ";"))
          (newline)))
      (setq lst (cdr (assoc 'vulkan-device-functions api)))
      (dolist (elt lst val)
        (let ((str (symbol-name elt)))
          (insert (concat "PFN_" str " " str ";"))
          (newline)))
      ; Insert extension functions
      (setq lst (cddr api))
      (dolist (elt lst val)
        (make-header-file-helper elt))
      ; Insert footer
      (dolist (elt source-footer val)
        (insert elt)
        (newline)))))

;; (defun format-line ()
;;     ""
;;   (interactive)
;;   (let ((beg (line-beginning-position)))
;;     (goto-char beg)
;;     (search-forward "PFN_")
;;     (let ((word-beg (point)))
;;       (forward-word)
;;       (let ((word-end (point)))
;;         (let ((word (buffer-substring-no-properties word-beg word-end)))
;;           (delete-region (line-beginning-position)
;;                          (line-end-position))
;;           (insert "PFN_")
;;           (insert word)
;;           (insert " ")
;;           (insert word)
;;           (insert ";")
;;           (forward-line 1))))))

;; (defun format-line-2 ()
;;     ""
;;   (interactive)
;;   (let ((beg (line-beginning-position)))
;;     (goto-char beg)
;;     (search-forward "PFN_")
;;     (let ((word-beg (point)))
;;       (forward-word)
;;       (let ((word-end (point)))
;;         (let ((word (buffer-substring-no-properties word-beg word-end)))
;;           (delete-region (line-beginning-position)
;;                          (line-end-position))
;;           (insert word)
;;           (insert " = (PFN_")
;;           (insert word)
;;           (insert ")vkGetInstanceProcAddr(instance, \"")
;;           (insert word)
;;           (insert "\");")
;;           (forward-line 1))))))



;; (defun format-line-helper (count)
;;     ""
;;     (interactive "nNumber of times:")
;;     (let ((total 0))
;;       (dotimes (number count total)
;;         (format-line))))

;; (defun format-line-2-helper (count)
;;     ""
;;     (interactive "nNumber of times:")
;;     (let ((total 0))
;;       (dotimes (number count total)
;;         (format-line-2))))

;; (defun format-line-3-helper (count)
;;     ""
;;     (interactive "nNumber of times:")
;;     (let ((total 0))
;;       (dotimes (number count total)
;;         (format-line-3))))

(provide 'extension-formatter)
;;; extension-formatter.el ends here
