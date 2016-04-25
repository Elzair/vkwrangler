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

;; Global variables

(defvar ext-fmt-path load-file-name "Path to this file.")
(defvar ext-fmt-excluded-funcs '(vkGetInstanceProcAddr vkGetDeviceProcAddr)
  "Functions to exclude from dynamic allocation.")

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
                     (intern (nth 2 x)))
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
      (remove-if #'(lambda (c)
                     (member (nth 0 c)
                             ext-fmt-excluded-funcs))
       (mapcar #'parse-command
              (filter-children (nthcdr 2 cmds)
                               'command)))))

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

(defun get-core-cmds (cmds ext-cmds)
  "Get all commands CMDS not listed under any extensions EXT-CMDS."
  (let ((ext-fns
         (mapcan #'(lambda (ext)
                     (mapcar #'car (nth 1 ext)))
                 ext-cmds)))
    (list 'core
          (remove-if #'(lambda (cmd)
                         (member (nth 0 cmd) ext-fns))
                     cmds))))

(defun parse-info (reg)
    "Parse all information from Vulkan Registry REG."
    (let* ((cmds    (parse-commands reg))
           (exts    (parse-extensions reg))
           (ext-cmds (get-ext-cmds cmds exts))
           (core-cmds (get-core-cmds cmds ext-cmds)))
      (list core-cmds
            ext-cmds)))

;; Header File Functions

(defun format-command (cmd)
    "Format function declaration from CMD."
    (concat
     (symbol-name (nth 1 cmd))
     " (*"
     (symbol-name (nth 0 cmd))
     ")("
     (mapconcat #'(lambda (param)
                  (mapconcat #'(lambda (elt)
                                 (symbol-name elt))
                             param
                             " "))
              (nth 2 cmd)
              ", ")
     ");"))

(defun format-commands (section &optional is-not-ext)
    "Format all commands in give SECTION.

If IS-NOT-EXT is nil, guard declarations with #ifdefs."
    (concat
     (when (null is-not-ext)
       (concat "#ifdef "
               (symbol-name (nth 0 section))
               "\n"))
     (mapconcat #'format-command
                (nth 1 section)
                "\n")
     (when (null is-not-ext)
       "\n#endif")))

(defun format-declarations (api)
    "Create function declarations for all functions in API."
    (concat (format-commands (nth 0 api) t)
            "\n"
            (mapconcat #'format-commands
                       (nth 1 api)
                       "\n")))

;; Main Commands

(defun get-vulkan-registry ()
  "Retrieve the vulkan registry from the internet and save parsed version as OUTPUT-FILE."
  (interactive)
  (let* ((parent-dir
          (file-name-as-directory (file-name-directory ext-fmt-path)))
         (xml-file
          (concat parent-dir
                  "vk.xml"))
         (sxp-file
          (concat parent-dir
                  "vk.sxp")))
    (url-copy-file "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/1.0/src/spec/vk.xml"
                   xml-file
                   t)
    (let ((contents nil))
      (with-temp-buffer
        (insert-file-contents xml-file)
        (setq contents (libxml-parse-xml-region (point-min) (point-max))))
      (save-excursion
        (find-file sxp-file)
        (delete-region (point-min) (point-max))
        (insert (pp-to-string contents))
        (save-buffer)
        (kill-buffer))
      contents)))

(defun make-api-file ()
  "Create a file listing function information for all Vulkan extensions and save it to OUTPUT-FILE and read from INPUT-FILE."
  (interactive)
  (let* ((parent-dir
          (file-name-as-directory (file-name-directory ext-fmt-path)))
         (api-file
          (concat parent-dir
                  "api.sxp"))
         (reg-conts (get-vulkan-registry))
         (api-conts nil))
    (setq api-conts (parse-info reg-conts))
    (save-excursion
      (find-file api-file)
      (insert (pp-to-string api-conts))
      (save-buffer)
      (kill-buffer))
    api-conts))

(defun test-make-header-file (api-file header-file)
  "This function will create a C header from the data in API-FILE and write it to HEADER-FILE."
  (interactive "FAPI File: \nFHeader File: ")
  (let* ((header-header
          (mapconcat #'identity
                     '("#ifndef __VKWRANGLER.H__"
                       "#define __VKWRANGLER.H__"
                       "#ifndef VK_NO_PROTOTYPES"
                       "#define VK_NO_PROTOTYPES"
                       "#endif"
                       "#include <vulkan/vulkan.h>"
                       "#ifdef __cplusplus"
                       "extern \"C\" {"
                       "#endif"
                       "")
                     "\n"))
         (header-footer
          (mapconcat #'identity
                     '(""
                       "void vkWranglerInitInstFns(VkInstance inst, const char* const* extensionNames);"
                       "void vkWranglerInitDevFns(VkDevice dev, const char* const* extensionNames);"
                       "#ifdef __cplusplus"
                       "}"
                       "#endif"
                       "#endif")
                     "\n"))
         (api-contents
          (with-temp-buffer
            (insert-file-contents api-file)
            (buffer-string)))
         (api
          (car (read-from-string api-contents)))
         (decls
          (format-declarations api))
         (out
          (concat header-header
                  decls
                  header-footer)))
    (find-file header-file)
    (insert out)))

;; (defun make-header-file-helper (ext)
;;     "This function will insert the vulkan extension EXT into the header file."
;;     (let ((ext-name (symbol-name (car ext)))
;;           (ext-fns  (cddr ext))
;;           (val      0)
;;           (tmp-name ""))
;;       (insert (concat "#ifdef " ext-name))
;;       (newline)
;;       (dolist (elt ext-fns val)
;;         (setq tmp-name (symbol-name elt))
;;         (insert (concat "PFN_" tmp-name " " tmp-name ";"))
;;         (newline))
;;       (insert "#endif")
;;       (newline)))

;; (defun make-header-file (api-file header-file)
;;   "This function will create a C header from the data in API-FILE and write it to HEADER-FILE."
;;   (interactive "FAPI File: \nFHeader File: ")
;;   (let* ((header-header '("#ifndef __VKWRANGLER.H__"
;;                           "#define __VKWRANGLER.H__"
;;                           "#ifndef VK_NO_PROTOTYPES"
;;                           "#define VK_NO_PROTOTYPES"
;;                           "#endif"
;;                           "#include <vulkan/vulkan.h>"
;;                           "#ifdef __cplusplus"
;;                           "extern \"C\" {"
;;                           "#endif"))
;;          (header-footer '("void vkWranglerInitInstFns(VkInstance inst, const char* const* extensionNames);"
;;                           "void vkWranglerInitDevFns(VkDevice dev, const char* const* extensionNames);"
;;                           "#ifdef __cplusplus"
;;                           "}"
;;                           "#endif"
;;                           "#endif"))
;;          (api-contents
;;           (with-temp-buffer
;;             (insert-file-contents api-file)
;;             (buffer-string)))
;;          (api
;;           (car (read-from-string api-contents))))
;;     (find-file header-file)
;;     (let ((val 0)
;;           (lst nil))
;;       ; Insert header
;;       (dolist (elt header-header val)
;;         (insert elt)
;;         (newline))
;;       ; Insert core function definitions
;;       (setq lst (cdr (assoc 'vulkan-instance-functions api)))
;;       (dolist (elt lst val)
;;         (let ((str (symbol-name elt)))
;;           (insert (concat "PFN_" str " " str ";"))
;;           (newline)))
;;       (setq lst (cdr (assoc 'vulkan-device-functions api)))
;;       (dolist (elt lst val)
;;         (let ((str (symbol-name elt)))
;;           (insert (concat "PFN_" str " " str ";"))
;;           (newline)))
;;       (setq lst (cdr (assoc 'vulkan-device-functions api)))
;;       (dolist (elt lst val)
;;         (let ((str (symbol-name elt)))
;;           (insert (concat "PFN_" str " " str ";"))
;;           (newline)))
;;       ; Insert extension functions
;;       (setq lst (cddr api))
;;       (dolist (elt lst val)
;;         (make-header-file-helper elt))
;;       ; Insert footer
;;       (dolist (elt header-footer val)
;;         (insert elt)
;;         (newline)))))

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
