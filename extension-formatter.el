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
(defvar ext-fmt-device-fn-params '(VkDevice VkQueue VkCommandBuffer)
  "Parameters that signify a function returend by vkGetDeviceProcAddr().")

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

(defun format-header-command (cmd)
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

(defun format-header-commands (section)
    "Format all commands in give SECTION.

If IS-NOT-EXT is nil, guard declarations with #ifdefs."
    (mapconcat #'format-header-command
               (nth 1 section)
               "\n"))

(defun format-header-declarations (api)
    "Create function declarations for all functions in API."
    (concat (format-header-commands (nth 0 api))
            "\n"
            (mapconcat #'format-header-commands
                       (nth 1 api)
                       "\n")))

;; Source File Functions

(defun filter-inst-or-dev-fns-helper (section devp)
  "Filter instance functions or device functions from SECTION if DEVP is true or false."
  (let ((name (nth 0 section))
        (fns  (nth 1 section)))
    (cl-flet ((filter-func (fn)
                           (member (nth 0 (nth 0 (nth 2 fn)))
                                   ext-fmt-device-fn-params)))
      (list name
            (if devp
                (remove-if-not #'filter-func fns)
              (remove-if #'filter-func fns))))))

(defun filter-inst-or-dev-fns (api devp)
    "Filter instance functions or device functions from API if DEVP is true or false."
    (cl-flet ((helper-func (section)
                           (filter-inst-or-dev-fns-helper section devp)))
      (list (filter-inst-or-dev-fns-helper (nth 0 api) devp)
            (mapcar #'helper-func
                    (nth 1 api)))))

(defun filter-param-names (api)
  "Filter all parameter names (i.e. final item of param list) from API."
  (cl-flet* ((filter-param-name (p)
                                (reverse (cdr (reverse p))))
             (filter-param-list (lst)
                                (mapcar #'filter-param-name lst))
             (filter-cmd (cmd)
                         (list (nth 0 cmd)
                               (nth 1 cmd)
                               (funcall #'filter-param-list
                                        (nth 2 cmd))))
             (filter-section (section)
                             (list (nth 0 section)
                                   (mapcar #'filter-cmd
                                           (nth 1 section)))))
    (list (filter-section (nth 0 api))
          (mapcar #'filter-section
                  (nth 1 api)))))


(defun format-source-command (cmd devp)
  "Format function definitions from CMD.  Use vkGetInstanceProcAddr() or vkGetDeviceProcAddr() if DEVP is false or true."
  (concat
   (symbol-name (nth 0 cmd))
   " = ("
   (symbol-name (nth 1 cmd))
   " (*"
   ")("
   (mapconcat #'(lambda (param)
                  (mapconcat #'(lambda (elt)
                                 (symbol-name elt))
                             param
                             " "))
              (nth 2 cmd)
              ", ")
   "))"
   (if devp
       "vkGetDeviceProcAddr(device"
     "vkGetInstanceProcAddr(instance")
   ", \""
   (symbol-name (nth 0 cmd))
   "\");"))

(defun format-source-commands (section extp devp)
    "Format all commands in given SECTION.  If EXTP is not nil, guard declarations with #ifdefs.  Pass DEVP to `format-source-command'."
    (cl-flet ((format-helper (cmd)
                             (format-source-command cmd devp)))
     (concat
      (when extp
        (concat "if (strcmp(ppEnabledExtensionNames[i], \""
                (symbol-name (nth 0 section))
                "\") == 0) {"
                "\n"))
      (mapconcat #'format-helper
                 (nth 1 section)
                 "\n")
      (when extp
        "\n}"))))

(defun make-function-definition (api devp)
  "Make function definition command from data in API.  If DEVP is true, make device funtions; otherwise, make instance functions."
  (cl-flet ((mfd-helper (section)
                        (format-source-commands section t devp)))
    (let ((filt-api (filter-inst-or-dev-fns api devp)))
      (concat "{"
             "\n"
             (format-source-commands (nth 0 filt-api) nil devp)
             "\n"
             "uint32_t i;"
             "\n"
             "for (i=0; i < enabledExtensionNameCount; i++) {"
             "\n"
             (mapconcat #'mfd-helper
                        (nth 1 filt-api)
                        "\n")
             "\n"
             "}"
             "\n"
             "}"))))

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
         (api-conts (parse-info reg-conts)))
    (save-excursion
      (find-file api-file)
      (delete-region (point-min) (point-max))
      (insert (pp-to-string api-conts))
      (save-buffer)
      (kill-buffer))
    api-conts))

(defun make-header-file (api-file header-file)
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
                       "void vkWranglerInitInstanceFunctions(VkInstance instance, uint32_t enabledExtensionNameCount, const char* const* ppEnabledExtensionNames);"
                       "void vkWranglerInitDeviceFunctions(VkDevice device, uint32_t enabledExtensionNameCount, const char* const* ppEnabledExtensionNames);"
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
          (format-header-declarations api))
         (output
          (concat header-header
                  decls
                  header-footer)))
    (save-excursion
      (find-file header-file)
      (delete-region (point-min) (point-max))
      (insert output)
      (save-buffer)
      (kill-buffer))))

(defun make-source-file (api-file source-file header-file)
  "This function will create a C source file from the data in API-FILE and write it to SOURCE-FILE.  Include specified HEADER-FILE."
  (interactive "FAPI File: \nFSource File: \nFHeader File: ")
  (let* ((source-header
          (concat "#include \""
                  header-file
                  "\""
                  "\n"))
         (api-contents
          (with-temp-buffer
            (insert-file-contents api-file)
            (buffer-string)))
         (api
          (filter-param-names (car (read-from-string api-contents))))
         (inst-func
          (concat "void vkWranglerInitInstanceFunctions(VkInstance instance, uint32_t enabledExtensionNameCount, const char* const* ppEnabledExtensionNames)"
                  (make-function-definition api nil)))
         (dev-func
          (concat "void vkWranglerInitDeviceFunctions(VkDevice device, uint32_t enabledExtensionNameCount, const char* const* ppEnabledExtensionNames)"
                  (make-function-definition api t)))
         (output
          (concat source-header
                  inst-func
                  "\n"
                  dev-func
                  )))
    (save-excursion
      (find-file source-file)
      (delete-region (point-min) (point-max))
      (insert output)
      (save-buffer)
      (kill-buffer))))

(provide 'extension-formatter)
;;; extension-formatter.el ends here
