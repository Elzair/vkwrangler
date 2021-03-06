;;; vk-wrangler.el --- Helper functions to generate Vulkan wrapper -*- lexical-binding: t -*-

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

(defvar vk-wrangler-path
  (file-name-as-directory (file-name-directory load-file-name)) "Path to this file.")
(defvar vk-wrangler-excluded-funcs
  '(vkGetInstanceProcAddr vkGetDeviceProcAddr)
  "Functions to exclude from dynamic allocation.")
(defvar vk-wrangler-device-fn-params
  '(VkDevice VkQueue VkCommandBuffer)
  "Parameters that signify a function returend by vkGetDeviceProcAddr().")
(defvar vk-wrangler-default-xml-url
  "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/1.0/src/spec/vk.xml"
  "Default URL for XML specification.")

;; Helper Functions

(defun vk-wrangler-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun vk-wrangler-filter-children (lst name)
  "Return only sub-lists of list LST that have a first element matching NAME."
  (remove-if-not #'(lambda (x)
                     (and (listp x)
                          (not (null x))
                          (eq (car x) name)))
                 lst))

(defun vk-wrangler-trim-final-elt (lst)
  "Trim final element from list LST."
  (cl-labels ((trim-helper (prev next)
                           (if (eq (car next)
                                   '##)
                               (reverse (cons '*
                                              prev))
                             (trim-helper (cons (car next)
                                                prev)
                                          (cdr next)))))
    (if (member '## lst)
        (trim-helper nil lst)
      (reverse (cdr (reverse lst))))))

;; Command Parsing Functions

(defun vk-wrangler-parse-param (param)
    "Parse function parameter PARAM."
  (mapcar #'(lambda (x)
              (cond ((and (listp x)
                          (not (null x)))
                     (intern (nth 2 x)))
                    ((stringp x)
                     (intern (vk-wrangler-chomp x)))))
          (nthcdr 2 param)))

(defun vk-wrangler-parse-proto (proto)
    "Parse function prototype PROTO."
    (list (intern (nth 2 (assoc 'name (nthcdr 2 proto))))
          (intern (nth 2 (assoc 'type (nthcdr 2 proto))))))

(defun vk-wrangler-parse-command (cmd)
    "Parse command CMD."
    (let* ((body (nthcdr 2 cmd))
           (proto (vk-wrangler-parse-proto (assoc 'proto body))))
      (list (nth 0 proto)
            (nth 1 proto)
            (mapcar #'vk-wrangler-parse-param
                    (vk-wrangler-filter-children body 'param)))))

(defun vk-wrangler-parse-commands (reg)
    "Parse all commands in Vulkan Registry REG."
    (let ((cmds (assoc 'commands
                       (nthcdr 2 reg))))
      (remove-if #'(lambda (c)
                     (member (nth 0 c)
                             vk-wrangler-excluded-funcs))
       (mapcar #'vk-wrangler-parse-command
              (vk-wrangler-filter-children (nthcdr 2 cmds)
                               'command)))))

;; Extension Functions

(defun vk-wrangler-filter-extensions (exts)
    "Return only extensions from EXTS that are supported in vulkan."
  (remove-if-not #'(lambda (ext)
                     (string-equal (cdr (assoc 'supported (nth 1 ext)))
                                   "vulkan"))
                 exts))

(defun vk-wrangler-parse-ext-reqs (reqs)
    "Parse functions required for extension REQS."
    (mapcar #'(lambda (x)
                (intern (cdr (assoc 'name (nth 1 x)))))
     (vk-wrangler-filter-children reqs 'command)))

(defun vk-wrangler-parse-ext (ext)
    "Parse extension EXT."
    (let* ((name (intern (cdr (assoc 'name    (nth 1 ext)))))
          (prot1 (cdr (assoc 'protect (nth 1 ext))))
          (prot  (if (null prot1) nil (intern prot1)))
          (reqs1 (assoc 'require ext))
          (reqs  (vk-wrangler-parse-ext-reqs reqs1)))
      (list name prot reqs)))

(defun vk-wrangler-parse-extensions (reg)
  "Retrieve all extensions from Vulkan Registry REG."
  (mapcar #'(lambda (x) (vk-wrangler-parse-ext x))
          (vk-wrangler-filter-extensions
           (vk-wrangler-filter-children (assoc 'extensions reg)
                                        'extension))))

;; Combination Functions

(defun vk-wrangler-get-ext-cmds (cmds exts)
  "Get all commands from CMDS listed under all extensions EXTS."
  (mapcar #'(lambda (ext)
              (list (nth 0 ext)
                    (nth 1 ext)
                    (mapcar #'(lambda (func)
                           (assoc func cmds))
                       (nth 2 ext))))
          exts))

(defun vk-wrangler-get-core-cmds (cmds ext-cmds)
  "Get all commands CMDS not listed under any extensions EXT-CMDS."
  (let ((ext-fns
         (mapcan #'(lambda (ext)
                     (mapcar #'car (nth 2 ext)))
                 ext-cmds)))
    (list 'core
          nil
          (remove-if #'(lambda (cmd)
                         (member (nth 0 cmd) ext-fns))
                     cmds))))

(defun vk-wrangler-parse-info (reg)
    "Parse all information from Vulkan Registry REG."
    (let* ((cmds    (vk-wrangler-parse-commands reg))
           (exts    (vk-wrangler-parse-extensions reg))
           (ext-cmds (vk-wrangler-get-ext-cmds cmds exts))
           (core-cmds (vk-wrangler-get-core-cmds cmds ext-cmds)))
      (list core-cmds
            ext-cmds)))

;; Header File Functions

(defun vk-wrangler-format-header-command (cmd)
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

(defun vk-wrangler-format-header-commands (section)
    "Format all commands in give SECTION."
    (concat
     (when (not (null (nth 1 section)))
       (concat "#ifdef " (symbol-name (nth 1 section)) "\n"))
     (mapconcat #'vk-wrangler-format-header-command
                (nth 2 section)
                "\n")
     (when (not (null (nth 1 section)))
       "\n#endif")))

(defun vk-wrangler-format-header-declarations (api)
    "Create function declarations for all functions in API."
    (concat (vk-wrangler-format-header-commands (nth 0 api))
            "\n"
            (mapconcat #'vk-wrangler-format-header-commands
                       (nth 1 api)
                       "\n")))

;; Source File Functions

(defun vk-wrangler-filter-inst-or-dev-fns-helper (section devp)
  "Filter instance functions or device functions from SECTION if DEVP is true or false."
  (let ((name (nth 0 section))
        (prot (nth 1 section))
        (fns  (nth 2 section)))
    (cl-flet ((filter-func (fn)
                           (member (nth 0 (nth 0 (nth 2 fn)))
                                   vk-wrangler-device-fn-params)))
      (list name
            prot
            (if devp
                (remove-if-not #'filter-func fns)
              (remove-if #'filter-func fns))))))

(defun vk-wrangler-filter-inst-or-dev-fns (api devp)
    "Filter instance functions or device functions from API if DEVP is true or false."
    (cl-flet ((helper-func (section)
                           (vk-wrangler-filter-inst-or-dev-fns-helper section devp)))
      (list (vk-wrangler-filter-inst-or-dev-fns-helper (nth 0 api) devp)
            (mapcar #'helper-func
                    (nth 1 api)))))

(defun vk-wrangler-filter-param-names (api)
  "Filter all parameter names (i.e. final item of param list) from API."
  (cl-flet* ((filter-param-list (lst)
                                (mapcar #'vk-wrangler-trim-final-elt lst))
             (filter-cmd (cmd)
                         (list (nth 0 cmd)
                               (nth 1 cmd)
                               (funcall #'filter-param-list
                                        (nth 2 cmd))))
             (filter-section (section)
                             (list (nth 0 section)
                                   (nth 1 section)
                                   (mapcar #'filter-cmd
                                           (nth 2 section)))))
    (list (filter-section (nth 0 api))
          (mapcar #'filter-section
                  (nth 1 api)))))


(defun vk-wrangler-format-source-command (cmd devp)
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
   "))(uintptr_t)"
   (if devp
       "vkGetDeviceProcAddr(device"
     "vkGetInstanceProcAddr(instance")
   ", \""
   (symbol-name (nth 0 cmd))
   "\");"))

(defun vk-wrangler-format-source-commands (section extp devp)
  "Format all commands in given SECTION.  If EXTP is not nil, guard declarations with #ifdefs.  Pass DEVP to `format-source-command'."
  (cl-flet ((format-helper (cmd)
                           (vk-wrangler-format-source-command cmd devp)))
    (if (not (null (nth 2 section)))
        (concat
         (when extp
           (concat (when (not (null (nth 1 section)))
                     (concat "#ifdef " (symbol-name (nth 1 section)) "\n"))
                   "if (strcmp(ppEnabledExtensionNames[i], \""
                   (symbol-name (nth 0 section))
                   "\") == 0) {"
                   "\n"))
         (mapconcat #'format-helper
                    (nth 2 section)
                    "\n")
         (when extp
           (concat "\n}"
                   (when (not (null (nth 1 section)))
                     "\n#endif"))))
      "")))

(defun vk-wrangler-make-function-definition (api devp)
  "Make function definition command from data in API.  If DEVP is true, make device funtions; otherwise, make instance functions."
  (cl-flet ((mfd-helper (section)
                        (vk-wrangler-format-source-commands section t devp)))
    (let ((filt-api (vk-wrangler-filter-inst-or-dev-fns api devp)))
      (concat "{"
             "\n"
             (vk-wrangler-format-source-commands (nth 0 filt-api) nil devp)
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

(defun vk-wrangler-get-vulkan-registry (&optional xml-url)
  "Retrieve the vulkan registry from the internet and save parsed version.

XML-URL is an alternative URL for the vulkan registry."
  (interactive)
  (let* ((my-url (if (null xml-url)
                   vk-wrangler-default-xml-url
                   xml-url))
         (spec-dir
          (file-name-as-directory (concat vk-wrangler-path
                                          "spec")))
         (xml-file
          (concat spec-dir
                  "vk.xml"))
         (sxp-file
          (concat spec-dir
                  "vk.sxp")))
    (url-copy-file my-url
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
        (kill-buffer)))))

(defun vk-wrangler-make-api-file ()
  "Create a file listing function information for all Vulkan extensions and save it to OUTPUT-FILE and read from INPUT-FILE."
  (interactive)
  (let* ((spec-dir
          (file-name-as-directory (concat vk-wrangler-path
                                          "spec")))
         (api-file
          (concat spec-dir
                  "api.sxp"))
         (sxp-file
          (concat spec-dir
                  "vk.sxp"))
         (reg-conts (car (read-from-string (with-temp-buffer
                                             (insert-file-contents sxp-file)
                                             (buffer-string)))))
         (api-conts (vk-wrangler-parse-info reg-conts)))
    (save-excursion
      (find-file api-file)
      (delete-region (point-min) (point-max))
      (insert (pp-to-string api-conts))
      (save-buffer)
      (kill-buffer))))

(defun vk-wrangler-make-header-file ()
  "This function will create a C header from the data in spec/api.sxp and write it to include/vkwrangler/vkwrangler.h."
  (interactive)
  (let ((api-file
         (concat vk-wrangler-path
                 "spec/api.sxp"))
        (header-file (concat vk-wrangler-path
                             "include/vkwrangler/vkwrangler.h"))
        (header-header
         (mapconcat #'identity
                    '("#ifndef __VKWRANGLER_H__"
                      "#define __VKWRANGLER_H__"
                      "#ifndef VK_NO_PROTOTYPES"
                      "#define VK_NO_PROTOTYPES"
                      "#endif"
                      "#include <string.h>"
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
                    "\n")))
    (make-directory (file-name-directory header-file) t)
    (let* ((api-contents
            (with-temp-buffer
              (insert-file-contents api-file)
              (buffer-string)))
           (api
            (car (read-from-string api-contents)))
           (decls
            (vk-wrangler-format-header-declarations api))
           (output
            (concat header-header
                    decls
                    header-footer)))
      (save-excursion
        (find-file header-file)
        (delete-region (point-min) (point-max))
        (insert output)
        (indent-region (point-min) (point-max))
        (save-buffer)
        (kill-buffer)))))

(defun vk-wrangler-make-source-file ()
  "This function will create a C source file from the data in API-FILE and write it to SOURCE-FILE.  Include specified HEADER-FILE."
  (interactive)
  (let* ((api-file
          (concat vk-wrangler-path
                  "spec/api.sxp"))
         (source-file
          (concat vk-wrangler-path
                  "src/vkwrangler.c"))
         (source-header
          (mapconcat #'identity
                     '("#include <vkwrangler/vkwrangler.h>"
                       "extern PFN_vkVoidFunction vkGetInstanceProcAddr(VkInstance instance, const char* pName);"
                       "extern PFN_vkVoidFunction vkGetDeviceProcAddr(VkDevice device, const char* pName);"
                       "\n")
                     "\n")))
    (make-directory (file-name-directory source-file) t)
    (let* ((api-contents
            (with-temp-buffer
              (insert-file-contents api-file)
              (buffer-string)))
           (api
            (vk-wrangler-filter-param-names (car (read-from-string api-contents))))
           (inst-func
            (concat "void vkWranglerInitInstanceFunctions(VkInstance instance, uint32_t enabledExtensionNameCount, const char* const* ppEnabledExtensionNames) "
                    (vk-wrangler-make-function-definition api nil)))
           (dev-func
            (concat "void vkWranglerInitDeviceFunctions(VkDevice device, uint32_t enabledExtensionNameCount, const char* const* ppEnabledExtensionNames) "
                    (vk-wrangler-make-function-definition api t)))
           (output
            (concat source-header
                    inst-func
                    "\n\n"
                    dev-func)))
      (save-excursion
        (find-file source-file)
        (delete-region (point-min) (point-max))
        (insert output)
        (indent-region (point-min) (point-max))
        (save-buffer)
        (kill-buffer)))))

(defun vk-wrangler-make ()
    "Make all C code files for vk-wrangler."
    (interactive)
    (vk-wrangler-make-header-file)
    (vk-wrangler-make-source-file))

(provide 'extension-formatter)
;;; vk-wrangler.el ends here
