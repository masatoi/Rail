;;; rail-test-helper.el --- Test helper for test suites

;; Copyright © 2022 Fermin MF
;;
;; Author: Fermin MF <fmfs@posteo.net>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'vc-git)

(defvar rail-test-helper-buffer "*rail-test-server*")

(cl-defmacro rail-test-helper-request-wrapper (&body body)
  `(progn
     (rail-test-helper-launch-server)
     ,@body
     (rail-test-helper-shutdown-server)))

(defun rail-test-helper-launch-server ()
  (let* ((name "python-nrepl")
	 (url (format "https://gitlab.com/sasanidas/%s.git" name)))
    
    (message "Downloading %s..." name)
    (unless (directory-empty-p name)
      (vc-git-clone url name nil))

    (shell-command (format "cd %s && poetry install" name))
    (start-process-shell-command "nrepl-python-server"
				 (get-buffer-create rail-test-helper-buffer)
				 (format "cd %s && make debug" name))))

(defun rail-test-helper-shutdown-server ()
  (ignore-errors
    (kill-process (get-buffer-process rail-test-helper-buffer))
    (with-current-buffer rail-test-helper-buffer
      (setq-local kill-buffer-query-functions
		  (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
      (set-buffer-modified-p nil)
      (kill-buffer rail-test-helper-buffer))))

(defun rail-test-helper-debug-info ()
  (with-current-buffer rail-test-helper-buffer
    (buffer-substring-no-properties (point-min) (point-max))))


(provide 'rail-test-helper)

;;; rail-test-helper.el ends here