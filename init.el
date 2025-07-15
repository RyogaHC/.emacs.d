(require 'package)

(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")))

(unless package-archive-contents
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)

(use-package paredit
  :ensure t)

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package ddskk
  :ensure t
  :config
  (setq default-input-method "japanese-skk"))
  ;(setq skk-use-act t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(random t)

(defun generate-random (keta)
  (+ (random (- (* 9 (expt 10 (- keta 1))) 1)) (expt 10 (- keta 1))))

(defun anzan (keta kaisu second)
  (interactive "n桁数:\nn回数:\nn表示時間:(s/number)")
  (let* ((random-list (mapcar 'generate-random (make-list kaisu keta)))
	 (sum (apply '+ random-list)))
    (message "")
    (sit-for 1)
    (dolist (random-number random-list)
      (message (int-to-string random-number))
      (sit-for second)
      (message "")
      (sit-for 0.05))
    (if (= (read-number "答え:") sum)
      (message "正解")
      (message (format "不正解だ。練習したまえ。(正解は「%d」)" sum)))))

(defun anzan-loop (keta kaisu second)
  (interactive "n桁数:\nn回数:\nn表示時間:(s/number)")
  (anzan keta kaisu second)
  (read-char)
  (anzan-loop keta kaisu second))

(global-display-line-numbers-mode 1)
(electric-pair-mode 1)
(setq inhibit-startup-screen t)
(display-time-mode 1)
