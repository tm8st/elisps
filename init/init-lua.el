;;;
;;; lua-mode settings
;;;--------------------------------------------------------------------------------

(my-require 'lua-mode)

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(provide 'init-lua)
