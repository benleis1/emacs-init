(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-solarized-light))
 '(custom-safe-themes
   '("ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e" "6f96a9ece5fdd0d3e04daea6aa63e13be26b48717820aa7b5889c602764cf23a" default))
 '(excorporate-configuration
   '(("resource-url" . "https://outlook.office365.com/EWS/Exchange.asmx")
     ("resource-url-prefixes" "https://outlook.office365.com/EWS/")
     ("authorization-endpoint" . "https://login.microsoftonline.com/servicenow.com/oauth2/authorize")
     ("access-token-endpoint" . "https://login.microsoftonline.com/servicenow.com/oauth2/token")
     ("client-identifier" . "751cf8be-ca07-484b-9308-fac4b9d85eff")
     ("scope" . "openid offline_access profile Mail.ReadWrite Mail.ReadWrite.Shared Mail.Send Mail.Send.Shared Calendars.ReadWrite Calendars.ReadWrite.Shared Contacts.ReadWrite Contacts.ReadWrite.Shared Tasks.ReadWrite Tasks.ReadWrite.Shared MailboxSettings.ReadWrite People.Read User.ReadBasic.All")
     ("authorization-extra-arguments"
      ("resource" . "https://outlook.office.com")
      ("response_mode" . "query")
      ("login_hint" . "benjamin.leis@servicenow.com")
      ("prompt" . "login")
      ("redirect_uri" . "https://login.microsoftonline.com/common/oauth2/nativeclient")
      ("" . ""))
     ("" . "")))
 '(imenu-list-mode-line-format
   '("%e" mode-line-frame-identification "  "
     (:eval
      (buffer-name imenu-list--displayed-buffer))
     " outline  sort:"
     (:eval
      (my/imenu-current-sort))
     mode-line-end-spaces))
 '(lsp-java-server-install-dir "/Users/benjamin.leis/.emacs.d/eclipse.jdt.ls/")
 '(lsp-treemacs-symbols-sort-functions '(lsp-treemacs-sort-by-name))
 '(markdown-header-scaling-values '(1.5 1.3 1.2 1.1 1.0 1.0))
 '(org-agenda-files
   '("~/org/daily-meetings.org" "/Users/benjamin.leis/org/current.org"))
 '(org-support-shift-select t)
 '(package-selected-packages
   '(corfu marginalia vertico org-make-toc undo-tree outshine outorg vterm centaur-tabs windata tree-mode groovy-mode htmlize org-preview-html stripe-buffer org-modern org-side-tree org-autolist excorporate neotree imenu-list markdown-toc dashboard all-the-icons-dired doom-modeline esup spaceline yasnippet doom-themes lsp-java lsp-treemacs lsp-ui lsp-mode which-key latex-math-preview latex-preview-pane auctex markdown-mode company org-bullets mixed-pitch sqlformat))
 '(use-system-tooltips nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#FDF6E3" :foreground "#556b72" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 170 :width normal :foundry "nil" :family "DejaVu Sans Mono for Powerline"))))
 '(doom-modeline ((t (:family "Helvetica"))))
 '(doom-modeline-buffer-major-mode ((t (:inherit (doom-modeline-emphasis bold) :background "dark gray" :foreground "gray0"))))
 '(doom-modeline-project-dir ((t (:inherit (doom-modeline font-lock-string-face bold) :foreground "gray52"))))
 '(font-lock-function-call-face ((t (:inherit default))))
 '(font-lock-variable-use-face ((t (:inherit nil))))
 '(imenu-list-entry-face ((t (:family "DejaVu Sans Mono for Powerline"))))
 '(imenu-list-entry-face-0 ((t (:inherit imenu-list-entry-face :foreground "#556B72"))))
 '(imenu-list-entry-face-1 ((t (:inherit imenu-list-entry-face :foreground "#556B72"))))
 '(imenu-list-entry-face-2 ((t (:inherit imenu-list-entry-face :foreground "#556B72"))))
 '(imenu-list-entry-face-3 ((t (:inherit imenu-list-entry-face))))
 '(lsp-bridge-alive-mode-line ((t (:inherit doom-modeline :weight bold))))
 '(lsp-face-semhl-class ((t (:inherit default))))
 '(lsp-face-semhl-type ((t (:inherit default))))
 '(lsp-headerline-breadcrumb-path-face ((t (:inherit default :family "Helvetica"))))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:inherit font-lock-doc-face :foreground "grey30" :weight bold))))
 '(marginalia-key ((t nil)))
 '(marginalia-size ((t (:inherit default :foreground "#556B72"))))
 '(markdown-bold-face ((t (:inherit bold :foreground "black"))))
 '(markdown-gfm-checkbox-face ((t (:inherit font-lock-builtin-face :foreground "#556B72" :slant normal :height 1.4))))
 '(markdown-header-face ((t (:inherit bold :foreground "gray0" :underline nil :height 1.5 :family "helvetica"))))
 '(markdown-italic-face ((t (:inherit italic :foreground "#556b72"))))
 '(markdown-list-face ((t (:foreground "gray0"))))
 '(markdown-pre-face ((t (:foreground "#2aa198" :family "DejaVu Sans Mono for Powerline"))))
 '(markdown-strike-through-face ((t (:strike-through t :height 1.0))))
 '(markdown-table-face ((t (:inherit markdown-code-face :family "DejaVu Sans Mono for Powerline"))))
 '(markdown-url-face ((t (:foreground "gray0" :weight normal))))
 '(mode-line ((t (:background "#f0e9d7" :box nil :family "helvetica"))))
 '(mode-line-inactive ((t (:background "#f6efdd" :foreground "#96A7A9" :box nil :family "helvetica"))))
 '(org-checkbox ((t (:inherit org-todo :height 1.7))))
 '(org-date ((t (:foreground "DodgerBlue1"))))
 '(org-level-1 ((t (:height 1.3 :weight bold :foreground "black"))))
 '(org-level-2 ((t (:foreground "dim gray" :weight bold :height 1.2))))
 '(org-level-3 ((t (:height 1.1 :weight bold :foreground "light slate gray"))))
 '(org-level-4 ((t (:inherit outline-4 :extend nil :foreground "light slate gray"))))
 '(org-level-5 ((t (:inherit outline-5 :extend nil :foreground "light slate gray"))))
 '(org-link ((t (:inherit link :foreground "RoyalBlue3" :weight medium))))
 '(org-modern-label ((t (:box (:line-width (-1 . -3) :color "#FDF6E3") :underline nil :weight regular :height 1.0 :width condensed))))
 '(org-special-keyword ((t (:foreground "Black"))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#6c71c4" :height 1.0 :family "DejaVu Sans Mono for Powerline"))))
 '(tab-line ((t (:background "#DED8C5" :foreground "#EEE8D5" :box (:line-width (4 . 4) :color "#ded8c5") :weight bold :height 1.3 :family "Helvetica"))))
 '(tab-line-tab-current ((t (:inherit tab-line-tab))))
 '(tab-line-tab-inactive ((t (:inherit mode-line-default :box nil))))
 '(tab-line-tab-modified ((t (:inherit nil :foreground "#6b8787" :slant normal :weight bold :family "DejaVu "))))
 '(tooltip ((t (:background "#EEE8D5" :foreground "#556b72" :height 1.5))))
 '(treemacs-root-face ((t (:foreground "gray42" :weight bold :height 1.2 :family "Deja Vu Sans Mono")))))
