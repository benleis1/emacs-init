;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-solarized-light))
 '(custom-safe-themes
   '("f38f24d7468df32f8945a0572c856223f8351b8ea050b717ce36b0051399e3ad"
     "8e8acbea18dbf51b41a09e7fc789f8af666b2ca8c099a268b5275bfc4541a5ff"
     "a8c1252f9844caf313a2315ecf1e8ef4d92495c9f2067d875bb1c783b08719ad"
     "7c0179ef765b9bf1d79612235236d29eebcaca9b4a287ff802959ed20db154b6"
     "05f1ee9db2c66cd715ab6d36ff949386c47dfff91a7df1f203d015b3ea304dbb"
     "998bc02f2e52205ad06df88a14d53168aef1ec1bbcc6fe2b0cd15fed8e5c8dae"
     "e0fbe5caa6e602975e59cdd381c9773a670a864dd7bb7bf2345414856148098b"
     "138ed99a323c1b93c52f4b3726caf2bc634b79a76fa63a3d3aff76394db5f28f"
     "2493d0ad0bb94bd2ad297a6d76288751a532fd6d8d6af694ac14008caa6b7fa2"
     "10e330880269244ae45ae9e02fe6f55766da9e15036e7c7f07d7ce228195deb5"
     "5cb84685a211fb46e47ca355dc91e52adf0c185dc0603cfe27c63855f200dd1f"
     "97239bdf95302306bb8b1a4b18f7a351d1787ca0a1ec87c004b43fd99a01d59f"
     "0f1341c0096825b1e5d8f2ed90996025a0d013a0978677956a9e61408fcd2c77"
     "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c"
     "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
     "6f96a9ece5fdd0d3e04daea6aa63e13be26b48717820aa7b5889c602764cf23a"
     default))
 '(excorporate-configuration
   '(("resource-url" . "https://outlook.office365.com/EWS/Exchange.asmx")
     ("resource-url-prefixes" "https://outlook.office365.com/EWS/")
     ("authorization-endpoint"
      . "https://login.microsoftonline.com/servicenow.com/oauth2/authorize")
     ("access-token-endpoint"
      . "https://login.microsoftonline.com/servicenow.com/oauth2/token")
     ("client-identifier" . "751cf8be-ca07-484b-9308-fac4b9d85eff")
     ("scope"
      . "openid offline_access profile Mail.ReadWrite Mail.ReadWrite.Shared Mail.Send Mail.Send.Shared Calendars.ReadWrite Calendars.ReadWrite.Shared Contacts.ReadWrite Contacts.ReadWrite.Shared Tasks.ReadWrite Tasks.ReadWrite.Shared MailboxSettings.ReadWrite People.Read User.ReadBasic.All")
     ("authorization-extra-arguments"
      ("resource" . "https://outlook.office.com")
      ("response_mode" . "query")
      ("login_hint" . "benjamin.leis@servicenow.com")
      ("prompt" . "login")
      ("redirect_uri"
       . "https://login.microsoftonline.com/common/oauth2/nativeclient")
      ("" . ""))
     ("" . "")))
 '(markdown-header-scaling-values '(1.5 1.3 1.2 1.1 1.0 1.0))
 '(org-agenda-files
   '("~/org/daily-meetings.org" "/Users/benjamin.leis/org/current.org"))
 '(package-selected-packages
   '(all-the-icons-dired consult corfu doom-modeline doom-themes
			 ef-themes ef-theses eglot-booster excorporate
			 folio folio-theme groovy-mode grove
			 imenu-list lsp-java lsp-ui magit marginalia
			 markdown-mermaid markdown-toc
			 markdown-ts-mode math-preview mixed-pitch
			 modus-themes nerd-icons-dired orderless
			 org-autolist org-modern org-pretty-table
			 org-roam sqlformat stripe-buffer treemacs
			 treesit-fold undo-tree vc-use-package vertico
			 wikimode yasnippet))
 '(package-vc-selected-packages
   '((modus-themes :url "https://github.com/protesilaos/modus-themes.git")
     (treesit-fold :url
		   "https://github.com/emacs-tree-sitter/treesit-fold")
     (wikimode :url "https://github.com/benleis1/wikimode")
     (org-pretty-table :url
		       "https://github.com/Fuco1/org-pretty-table")
     (vc-use-package :vc-backend Git :url
		     "https://github.com/slotThe/vc-use-package")))
 '(treesit-fold-summary-format " %s "))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :foreground "gray33" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 160 :width normal :foundry "nil" :family "DejaVuSansM Nerd Font"))))
 '(cursor ((t (:background "dark gray"))))
 '(font-lock-constant-face ((t (:foreground "gray33" :weight bold))))
 '(font-lock-function-call-face ((t (:inherit nil :foreground "gray33" :slant italic))))
 '(font-lock-type-face ((t (:foreground "gray33" :slant italic))))
 '(font-lock-variable-use-face ((t (:inherit font-lock-variable-name-face :foreground "gray33"))))
 '(imenu-list-entry-face ((t (:family "DejaVu Sans Mono for Powerline"))))
 '(imenu-list-entry-face-0 ((t (:inherit imenu-list-entry-face :foreground "#556b72"))))
 '(imenu-list-entry-face-1 ((t (:inherit imenu-list-entry-face :foreground "#556b72"))))
 '(imenu-list-entry-face-2 ((t (:inherit imenu-list-entry-face :foreground "#556b72"))))
 '(imenu-list-entry-face-3 ((t (:inherit imenu-list-entry-face))))
 '(line-number ((t (:inherit default :background "#EEE8D5" :foreground "#96A7A9" :weight normal))))
 '(markdown-code-face ((t (:extend t :background "#f2e7d0" :weight normal :family "Space Mono for Powerline"))))
 '(markdown-header-face ((t (:inherit bold :foreground "gray0" :underline nil :height 1.5 :family "helvetica"))))
 '(markdown-italic-face ((t (:inherit italic :foreground "gray27"))))
 '(markdown-link-face ((t (:foreground "goldenrod4"))))
 '(markdown-list-face ((t (:foreground "gray0"))))
 '(markdown-metadata-key-face ((t (:foreground "black" :weight medium))))
 '(markdown-pre-face ((t (:foreground "SlateGray4" :family "DejaVu Sans Mono for Powerline"))))
 '(markdown-table-face ((t (:inherit markdown-code-face :family "DejaVu Sans Mono for Powerline"))))
 '(markdown-url-face ((t (:foreground "gray0" :weight normal))))
 '(mode-line-active ((t (:inherit mode-line :background "cornsilk3" :box (:line-width (2 . 1) :color "tan" :style pressed-button)))))
 '(org-checkbox ((t (:inherit org-todo :height 1.7))))
 '(org-date ((t (:foreground "DodgerBlue1"))))
 '(org-document-info ((t (:foreground "black"))))
 '(org-document-title ((t (:foreground "black" :weight bold))))
 '(org-level-1 ((t (:height 1.3 :weight bold :foreground "black"))))
 '(org-level-2 ((t (:foreground "dim gray" :weight bold :height 1.2))))
 '(org-level-3 ((t (:height 1.1 :weight bold :foreground "light slate gray"))))
 '(org-level-4 ((t (:inherit outline-4 :extend nil :foreground "light slate gray"))))
 '(org-level-5 ((t (:inherit outline-5 :extend nil :foreground "light slate gray"))))
 '(org-link ((t (:inherit link :foreground "DodgerBlue1"))))
 '(org-modern-label ((t (:box (:line-width (-1 . -3) :color "#FDF6E3") :underline nil :weight regular :height 1.0 :width condensed))))
 '(org-special-keyword ((t (:foreground "Black"))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#6c71c4" :height 1.0 :family "DejaVu Sans Mono for Powerline"))))
 '(shadow ((t (:foreground "gray56"))))
 '(stripe-hl-line ((t (:background "DarkOrange2" :foreground "ivory"))))
 '(tab-line ((t (:background "#EEE8D5" :height 1.3 :family "San Francisco (SF Pro)"))))
 '(tab-line-tab-modified ((t (:inherit font-lock-doc-face :foreground "DarkOrange1" :weight bold :family "DejaVu "))))
 '(tooltip ((t (:background "#EEE8D5" :foreground "#556b72" :height 1.3))))
 '(treesit-fold-replacement-face ((t (:background "goldenrod4" :foreground "white" :box (:line-width (1 . -1) :color "black" :style pressed-button)))))
 '(vertical-border ((t (:background "gray50" :foreground "gray50")))))
