#! /usr/bin/env bash
set -euo pipefail

src="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
dest="$HOME/.emacs.d"

echo "Linking the config files into $dest"
mkdir -p "$dest"

for f in early-init.el init.el macos.el linux.el modeline.el tab-config.el dape-java.el; do
  ln -sfn "$src/$f" "$dest/$f"
done

for f in funnel4.png desktop2.png close.png; do
  cp "$src/$f" "$dest/"
done

font_installed() {
  if command -v fc-list >/dev/null 2>&1; then
    fc-list | grep -qi "DejaVuSansM.*Nerd"
  else
    find "$HOME/Library/Fonts" /Library/Fonts /System/Library/Fonts \
         "$HOME/.local/share/fonts" /usr/share/fonts /usr/local/share/fonts \
         -iname "DejaVuSansM*Nerd*" 2>/dev/null | grep -q .
  fi
}

if ! font_installed; then
  echo "WARNING: 'DejaVuSansM Nerd Font' not found. early-init.el requires it;" >&2
  echo "         without it Emacs falls back to a terminal frame instead of a GUI one." >&2
  echo "         macOS: brew install --cask font-dejavu-sans-mono-nerd-font" >&2
  echo "         Linux: download DejaVuSansMono.zip from github.com/ryanoasis/nerd-fonts/releases," >&2
  echo "                unzip into ~/.local/share/fonts, then run fc-cache -f" >&2
fi
