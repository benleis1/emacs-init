#! /usr/bin/env bash
set -euo pipefail

src="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
dest="${EMACS_DIR:-$HOME/.emacs.d}"

usage() {
  cat <<EOF
Usage: $(basename "$0") [-d DIR] [-h]

Link this Emacs configuration into an Emacs directory and check for the
external tools it relies on.

Options:
  -d, --dir DIR   Install into DIR instead of ~/.emacs.d
  -h, --help      Show this help and exit

Environment:
  EMACS_DIR       Same as --dir (the command-line option takes precedence)

Installs:
  DejaVuSansM Nerd Font   required by early-init.el for a GUI frame; installed
                          via Homebrew (macOS) or a download from the
                          nerd-fonts releases (Linux) if missing

Checks (warnings only, nothing is installed for you):
  aspell                  spell checking (falls back to ispell)
  pg_format (pgformatter) SQL formatting commands
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    -h|--help)
      usage
      exit 0
      ;;
    -d|--dir)
      if [ $# -lt 2 ] || [ -z "$2" ]; then
        echo "error: $1 requires a directory argument" >&2
        exit 2
      fi
      dest="$2"
      shift 2
      ;;
    --dir=*)
      dest="${1#--dir=}"
      shift
      ;;
    *)
      echo "error: unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

case "$dest" in
  "~") dest="$HOME" ;;
  "~/"*) dest="$HOME/${dest#"~/"}" ;;
esac

echo "Linking the config files into $dest"
mkdir -p "$dest"
mkdir -p "$dest/lisp"

for f in early-init.el init.el; do
  ln -sfn "$src/$f" "$dest/$f"
done

for f in "$src"/lisp/*.el; do
  case "$f" in *-tests.el|*/tests.el|*-theme.el) continue ;; esac
  ln -sfn "$f" "$dest/lisp/$(basename "$f")"
done

for f in funnel4.png funnel4-dark.png desktop2.png close.png close-dark.png; do
  cp "$src/icons/$f" "$dest/lisp"
done

font_installed() {
  if command -v fc-list >/dev/null 2>&1; then
    fc-list | grep -i "DejaVuSansM.*Nerd" >/dev/null
  else
    find "$HOME/Library/Fonts" /Library/Fonts /System/Library/Fonts \
         "$HOME/.local/share/fonts" /usr/share/fonts /usr/local/share/fonts \
         -iname "DejaVuSansM*Nerd*" 2>/dev/null | grep . >/dev/null
  fi
}

install_font() {
  if [ "$(uname -s)" = "Darwin" ]; then
    command -v brew >/dev/null 2>&1 || return 1
    brew install --cask font-dejavu-sans-mono-nerd-font
  else
    command -v curl >/dev/null 2>&1 && command -v unzip >/dev/null 2>&1 || return 1
    local fontdir="$HOME/.local/share/fonts" tmp
    tmp="$(mktemp -d)"
    mkdir -p "$fontdir"
    curl -fsSL -o "$tmp/DejaVuSansMono.zip" \
      https://github.com/ryanoasis/nerd-fonts/releases/latest/download/DejaVuSansMono.zip &&
      unzip -qo "$tmp/DejaVuSansMono.zip" '*.ttf' -d "$fontdir" || { rm -rf "$tmp"; return 1; }
    rm -rf "$tmp"
    ! command -v fc-cache >/dev/null 2>&1 || fc-cache -f "$fontdir"
  fi
}

if ! font_installed; then
  echo "Installing 'DejaVuSansM Nerd Font'"
  if ! install_font || ! font_installed; then
    echo "WARNING: could not install 'DejaVuSansM Nerd Font'. early-init.el requires it;" >&2
    echo "         without it Emacs falls back to a terminal frame instead of a GUI one." >&2
    echo "         macOS: brew install --cask font-dejavu-sans-mono-nerd-font" >&2
    echo "         Linux: download DejaVuSansMono.zip from github.com/ryanoasis/nerd-fonts/releases," >&2
    echo "                unzip into ~/.local/share/fonts, then run fc-cache -f" >&2
  fi
fi

if ! command -v aspell >/dev/null 2>&1; then
  echo "WARNING: 'aspell' not found. Flyspell will fall back to ispell, if present." >&2
  echo "         macOS: brew install aspell" >&2
  echo "         Linux: sudo apt install aspell aspell-en   (or your distro's equivalent)" >&2
fi

if ! command -v pg_format >/dev/null 2>&1; then
  echo "WARNING: 'pg_format' (pgformatter) not found. The SQL formatting commands won't work." >&2
  echo "         macOS: brew install pgformatter" >&2
  echo "         Linux: sudo apt install pgformatter   (or your distro's equivalent)" >&2
fi

# run diagnostics after everything else is done
if command -v emacs >/dev/null 2>&1; then
  echo
  log="$(mktemp)"
  if ! emacs --batch -l "$dest/lisp/config-diagnostics.el" \
       -f my-config-diagnostics </dev/null 2>"$log"; then
    echo "WARNING: could not run my-config-diagnostics:" >&2
    cat "$log" >&2
  fi
  rm -f "$log"
fi
