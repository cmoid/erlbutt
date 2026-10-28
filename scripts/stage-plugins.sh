#!/bin/sh
# SPDX-License-Identifier: GPL-2.0-only
#
# Copyright (C) 2026 Charles Moid
#
# Build the plugins a package ships, and stage them where the release
# overlay copies them from.
#
#   PLUGINS="../ssb_git ..." stage-plugins.sh
#
# Run by rebar3 before every release and tar (rebar.config pre_hooks), from
# the erlbutt root.  PLUGINS is a space-separated list of plugin source
# trees; the Makefile sets it to ../ssb_git when that exists, and unset
# means a package with no plugins.
#
# A plugin is a rebar3 project depending on `ssb`.  It is built against
# THIS tree's apps/ssb (a _checkouts symlink, which rebar3 uses in place of
# the declared dep) with this tree's rebar3, so its beams match the node
# they ship with.  Then _build/plugins/<app>/ gets its ebin, its priv if
# any, and its bin/ if any (ssb_git's git-ssb launcher), which the overlay
# copies to <release>/plugins/, with its elisp/ if it has one (ssb_git's
# Emacs front end).  plugin_loader starts them at boot.
#
# _build/plugins always exists, with a README, so the overlay never has a
# missing source to fail on.
set -eu

top=$(pwd -P)
out=$top/_build/plugins

rm -rf "$out"
mkdir -p "$out"
# relx's overlay copies into an existing release directory without
# clearing it, so a plugin from an earlier build would survive one built
# without it, and go into the tarball.  Clear every profile's copy; the
# overlay recreates it from $out.
rm -rf "$top"/_build/*/rel/ssb/plugins
cat > "$out/README" <<'EOF'
Plugins shipped with this erlbutt package: OTP applications started at
boot by plugin_loader, after the node itself.

To add one, put its directory (holding ebin/<app>.app) in <data>/plugins/
instead; a plugin there replaces one of the same name here, and survives
an upgrade.  {plugins, [App, ...]}. in <data>/ssb.cfg chooses which start;
{plugins, []}. starts none.
EOF

for src in ${PLUGINS:-}; do
    src=$(cd "$src" && pwd -P)
    set -- "$src"/src/*.app.src
    [ "$#" -eq 1 ] && [ -f "$1" ] || {
        echo "stage-plugins: $src has no single src/*.app.src" >&2; exit 1; }
    app=$(basename "$1" .app.src)

    echo "===> Building plugin $app from $src"
    mkdir -p "$src/_checkouts"
    ln -sfn "$top/apps/ssb" "$src/_checkouts/ssb"
    (cd "$src" && "$top/rebar3" compile)

    lib=$src/_build/default/lib/$app
    mkdir -p "$out/$app"
    cp -R "$lib/ebin" "$out/$app/ebin"
    [ -d "$lib/priv" ] && cp -R "$lib/priv" "$out/$app/priv"
    [ -d "$src/bin" ]  && cp -R "$src/bin"  "$out/$app/bin"
    if [ -d "$src/elisp" ]; then
        mkdir -p "$out/$app/elisp"
        cp "$src"/elisp/*.el "$out/$app/elisp/"
    fi
done
