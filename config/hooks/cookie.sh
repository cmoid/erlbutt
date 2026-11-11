# SPDX-License-Identifier: GPL-2.0-only
#
# Copyright (C) 2026 Charles Moid
#
# relx pre_start hook: make sure the node has a real distribution cookie.
#
# vm.args.prod carries no -setcookie, so the node uses $HOME/.erlang.cookie
# like any other BEAM — one per account, never in git, and never in a
# process listing (a -setcookie in vm.args shows up in `ps` on the beam
# process; the file is read by the VM itself).  Local tools running as
# the same user (bin/ssb remote_console, maxbutt/distel) find it with no
# configuration at all.
#
# This hook exists for two reasons:
#
#  1. SSB_COOKIE still works.  If it is set, it becomes the cookie file's
#     contents, so an install that keeps it in /etc/erlbutt/env upgrades
#     without its cookie changing under maxbutt or sbutt-console.
#
#  2. `bin/ssb daemon` reads the cookie BEFORE pre_start hooks run, then
#     waits for the node with erl_rpc.  On a fresh install with no cookie
#     file the VM would create one, but the script would still be holding
#     an empty cookie and wait forever.  Creating the file here, and
#     setting COOKIE, closes that.
#
# It is SOURCED by the start script (relx_run_hooks), so it runs in that
# shell under `set -e`: no `exit` except on purpose, and it may set the
# script's COOKIE variable.  It does nothing when vm.args has its own
# -setcookie (the dev and devpkg builds), so it never writes a cookie file
# the release did not ask for.

if ! grep -q '^-setcookie' "$VMARGS_PATH"; then
    _ssb_cookie_file="$HOME/.erlang.cookie"

    # Chained with && on purpose: set -e does not apply inside a command
    # on the left of ||, so each step has to carry its own failure out.
    _ssb_cookie_write() {
        ( umask 077 &&
          rm -f "$_ssb_cookie_file" &&
          printf '%s' "$1" > "$_ssb_cookie_file" &&
          chmod 400 "$_ssb_cookie_file" ) || {
            echo "erlbutt: cannot write $_ssb_cookie_file — refusing to start" \
                 "with a cookie the node and its tools would not agree on" >&2
            exit 1
        }
    }

    if [ -n "${SSB_COOKIE:-}" ]; then
        if [ ! -f "$_ssb_cookie_file" ] ||
           [ "$(cat "$_ssb_cookie_file")" != "$SSB_COOKIE" ]; then
            echo "erlbutt: writing SSB_COOKIE to $_ssb_cookie_file" >&2
            _ssb_cookie_write "$SSB_COOKIE"
        fi
    elif [ ! -f "$_ssb_cookie_file" ]; then
        # The same shape the VM generates (uppercase letters), but longer.
        _ssb_cookie_write "$(LC_ALL=C tr -dc 'A-Z' < /dev/urandom | head -c 32)"
    fi

    COOKIE="$(cat "$_ssb_cookie_file")"
    unset -f _ssb_cookie_write
    unset _ssb_cookie_file
fi
