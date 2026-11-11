#!/bin/sh
# SPDX-License-Identifier: GPL-2.0-only
#
# Copyright (C) 2026 Charles Moid
#
# Make a release tarball self-contained on macOS.
#
#   vendor-macos-dylibs.sh <in.tar.gz> <out.tar.gz>
#
# `include_erts` bundles the VM, but the NIFs that OTP and our deps build
# link their C libraries by ABSOLUTE path recorded at build time:
#
#   crypto.so      -> /opt/homebrew/opt/openssl@3/lib/libcrypto.3.dylib
#   enacl_nif.so   -> /opt/homebrew/opt/libsodium/lib/libsodium.26.dylib
#   wx (devpkg)    -> /opt/homebrew/opt/wxwidgets@3.2/lib/*.dylib
#
# dyld treats those as exact paths, not as a search: it never looks in
# /usr/local, so an Intel Mac fails even with the same Homebrew formulae
# installed, and a libsodium bump to .27 breaks a correctly-installed
# machine.  (Linux is fine unaided -- ld.so resolves by SONAME.)
#
# So: copy each such library in beside the object that needs it and
# rewrite the reference to @loader_path.  Co-locating rather than using a
# shared vendor dir + @rpath is what keeps this simple -- dylibs that
# depend on each other (the wx stack) land in one directory, so their
# inter-dependencies resolve by the same @loader_path/<basename> rule with
# no rpath entries to manage.
#
# It also strips build-host paths (debug symbol tables and absolute
# install names) out of the Mach-O objects, so a package does not ship the
# builder's home directory.
#
# NOTE this must run on the TARBALL, not the assembled release: relx
# re-copies every app dir during `rebar3 tar`, so anything written into
# _build/<profile>/rel/ between `release` and `tar` is silently discarded.
#
# Non-macOS is a pass-through copy.

set -eu

IN=${1:?usage: vendor-macos-dylibs.sh <in.tar.gz> <out.tar.gz>}
OUT=${2:?usage: vendor-macos-dylibs.sh <in.tar.gz> <out.tar.gz>}

if [ "$(uname -s)" != "Darwin" ]; then
    cp "$IN" "$OUT"
    echo "==> not macOS; copied through unchanged"
    exit 0
fi

STAGE=$(mktemp -d)
trap 'rm -rf "$STAGE"' EXIT INT TERM

mkdir -p "$STAGE/rel"
tar xzf "$IN" -C "$STAGE/rel"

## Both tools below are chatty on stderr about things we are doing on
## purpose ("will invalidate the code signature", "replacing existing
## signature").  Swallow that, but keep real failures: on a non-zero exit
## the captured output is printed and the build stops.
retool() {
    _out=$(install_name_tool "$@" 2>&1) || { echo "$_out" >&2; exit 1; }
}

resign() {
    _out=$(codesign -f -s - "$1" 2>&1) || { echo "$_out" >&2; exit 1; }
}

## Every Mach-O object we may need to touch or check.
objects() {
    find "$STAGE/rel" \( -name '*.so' -o -name '*.dylib' \) -type f -print
}

## Path as it will appear inside the package, for messages.
rel() {
    echo "$1" | sed "s|$STAGE/rel/||"
}

## An object's own install name (LC_ID_DYLIB).  Empty for a Mach-O bundle,
## which is how most NIFs are built.
install_name_of() {
    otool -D "$1" 2>/dev/null | tail -n +2
}

## A dependency is "external" if dyld would resolve it outside the
## release: an absolute path that is not an OS path.  @loader_path and
## @rpath references are already relative, and /usr/lib + /System are
## guaranteed present (and are protected by SIP, so they could not be
## vendored even if we wanted to).
external_deps() {
    _obj=$1
    _self=$(otool -D "$_obj" 2>/dev/null | tail -n +2)
    otool -L "$_obj" 2>/dev/null | tail -n +2 | sed 's/ (compatibility.*//;s/^[[:space:]]*//' |
        while read -r _dep; do
            [ -n "$_dep" ] || continue
            ## A dylib's own install name shows up in otool -L; not a dep.
            [ "$_dep" = "$_self" ] && continue
            case $_dep in
                /usr/lib/*|/System/*|@*) continue ;;
                /*) echo "$_dep" ;;
            esac
        done
}

## Rewrite one object's external references to @loader_path, copying each
## library in beside it.  Newly copied libraries are echoed so the driver
## can process them in turn.
##
## Deliberately NOT recursive: POSIX sh has no `local`, so a recursive
## call overwrites its caller's variables -- which silently left the
## calling object unsigned and signed the callee twice.
vendor_obj() {
    _obj=$1
    _dir=$(dirname "$_obj")
    _touched=0

    ## OTP ships its NIFs read-only (444).  install_name_tool rewrites via
    ## a temp file and rename, so it does not care, but codesign writes in
    ## place and fails without u+w.  Restore the mode afterwards so the
    ## package matches an untouched build.
    _mode=$(stat -f '%Lp' "$_obj")
    chmod u+w "$_obj"

    for _dep in $(external_deps "$_obj"); do
        _base=$(basename "$_dep")
        _local="$_dir/$_base"
        if [ ! -f "$_local" ]; then
            if [ ! -f "$_dep" ]; then
                echo "ERROR: $_obj needs $_dep, which is not on this machine." >&2
                echo "       Install it (brew) and rebuild -- otherwise the" >&2
                echo "       package cannot be made self-contained." >&2
                exit 1
            fi
            cp "$_dep" "$_local"
            chmod u+w "$_local"
            ## Its own install name is still the absolute build-host path;
            ## point it at itself so nothing re-derives the old location.
            ## Sign it now: that -id is a modification, and a library with
            ## no external deps of its own would otherwise never be signed.
            retool -id "@loader_path/$_base" "$_local"
            resign "$_local"
            echo "    + $_base ($(du -h "$_local" | cut -f1 | tr -d ' '))"
            echo "$_local" >>"$STAGE/discovered"
        fi

        retool -change "$_dep" "@loader_path/$_base" "$_obj"
        _touched=1
    done

    ## install_name_tool invalidates the existing (ad-hoc, linker-signed)
    ## signature, and arm64 dyld refuses to load an invalidly signed image
    ## -- it kills the process rather than reporting a missing library, so
    ## an unsigned object surfaces as a baffling signature error at NIF
    ## load time.  A signature covers only its own file, so signing here
    ## stays valid when a dependency is signed later.
    [ "$_touched" = 1 ] && resign "$_obj"

    chmod "$_mode" "$_obj"
}

echo "==> vendoring macOS libraries into $(basename "$OUT")"

## Worklist: seed with the release's own objects, then keep draining
## whatever gets copied in, so a library that itself links another
## non-system library (the wx stack does) is followed to closure.
PENDING=$(objects)
while [ -n "$PENDING" ]; do
    : >"$STAGE/discovered"
    for obj in $PENDING; do
        if [ -n "$(external_deps "$obj")" ]; then
            echo "  $(echo "$obj" | sed "s|$STAGE/rel/||")"
            vendor_obj "$obj"
        fi
    done
    PENDING=$(cat "$STAGE/discovered")
done
rm -f "$STAGE/discovered"

## Strip build-host traces.  Two of them, neither consulted at runtime but
## both shipped to every user:
##
##   - the debug symbol table records absolute C source paths, e.g.
##     /Users/<you>/code/erlbutt/_build/.../enacl/c_src/aead.c
##   - a NIF built as a dylib rather than a bundle (esqlite3_nif.so)
##     carries an absolute LC_ID_DYLIB pointing into the build tree
##
## The install name is genuinely unused -- erlang:load_nif/2 dlopens by a
## path it computes from the app's priv dir, so nothing ever consults it
## (verified by relocating the .so and loading it) -- but it is the
## builder's home directory either way.
##
## `strip -S` drops debug symbols only.  Deliberately NOT -x: that also
## removes local symbols, which buys nothing here and is a bigger change
## to make to third-party dylibs.  Every global symbol, `nif_init`
## included, survives -S.
echo "==> stripping build-host paths"
for obj in $(objects); do
    _mode=$(stat -f '%Lp' "$obj")
    chmod u+w "$obj"

    _id=$(install_name_of "$obj")
    case ${_id:-} in
        ""|@*|/usr/lib/*|/System/*) ;;
        /*) retool -id "@loader_path/$(basename "$obj")" "$obj"
            echo "  $(rel "$obj") (install name)" ;;
    esac

    _serr=$(strip -S "$obj" 2>&1) || { echo "$_serr" >&2; exit 1; }
    resign "$obj"
    chmod "$_mode" "$obj"
done

## Verify 1: nothing anywhere in the tree may still point outside the
## release.  otool is exactly what dyld will consult at load time.
REMAINING=$(for obj in $(objects); do external_deps "$obj"; done | sort -u)
if [ -n "$REMAINING" ]; then
    echo "ERROR: external references remain after vendoring:" >&2
    echo "$REMAINING" | sed 's/^/       /' >&2
    exit 1
fi

## Verify 2: every object still has a valid signature.  A stock build has
## all of them valid (ad-hoc, linker-signed), so any invalid one here is
## an install_name_tool edit we failed to re-sign -- which would abort the
## process at load time on arm64 rather than degrade.
BADSIG=$(for obj in $(objects); do
             codesign -v "$obj" >/dev/null 2>&1 || echo "$obj"
         done | sed "s|$STAGE/rel/||")
if [ -n "$BADSIG" ]; then
    echo "ERROR: invalid code signature after rewriting:" >&2
    echo "$BADSIG" | sed 's/^/       /' >&2
    exit 1
fi

## Verify 3: no Mach-O object may still carry the build tree's path.
## BUILD_ROOT defaults to the directory make was run from, which is the
## repo root and the prefix of every path the compilers baked in.
##
## Scoped to Mach-O on purpose: devpkg's .beam files legitimately contain
## absolute source paths, because that profile keeps debug_info and
## include_src for live development.  That is the package's purpose, not a
## defect, and stripping it would be self-defeating.
BUILD_ROOT=${BUILD_ROOT:-$PWD}
LEAKED=$(for obj in $(objects); do
             grep -al "$BUILD_ROOT" "$obj" 2>/dev/null || true
         done | sed "s|$STAGE/rel/||")
if [ -n "$LEAKED" ]; then
    echo "ERROR: build path ($BUILD_ROOT) still present in:" >&2
    echo "$LEAKED" | sed 's/^/       /' >&2
    exit 1
fi

tar czf "$OUT" -C "$STAGE/rel" .
echo "==> self-contained: no external refs, no build paths, signatures valid"
