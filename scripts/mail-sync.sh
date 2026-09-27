#!/bin/sh
# Keep the latest output for each account, and try all accounts on failure.
# Account identifiers are supplied by the private mu4e configuration.
if [ "$#" -eq 0 ]; then
    printf 'Usage: %s ACCOUNT [ACCOUNT ...]\n' "$0" >&2
    exit 2
fi
for account in "$@"; do
    case "$account" in
        ''|*[!a-zA-Z0-9_-]*)
            printf 'Invalid mail account: %s\n' "$account" >&2; exit 2 ;;
    esac
    if [ ! -r "$HOME/.config/isync/$account-mbsyncrc" ]; then
        printf 'Missing sync configuration for account: %s\n' "$account" >&2
        exit 2
    fi
done
umask 077
log_dir="${XDG_STATE_HOME:-$HOME/.local/state}/mail/sync"
mkdir -p "$log_dir" || exit 1
status=0
for account in "$@"; do
    config="$HOME/.config/isync/$account-mbsyncrc"
    log="$log_dir/$account.log"
    timeout 120s mbsync -c "$config" -a >"$log" 2>&1
    result=$?
    printf '\n[%s] exit=%s; log=%s\n' "$account" "$result" "$log"
    cat "$log"
    if [ "$result" -ne 0 ]; then
        status=1
    fi
done
exit "$status"
