GUIX_PROFILE=$HOME/.emacs.d/guix/pull-profile
. "$GUIX_PROFILE/etc/profile"
GUIX_PROFILE=$HOME/.emacs.d/guix/package-profile
. "$GUIX_PROFILE/etc/profile"

emacs $@
