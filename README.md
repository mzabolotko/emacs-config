emacs-config
============

The literal configuration is used.
To use the direct-based configuration, you must use the file Emacs.org.
To use the Gui-based configuration, you must use the file Emacs-Guix.org.

To generate a configuration file from the selected org file, open it and press the key combination: C-c C-v C-t.
This will trigger configuration generation for Emacs.

To use the generated configuration, you can specify the path to the config.el file:
- for straight
```
(load "~/.config/emacs/init.el")
```

- for Guix
```
(load "~/.config/emacs-guix/init.el")
```
