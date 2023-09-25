# Freekey.el

This is a _very_ early take on a new keybinding system for Emacs.  Currently this is focused on very specifically unmapping/remapping and managing `C-c` and `C-x`, such that you can regain control of these (and eventually any other) keys.  Right now this basically happens, but a lot of things are hardcoded.

The **eventual** goal is something like:

  * Capture/remap any prefixes by any mode/etc
  * Define symbolic things like `delete-key`, and catch bindings to `C-d` and remap them to what _you_ want delete to be
  * Provide sensible / easy remappings for various layouts (dvorak etc), weird keyboards, etc
