Config Files
============

Config files and scripts to set things up.

## Scripts

### Ubuntu and related distros

The `muhbuntu.sh` shell script is for configuring Ubuntu and related
projects (Lubuntu, Xubuntu, etc.).

Download and install [oh my zsh](https://github.com/ohmyzsh/ohmyzsh).

### Windows

For now I am just manually
[linking](https://www.howtogeek.com/howto/16226/complete-guide-to-symbolic-links-symlinks-on-windows-or-linux/)
the relevant files. Automating later will probably require running a
PowerShell with some admin privileges. More info on making links
[here](https://winaero.com/blog/create-symbolic-link-windows-10-powershell/).

From an elevated powershell do,

```powershell
New-Item -ItemType SymbolicLink -Path "Link" -Target "Target"
```

where the "Target" items are config files here and the targets are the
places apps (like emacs) will read from and the "Path" is where you
want to place a link. Mine was something like
`c:/Users/foobarbaz/AppData/Roaming/.emacs.d/init.el`. (**Hint**: Use
[`xah-copy-file-path`](http://ergoemacs.org/emacs/emacs_copy_file_path.html)
to make getting this path easy in Windows.)

## Remapping Caps Lock to Escape

See ["Linux: Swap CapsLock Escape
Keys"](http://xahlee.info/linux/linux_swap_capslock_esc_key.html) for more
help.
