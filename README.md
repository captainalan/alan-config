Config Files
============

Config files and scripts to set things up.

Scripts
--------

### Linux

Download and install `zsh` (with optional eye candy using [oh my
zsh](https://github.com/ohmyzsh/ohmyzsh)) early on so you can edit
`.zshrc` and related config files without having to create more work
by having to move a bash config.

The `muharch.sh` shell script is for configuring Arch.
The `muhbuntu.sh` shell script is for configuring Ubuntu and related
projects (Lubuntu, Xubuntu, etc.).


### Windows

For now I am just manually
[linking](https://www.howtogeek.com/howto/16226/complete-guide-to-symbolic-links-symlinks-on-windows-or-linux/)
the relevant files. Automating later will probably require running a
PowerShell with some admin privileges. More info on making links
[here](https://winaero.com/blog/create-symbolic-link-windows-10-powershell/).

Summon an **Elevated PowerShell** by right clicking on the Start menu
and selecting `Windows PowerShell (Admin)`.

```powershell
New-Item -ItemType SymbolicLink -Path "Link" -Target "Target"
```

where the "Target" items are config files here and the "Path" items are the
places apps (like emacs) will read from. I ran this a few times to also link
the sub-directories of =.emacs.d= here that have some additional custom code. 

```powershell
New-Item -ItemType SymbolicLink -Target "C:\Users\alan\github-stuff\alan-config\.emacs.d\init.el" -Path "C:\Users\alan\AppData\Roaming\.emacs.d\init.el"
New-Item -ItemType SymbolicLink -Target "C:\Users\alan\github-stuff\alan-config\.emacs.d\lisp\" -Path "C:\Users\alan\AppData\Roaming\.emacs.d\lisp"
New-Item -ItemType SymbolicLink -Target "C:\Users\alan\github-stuff\alan-config\.emacs.d\snippets\" -Path "C:\Users\alan\AppData\Roaming\.emacs.d\snippets"
```

You'll of course want to change the names and directories above to
your own machine's settings (the `...\AppData\Roaming...` stuff
structure should be same though).


## Remapping Caps Lock to Escape

See ["Linux: Swap CapsLock Escape
Keys"](http://xahlee.info/linux/linux_swap_capslock_esc_key.html) for
more help with Linux

For Windows, you'll want to get
[SharpKeys](https://chocolatey.org/packages/sharpkeys). Note that as
of July 2020, if you do a fresh install of Nodejs, you'll be prompted
with the option to install Chocolatey (package manager for Window), so
SharpKeys should be readily available if you've already set up this
tooling.
