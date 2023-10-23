# Install elisp files

## Debian / Ubuntu

Clone this repo to home path.

Build config symlink to emacs directory:

```bash
sudo ln -s ~/ld-emacs/site-lisp /usr/share/emacs/ld
```

Copy site-start.el in emacs directory to start my config:

```bash
sudo cp ~/ld-emacs/site-start.el /usr/share/emacs/site-lisp/
```

Initialize submodules:

```plaintext
cd ~/ld-emacs && git submodule update --init --recursive
```

## Windows

Clone this repo to `C:\Users\<username>\AppData\Roaming\ld-emacs` and initialize submodules.

Put the content below to `~/.emacs.d/init.el` (to `C:\Users\<username>\AppData\Roaming\.emacs.d\init.el`).

```lisp
(load-file "~/ld-emacs/site-start.el")
```

# Install dependencies for extensions

## Debian / Ubuntu

```plaintext
# for citre (ctags frontend)
sudo apt install universal-ctags

# for counsel-rg
sudo apt install ripgrep
```

Note: Emacs will load ~/.profile for env variables at the start. According to the code in ~/.profile, emacs will not load ~/.bashrc. So if user wish to add some env variables, the code should be written to ~/.profile.

## Windows

Download Universal-ctags binary and add it to environment variable `Path`.

Download the ripgrep for windows from [ripgrep](https://github.com/BurntSushi/ripgrep) and add the .exe file to env variable `Path`.
