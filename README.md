Make symlink for `myemacs`

```
ln -s $HOME/.emacs.d/myemacs $thisFolder
```

Edit `~/.emacs` to have

```
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/myemacs"))
(load "myemacs")
```

