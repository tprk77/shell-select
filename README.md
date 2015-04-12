# shell-select #

This extension provides a quick and easy way to use multiple shells.

Launching shells in Emacs is harder then it should be: you have to run the shell command, if you
want to name the shell you have to provide a prefix argument, and then when you name the shell you
probably want to type asterisks and what-not so it looks good.

This extension helps alleviate those annoyances by doing some of the work for you. When launching a
shell, you will always be prompted for a name. If the name looks kind of plain, it will be fixed up
to look like a shell name. (So something like `debug` will get turned into `*debug-shell*`. Cool
right?) Then when you want to switch shells, you will get choice of just the shell buffers, with
completion. I recommend using Ido completion to make it even better.

There is only one function in this extension: `shell-select-switch-shell`. You should probably bind
it to a really awesome key, like `<f1>` maybe.
