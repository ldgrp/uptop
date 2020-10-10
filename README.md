uptop 🙌
==============

`uptop` is a terminal client for Up Bank ⚡ written in Haskell.

Powered by the [Up Bank API][up-api], [brick][brick] and [servant][servant].

![tui-screenshot](/img/main.png)

## Quick Start 🚀

Pre-built (x86_64) binaries for Ubuntu and macOS are available at

https://github.com/ldgrp/uptop/releases

Download the binary, untar the archive and run the `uptop` executable.

`uptop` will use the `UP_BANK_TOKEN` environment variable to
populate the initial auth screen.

```bash
$ tar -xvf uptop-<version>-<os>.tar
$ cd dist
$ ./uptop
```

If you enconter this error,
```
error while loading shared libraries: libtinfo.so.5: cannot open shared object file: No such file or directory
```
installing `libncurses5` on Ubuntu solves the problem.

```
$ sudo apt-get update
$ sudo apt-get install libncurses5
```

### Windows

`uptop` depends on `vty` which does **not** support Windows.
Follow the instructions above for WSL.

### Source
If you do not have `cabal`, I highly recommend [`ghcup`][ghcup] or 
reading [this manual][cabal].

```
git clone https://github.com/ldgrp/uptop
cd uptop
cabal install uptop
```

## Controls 🕹️

|               | Key         | Vim-style   |
|---------------|-------------|-------------|
| Up            | Up          | k           |
| Down          | Down        | j           |
| Page Up       | PgUp        | Ctrl-b      |
| Page Down     | PgDown      | Ctrl-f      |
| Go to first   | Home        | g           |
| Go to last    | End         | G           |
| Viewport Up   | Ctrl+W Up   | Ctrl+W k    |
| Viewport Down | Ctrl+W Down | Ctrl+W j    |
  

## Todo  📝
- [ ] Help screen
- [ ] Mouse support
- [ ] Webhooks
- [ ] Weekly spending chart
- [ ] Background thread for transactions
- [ ] Search

[up-api]: https://developer.up.com.au/
[brick]: https://github.com/jtdaugherty/brick/
[servant]: https://servant.dev/
[ghcup]: https://www.haskell.org/ghcup/
[cabal]: https://www.haskell.org/cabal/
