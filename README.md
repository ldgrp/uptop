uptop 🙌
==============

`uptop` is a terminal client for Up Bank ⚡ written in Haskell.

Powered by the [Up Bank API][up-api], [brick][brick] and [servant][servant].

![tui-screenshot](/img/main.png)

## Quick Start 🚀

Pre-built (x86_64) binaries for Ubuntu and macOS are available at

https://github.com/ldgrp/uptop/releases

Download the binary, untar the archive and run the `uptop` executable.

```bash
$ tar -xvf uptop-<version>-<os>.tar
$ cd dist
$ ./uptop
```

![auth-tui-screenshot](/img/auth.png)

`uptop` checks the `UP_BANK_TOKEN` environment variable and uses it to populate
the initial auth screen.

### Build from source
If you do not have `cabal`, I highly recommend [`ghcup`][ghcup] or 
reading [this manual][cabal].

```
git clone https://github.com/ldgrp/uptop
cd uptop
cabal install uptop
```

## Controls 🕹️

|               | Key       | Vim-style |
|---------------|-----------|-----------|
| Up            | Up        | k         |
| Down          | Down      | j         |
| Page Up       | PgUp      | Ctrl-b    |
| Page Down     | PgDown    | Ctrl-f    |
| Go to first   | Home      | g         |
| Go to last    | End       | G         |
| Viewport Up   | Ctrl+Up   | Ctrl+k    |
| Viewport Down | Ctrl+Down | Ctrl+j    |
  

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
