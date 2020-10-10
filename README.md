uptop 🙌
==============

`uptop` is a terminal client for Up Bank ⚡ written in Haskell.

Powered by the [Up Bank API][up-api], [brick][brick] and [servant][servant].

![tui-screenshot](/img/main.png)

# Installation

### Build from Source
If you do not have `cabal`, I highly recommend [`ghcup`][ghcup] or 
reading [this manual][cabal].

```
git clone https://github.com/ldgrp/uptop
cd uptop
cabal run uptop
```

# Usage

Start `uptop` with the `UP_BANK_TOKEN` environment variable, or choose to enter it later.

![auth-tui-screenshot](/img/auth.png)

[up-api]: https://developer.up.com.au/
[brick]: https://github.com/jtdaugherty/brick/
[servant]: https://servant.dev/
[ghcup]: https://www.haskell.org/ghcup/
[cabal]: https://www.haskell.org/cabal/