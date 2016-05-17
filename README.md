Hacker News Twitter Bot
=======================

A rewrite of https://github.com/sengaya/hn-bot-top-stories in Haskell.

The bot posts the current top story of [Hacker News](https://news.ycombinator.com/) to Twitter. To see this bot in action follow @hn_bot_top1: https://twitter.com/hn_bot_top1

Changes in comparison to the Python version:
- Supports only the current top story, not top 3 or top 5 etc.
- Minor update how to calculate length of URLs
- Configuration via environment variables

# Configuration
The following 4 environment variables are read for Twitter authentication.
```
OAUTH_CONSUMER_KEY
OAUTH_CONSUMER_SECRET
OAUTH_ACCESS_TOKEN
OAUTH_ACCESS_SECRET
```

# Seen file
The program stores already posted stories in a simple file: seen.db

# Build dependencies
Debian/Ubuntu (tested on 14.04):
```
apt-get install libcrypto++-dev libssl-dev zlib1g-dev
```

# Build and run tests
At the moment (may 2016) the [hackernews library](https://github.com/dmjio/hackernews) has a bug, so you need to build it locally with [this patch](https://github.com/dmjio/hackernews/pull/18).
```
cabal sandbox init
cabal sandbox add-source /path/to/hackernews/repo
cabal install --only-dependencies --enable-tests
cabal build
cabal test
```

# Run dependencies
Debian/Ubuntu (tested on 14.04):
```
apt-get install ca-certificates libgmp10 libssl1.0.0 zlib1g
```

# License
See [LICENSE](LICENSE)
