## Who let you in here?

Relax! I'm here to make your life easier. Has your company ever
switched to using [Slack](https://slack.com), and then you wanted to
write silly Slack bots in Haskell as a way to learn Haskell?

<sup>Really?<sup>Wow<sup>That was a pretty specific question.</sup></sup>

Uh, do you want to be friends? Well let's talk about it later, because right now I have an example for you.

But you'll have to grab me first:

* `cabal sandbox init`
* `cabal install linklater`

If you don't have Haskell, it's quite easy: [Windows](http://www.haskell.org/platform/), [Mac](http://ghcformacosx.github.io/), and [Linux](https://gist.githubusercontent.com/hlian/b5a975252997cb3e0020/raw/e4ecab3042225d321a88ee74e804c38ead38ed52/gistfile1.txt).

## Show me an example!

* [jpgtobot](https://github.com/hlian/jpgtobot/blob/master/Main.hs) is a slackbot that pastes the image at `foo.jpg.to` into chat. It even supports jpgto's flags: so `-- r+gif` will give you a GIF, randomly selected from all known images named `foo`. Thanks to the magic of -screen scraping-.

  ![jpgtobot in action](https://raw.githubusercontent.com/hlian/linklater/38536bebf00c60fb1214b2c3a741ce00485e87af/corgi.jpg)

* [hi5bot](https://github.com/hlian/hi5bot/blob/master/Main.hs) lets you high-five people. There are other amazing things it can do too.

* [huskybot](https://github.com/hlian/huskybot) is for people who need pictures of huskies, now. [As featured on Wired.](http://www.wired.com/2015/08/slack-overrun-bots-friendly-wonderful-bots/all/1)

* [mathbot](https://github.com/hlian/mathbot) takes in LaTeX, spits out PNGs.

## Features

  * Uses `Text` for state-of-the-art Unicode support;
  * Lovely documentation with no misspelllllings to be found;
  * Supports [Slack's formatting syntax](https://api.slack.com/docs/formatting Slack's formatting syntax)
  * Comes with a fast mode (`slashSimple`) and a power mode (`slash`)
  * A warm, receptive maintainer with beautiful brown eyes;
  * Fully Haddock'd methods and module;
  * Open source (BSD3).

## Contributors

* [Hao Lian](https://hao.codes), author;
* [Ulysses Popple](http://upopple.com/);
* [Ian Henry](https://ianthehenry.com/), who showed me the `flip (+) foo` -> `(+ foo)` trick; and
* *Shields* (the Grizzly Bear album), which I listened all the way through for the first time while I was writing this ★★★★.

## See also

* [tightrope](https://github.com/ianthehenry/tightrope), a library Ian should really document
