<pre>

/\ \     /\ \ /\ "-.\ \ /\ \/ /  /\ \     /\  __ \ /\__  _\/\  ___\ /\  == \
\ \ \____\ \ \\ \ \-.  \\ \  _"-.\ \ \____\ \  __ \\/_/\ \/\ \  __\ \ \  __<
 \ \_____\\ \_\\ \_\\"\_\\ \_\ \_\\ \_____\\ \_\ \_\  \ \_\ \ \_____\\ \_\ \_\
  \/_____/ \/_/ \/_/ \/_/ \/_/\/_/ \/_____/ \/_/\/_/   \/_/  \/_____/ \/_/ /_/


A Haskell library for the Slack API.
It even comes with support for real-
time messaging (read: web sockets).

<b>Development (OS X)</b>

<a href="https://circleci.com/gh/hlian/linklater"><img src="https://circleci.com/gh/hlian/linklater.svg?style=svg" alt="CircleCI status"></a>
 
<a href="https://github.com/commercialhaskell/stack/releases">Install stack into ~/.local/bin/.</a>

• export PATH=~/.local/bin:$PATH
• stack setup
• stack build --fast --test

Right now Stack is a hard requirement
because I hate versioning my
dependencies. Pull requests welcome!


<b>I just want the library</b>

It is on Hackage but not on Stackage.
You will have to add "linklater-4.0.0.0"
to "extra-deps".


<b>I just want documentation</b>

• <a href="http://hlian.github.io/linklater/hackage/">GitHub Pages</a> has documentation for
  the master branch

• <a href="https://hackage.haskell.org/package/linklater">Hackage</a> has documentation for the
  last published version<


<b>Bots built with Linklater</b>

• <a href="https://github.com/hlian/linklater/blob/master/examples/app/JointPhotographicExpertsGroupTonga.hs">jpgtobot</a>
  Uses @dpatti’s jpg-to library to
  search for JPEGs on the internet.
  Usage: `/jpeg togepi` or `/jpeg two
  headed boy`.

• <a href="https://github.com/hlian/hi5bot/">hi5bot</a>
  Lets you high-five people, among other
  amazing interactions. Usage: `/hi5
  susan`, `/hi5`.

• <a href="https://github.com/hlian/huskybot">huskybot</a>
  As featured in <em><a href="http://www.wired.com/2015/08/slack-overrun-bots-friendly-wonderful-bots/all/1">Wired magazine</a></em>.

• <a href="https://github.com/hlian/mathbot">mathbot</a>
  Takes in LaTeX, spits out PNGs. Usage:
  `/math e^{i\pi} + 1 = 0`.


<b>Features</b>

• Lovely Haddock-friendly documentation
  with no misspelllllings to be found

• A little DSL for the <a href="https://api.slack.com/docs/formatting">Slack formatting
  syntax</a>

• Comes, out of the box!, with a fast
  mode (`slashSimple`) and a power mode
  (`slash`)

• A warm, receptive maintainer with
  beautiful brown eyes

• BSD3 license


<b>Contributors</b>

• <a href="https://hao.codes">Hao Lian</a> @hlian
• <a href="http://upopple.com/">Ulysses Popple</a> @ulyssesp
• <a href="https://ianthehenry.com/">Ian Henry</a> @ianthehenry


<b>Our fierce competition</b>

• <a href="https://github.com/ianthehenry/tightrope">tightrope</a>


<b>Code of conduct</b>

All contributors to this project
solemnly swear to follow the code of
conduct cross their hearts hope to die
spit a seed into an eagle’s eye.

<a href="http://contributor-covenant.org/version/1/2/0/">~code of conduct~</a>

</pre>
