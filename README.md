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

Here's a `/jpgto` bot. If you run this program and then tell Slack
about your server (incoming hook and custom slash command) and then
type `/jpgto diplomatico` in one of your channels, you'll get the
image from [http://diplomatico.jpg.to](http://diplomatico.jpg.to). How, you say? _Screen scraping_.

```haskell
-- Remaining imports left as an exercise to the reader.
import Network.Linklater (say, slashSimple, Command(..), Config(..), Message(..), Icon(..), Format(..))

findUrl :: Text -> Maybe Text
findUrl = fmap fromStrict . maybeResult . parse (manyTill (notChar '\n') (string "src=\"") *> takeTill (== '"'))

jpgto :: Maybe Command -> IO Text
jpgto (Just (Command user channel (Just text))) = do
  message <- (fmap messageOf . findUrl . decodeUtf8 . flip (^.) responseBody) <$> get ("http://" <> (unpack subdomain) <> ".jpg.to/")
  case (debug, message) of
    (True, _) -> putStrLn ("+ Pretending to post " <> (unpack . decodeUtf8 . encode) message) >> return ""
    (False, Just m) -> config' >>= say m >> return ""
    (False, Nothing) -> return "Something went wrong!"
  where config' = (Config "trello.slack.com" . filter (/= '\n') . pack) <$> readFile "token"
        subdomain = (intercalate "." . fmap (filter isLetter . filter isAscii) . words) text
        messageOf url = FormattedMessage (EmojiIcon "gift") "jpgtobot" channel [FormatAt user, FormatLink url (subdomain <> ".jpg.to>")]
        debug = True
jpgto _ = return "Type more! (Did you know? jpgtobot is only 26 lines of Haskell. <https://github.com/hlian/jpgtobot/blob/master/Main.hs>)"

main :: IO ()
main = let port = 3000 in putStrLn ("+ Listening on port " <> show port) >> run port (slashSimple jpgto)
```

For the full example (since this one is missing a ton of imports), see
the `examples/` directory on GitHub.

Now! `/jpgto corgi`:

![jpgtobot in action](corgi.jpg)

So easy. Much fast.

## Features

  * Uses 'Text' for state-of-the-art Unicode support;
  * Lovely documentation with no misspelllllings to be found;
  * Supports [Slack's formatting syntax](https://api.slack.com/docs/formatting Slack's formatting syntax)
  * Comes with a fast mode (`slashSimple`) and a power mode (`slash`)
  * A warm, receptive maintainer with beautiful brown eyes;
  * Fully Haddock'd methods and module;
  * Open source (BSD3).

## Contributors

* [Hao Lian](https://hao.codes), author
* [Ian Henry](https://ianthehenry.com), design review and _future contributor_???
* *Shields* (the Grizzly Bear album), which I listened all the way through for the first time while I was writing this ★★★★
