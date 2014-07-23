## Who let you in here?

Relax! I'm here to make your life easier. Has your company ever switched to using Slack, and then you wanted to write silly Slack bots in Haskell as a way to learn Haskell?

Really?

Wow.

(That was a pretty specific question.)

Uh, do you want to be friends? Well let's talk about it later, because right now I have something that's going to blow your mind!

## Show me an example!

Here's a `/jpgto` bot! If you run this program and then tell Slack
about your server (incoming hook and custom slash command) and then
type `/jpgto diplomatico` in one of your channels, you'll get the
image from [http://diplomatico.jpg.to](http://diplomatico.jpg.to). How, you say? _Screen scraping_.

```haskell

urlParser :: Parser B.ByteString
urlParser = p
  where
    p = garbage *> url
    garbage = string "src=\"" <|> (P.take 1 *> garbage)
    url = takeTill (== _quotedbl)

urlFor :: Text -> IO Text
urlFor search = do
  r <- get (T.unpack $ F.format "http://{}.jpg.to/" [search])
  (return . handle . parse urlParser . strictly) (r ^. responseBody)
  where
    strictly = B.concat . L.toChunks
    handle (Fail i ctxs s) = error (show (i, ctxs, s))
    handle (Partial f) = handle (f "")
    handle (Done _ r) = toText r

jpgto :: Maybe Command -> Application
jpgto (Just (Command (User user) channel text)) req respond = do
  url <- urlFor (maybe "spolsky" id text)
  say (Message channel (response url) (EmojiIcon "gift")) config
  (respond . responseLBS status200 headers) ""
  where
    response url = F.format "@{} {}" (user, url)
    config = Config token "trello.slack.com"
    headers = [("Content-Type", "text/plain")]

main :: IO ()
main = do
  let port = 80
  putStrLn (F.format "+ Listening on port {}" [port])
  run port (slash jpgto)
  return ()
```

For the full example (since this one is missing a ton of imports), see
the `examples/` directory on GitHub.

## Features

* Uses `Text` everywhere so you can send your slash commands crazy Unicode characters all day long.
* Lovely documentation.
* Battle-tested.


## Contributors

* [Hao Lian](https://hao.codes), author
* [Ian Henry](https://ianthehenry.com), design review and _future contributor_???
* *Shields* (the Grizzly Bear album), which I listened all the way through for the first time while I was writing this ★★★★
