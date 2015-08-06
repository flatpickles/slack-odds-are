Odds Are Slack Bot
==================

This is a bot designed to referee games of [Odds Are](http://www.urbandictionary.com/define.php?term=Odds+are) within a Slack channel.

If you'd like to use this bot for your Slack team, first clone this repo on the machine you'd like to use to run the bot. [Create a new bot](https://my.slack.com/services/new/bot) for your Slack team, and give it whatever name and associated swag you like. Once it's created, copy your new bot's API Token and save it as "token.txt" in your odds_are directory. Then use `sbt run` within this directory to start up the bot. Enjoy!

Huge shoutout to [@gilbertw1](https://github.com/gilbertw1) for his awesome [Slack Scala Client](https://github.com/gilbertw1/slack-scala-client), upon which this bot is built.