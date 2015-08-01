package biz.man1.odds_are

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Success, Failure}

import akka.actor.{ActorSystem, Cancellable}
import slack.api.SlackApiClient
import slack.models.{User, Im}
import slack.rtm.SlackRtmClient

object GameState extends Enumeration {
  val Chillin, WaitingForBound, WaitingForOdds = Value
}
import GameState._

case class RichGameState(
  state: GameState.Value,
  challengingUser: Option[String] = None,
  challengingUserId: Option[String] = None,
  challengedUser: Option[String] = None,
  challengedUserId: Option[String] = None
)

object OddsAre {
  val challengeTimeout = 10 seconds // TODO: change this

  // Set up some global Slack API nonsense
  implicit val system = ActorSystem("slack")
  // TODO: put tokein in an external config file, and don't push that
  // TODO: regenerate the token before deploying, because this git history will probably be public
  val token = "xoxb-8514952037-ZjR429APAyVfNiCOfslE9lYA"
  val apiClient = SlackApiClient(token)

  // Maintain a mutable game state
  var gameState: RichGameState = RichGameState(GameState.Chillin)
  var nextReset: Option[Cancellable] = None

  def main(args: Array[String]) = {
    refreshAndRun()
  }

  def refreshAndRun(): Unit = {
    resetState(()=>{})
    val resolvedSlackInfo: Future[(Seq[User], Seq[Im])] = for {
      users <- apiClient.listUsers()
      ims <- apiClient.listIms()
    } yield (users, ims)

    resolvedSlackInfo.onComplete {
      case Success((users, ims)) => runReceiveLoop(users, ims, SlackRtmClient(token))
      case _ => println("something went terribly wrong")
    }
  }

  def updateReset(onUpdate: () => Unit) = {
    nextReset.map(_.cancel())
    nextReset = Some(system.scheduler.scheduleOnce(challengeTimeout) {
      resetState(onUpdate)
    })
  }

  def resetState(onUpdate: () => Unit) = {
    println("[LOG] Resetting state")
    gameState = RichGameState(GameState.Chillin)
    onUpdate()
  }

  def runReceiveLoop(users: Seq[User], imChannels: Seq[Im], rtmClient: SlackRtmClient): Unit = {
    val usernameRegex = users.map(_.name).mkString("|").r
    val numberRegex = "[0-9]+".r

    val uidToUser = users.map { user =>
      (user.id, user.name)
    }.toMap
    val userToUid = uidToUser.map(_.swap)
    val channelToUser = imChannels.map { im =>
      (im.id, im.user)
    }.toMap

    rtmClient.onMessage { message =>
      val sendMessage = (msg: String) => rtmClient.sendMessage(message.channel, msg)
      val messageContains = (substring: String) => message.text.toLowerCase().contains(substring) : Boolean

      // RESTART BOT
      if (messageContains("odds_are restart")) {
        sendMessage("Restarting...")
        rtmClient.close()
        refreshAndRun()
        return
      } else

      // LOG STATE TO CHANNEL
      if (messageContains("odds_are state")) {
        // Log state to channel
        sendMessage(s"Current state: ${gameState.state}. Challenge: ${gameState.challengingUser} -> ${gameState.challengedUser}")
        return
      }

      uidToUser.get(message.user) match {
        case Some(author) => {
          gameState.state match {
            case GameState.Chillin => {
              if (messageContains("odds are")) {
                // We're waiting for "odds are" input, and it's lit
                usernameRegex.findFirstIn(message.text) match {
                  case Some(username) => {
                    gameState = RichGameState(GameState.WaitingForBound, Some(author), Some(message.user), Some(username), userToUid.get(username))
                    updateReset(() => {
                      sendMessage(s"@${username}: ya done waited too long.")
                    })
                    sendMessage(s"@${username}: reply to ${author}'s challenge with your upper bound.")
                  }
                  case None =>
                    sendMessage(s"@${author}: please direct your Odds Are challenges towards another user by mentioning them.")
                }
              }
            }
            case GameState.WaitingForBound => {
              for {
                bound <- numberRegex.findFirstIn(message.text)
                challenger <- gameState.challengingUser
                challenged <- gameState.challengedUser
                if author == challenged
              } yield {
                sendMessage(s"The odds are set! @${challenger} and @${challenged}, IM me a number 1-${bound}")
                gameState = RichGameState(GameState.WaitingForOdds, gameState.challengingUser, gameState.challengingUserId, gameState.challengedUser, gameState.challengedUserId)
                updateReset(() => {
                  sendMessage(s"@${challenger} and @${challenged}: ya done waited too long.")
                })
              }
            }
            case GameState.WaitingForOdds => {
              // TODO: collect and record odds entries from both engaged users, and reset the timeout as appropriate

              channelToUser.get(message.channel).map { user =>
                // TODO: user here is ID, and we have name in state class. Do we need both?
                println(s"im from ${user}")
              }
            }
          }
        }
        case _ => {
          println(s"[LOG] Couldn't resolve message sender for ${message.text}")
          sendMessage(s"Whoops something is broken, try sending 'odds_are restart'")
        }
      }
    }
  }
}

// TODO: refactor and clean everything up, damn
