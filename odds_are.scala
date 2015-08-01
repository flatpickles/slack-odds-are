import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Success, Failure}

import akka.actor.{ActorSystem, Cancellable}
import slack.api.SlackApiClient
import slack.models.{User, Im}
import slack.rtm.SlackRtmClient

sealed abstract class GameState
case class ChillinState() extends GameState
case class WaitingForBoundState(challengingUser: String, challengedUser: String) extends GameState
case class WaitingForOddsState(challengingUser: String, challengedUser: String) extends GameState

object OddsAre {
  // Set up some global Slack API nonsense
  implicit val system = ActorSystem("slack")
  // TODO: put tokein in an external config file, and don't push that
  // TODO: regenerate the token before deploying, because this git history will probably be public
  val token = "xoxb-8514952037-ZjR429APAyVfNiCOfslE9lYA"
  val rtmClient = SlackRtmClient(token)
  val apiClient = SlackApiClient(token)

  // Maintain a mutable game state
  var gameState: GameState = ChillinState()
  var nextReset: Option[Cancellable] = None

  def main(args: Array[String]) = {
    refreshAndRun()
  }

  def refreshAndRun(): Unit = {
    val resolvedSlackInfo: Future[(Seq[User], Seq[Im])] = for {
      users <- apiClient.listUsers()
      ims <- apiClient.listIms()
    } yield (users, ims)

    resolvedSlackInfo.onComplete {
      case Success((users, ims)) => runReceiveLoop(users, ims, rtmClient)
      case _ => println("something went terribly wrong")
    }
  }

  def updateReset(onUpdate: () => Unit) = {
    nextReset.map(_.cancel())
    nextReset = Some(system.scheduler.scheduleOnce(5 seconds) {
      resetState(onUpdate)
    })
  }

  def resetState(onUpdate: () => Unit) = {
    println("[LOG] Resetting state")
    gameState = ChillinState()
    onUpdate()
  }

  def runReceiveLoop(users: Seq[User], imChannels: Seq[Im], rtmClient: SlackRtmClient): Unit = {
    val usernameRegex = users.map(_.name).mkString("|").r
    val uidToUser = users.map { user =>
      (user.id, user.name)
    }.toMap
    val channelToUser = imChannels.map {im =>
      (im.id, im.user)
    }.toMap

    rtmClient.onMessage { message =>
      val sendMessage = (msg: String) => {rtmClient.sendMessage(message.channel, msg)}

      if (message.text.toLowerCase().contains("odds_are restart")) {
        // Restart bot
        rtmClient.sendMessage(message.channel, "Restarting...")
        rtmClient.close()
        refreshAndRun()
        return
      }

      gameState match {
        case state: ChillinState => {
          if (message.text.toLowerCase().contains("odds are")) {
            // We're waiting for "odds are" input, and it's lit
            usernameRegex.findFirstIn(message.text) match {
              case Some(username) =>
                uidToUser.get(message.user) match {
                  case Some(author) => {
                    gameState = WaitingForBoundState(author, username)
                    updateReset(() => {
                      sendMessage(s"@${username}: ya done waited too long.")
                    })
                    sendMessage(s"@${username}: reply to ${author}'s challenge with your upper bound.")
                  }
                  case _ => {
                    println(s"[LOG] Couldn't resolve message sender for ${message.text}")
                    sendMessage(s"Whoops something is broken, try sending 'odds_are restart'")
                  }
                }
              case None =>
                sendMessage("Please direct your Odds Are challenges towards another user by mentioning them.")
            }
          }
        }
        case state: WaitingForBoundState => {
          // TODO: collect bounds entry from challenged user, and reset the timeout as appropriate
        }
        case state: WaitingForOddsState => {
          // TODO: collect and record odds entries from both engaged users, and reset the timeout as appropriate

          channelToUser.get(message.channel).map { user =>
            // TODO: user here is ID, and we have name in state class. Do we need both?
            println(s"im from ${user}")
          }
        }
        case _ => println("[LOG] I'm as confused as you are.")
      }
    }
  }
}

// TODO: refactor and clean everything up, damn
