package biz.man1.odds_are

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Success, Failure}

import akka.actor.{ActorSystem, Cancellable, ActorRef}
import slack.api.SlackApiClient
import slack.models.{User, Im, Message}
import slack.rtm.SlackRtmClient

object OddsAre {
  implicit val system = ActorSystem("slack")
  val challengeTimeout = 10 seconds // TODO: change this
  var nextReset: Option[Cancellable] = None

  def main(args: Array[String]) = {
    refreshAndRun()
  }

  def refreshAndRun(): Unit = {
    // TODO: put token in an external config file, and don't push that
    // TODO: regenerate the token before deploying, because this git history will probably be public
    val token = "xoxb-8514952037-ZjR429APAyVfNiCOfslE9lYA"
    val apiClient = SlackApiClient(token)
    val resolvedSlackInfo: Future[(Seq[User], Seq[Im])] = for {
      users <- apiClient.listUsers()
      ims <- apiClient.listIms()
    } yield (users, ims)

    resolvedSlackInfo.onComplete {
      case Success((users, ims)) => {
        val usersMap = users.map { user =>
          (user.id, user)
        }.toMap
        val channelIdsMap = (for {
          imChannel <- ims
          user <- usersMap.get(imChannel.user)
        } yield (imChannel.id, user)).toMap
        listenForOddsAre(SlackRtmClient(token), usersMap, channelIdsMap)
      }
      case _ => println("[LOG] startup failed")
    }
  }

  def updateReset(onUpdate: () => Unit) = {
    nextReset.map(_.cancel())
    nextReset = Some(system.scheduler.scheduleOnce(challengeTimeout) {
      onUpdate()
    })
  }

  def checkForCommands(rtmClient: SlackRtmClient, message: Message, status: String, challengingUserOpt: Option[User], challengedUserOpt: Option[User]): Boolean = {
      val sendMessage = (msg: String) => rtmClient.sendMessage(message.channel, msg)
      val messageContains = (substring: String) => message.text.toLowerCase().contains(substring) : Boolean

      // RESTART BOT
      if (messageContains("odds_are") && messageContains("restart")) {
        sendMessage("Restarting...")
        rtmClient.close()
        refreshAndRun()
        return true
      } else

      // LOG STATE TO CHANNEL
      if (messageContains("odds_are") && messageContains("status")) {
        (challengingUserOpt, challengedUserOpt) match {
          case (Some(challengingUser), Some(challengedUser)) =>
            sendMessage(s"Current status: ${status}.\nChallenge: ${challengingUser.name} -> ${challengedUser.name}")
          case _ =>
            sendMessage(s"Current status: ${status}")
        }

        return true
      }

      return false
  }

  def listenForOddsAre(rtmClient: SlackRtmClient, uidsToUsers: Map[String, User], channelIdToUsers: Map[String, User]): ActorRef = {
    val usernamesToUsers = uidsToUsers.map { case (id, user) =>
      (user.name, user)
    }.toMap
    val usernameRegex = uidsToUsers.values.map(_.name).mkString("|").r

    lazy val listener: ActorRef = rtmClient.onMessage { message =>
      val sendMessage = (msg: String) => rtmClient.sendMessage(message.channel, msg)
      val messageContains = (substring: String) => message.text.toLowerCase().contains(substring) : Boolean

      if (!checkForCommands(rtmClient, message, "listening for challenges", None, None) && messageContains("odds are")) {
        uidsToUsers.get(message.user) match {
          case Some(author) => {
            usernamesToUsers.get(usernameRegex.findFirstIn(message.text).getOrElse("")) match {
              case Some(challengedUser) => {
                sendMessage(s"@${challengedUser.name}: reply to ${author.name}'s challenge with your upper bound.")
                rtmClient.removeEventListener(listener)
                val upperBoundListener = listenForUpperBound(rtmClient, uidsToUsers, channelIdToUsers, author, challengedUser)
                updateReset(() => {
                  sendMessage(s"@${challengedUser.name}: ya done waited too long.")
                  rtmClient.removeEventListener(upperBoundListener)
                  listenForOddsAre(rtmClient, uidsToUsers, channelIdToUsers)
                })
              }
              case None =>
                sendMessage(s"@${author.name}: please direct your challenges towards another user by mentioning them.")
            }
          }
          case _ => {
            println(s"[LOG] Couldn't resolve message sender for ${message.text}")
            sendMessage(s"Whoops something is broken, try sending 'odds_are restart'")
          }
        }
      }
    }
    listener
  }

  def listenForUpperBound(rtmClient: SlackRtmClient, uidsToUsers: Map[String, User], channelIdToUsers: Map[String, User], challengingUser: User, challengedUser: User): ActorRef = {
    val numberRegex = "[0-9]+".r
    lazy val listener: ActorRef = rtmClient.onMessage { message =>
      val sendMessage = (msg: String) => rtmClient.sendMessage(message.channel, msg)
      val messageContains = (substring: String) => message.text.toLowerCase().contains(substring) : Boolean

      if (!checkForCommands(rtmClient, message, "waiting for upper bound", Some(challengingUser), Some(challengedUser)) && message.user == challengedUser.id) {
        for {
          bound <- numberRegex.findFirstIn(message.text)
        } yield {
          sendMessage(s"The odds are set! @${challengingUser.name} and @${challengedUser.name}, IM me a number 1-${bound}")
          rtmClient.removeEventListener(listener)
          val imOddsListener = listenForImOdds(rtmClient, uidsToUsers, channelIdToUsers, challengingUser, challengedUser)
          updateReset(() => {
            sendMessage(s"@${challengingUser.name} and @${challengedUser.name}: ya done waited too long.")
            rtmClient.removeEventListener(imOddsListener)
            listenForOddsAre(rtmClient, uidsToUsers, channelIdToUsers)
          })
        }
      }
    }
    listener
  }

  def listenForImOdds(rtmClient: SlackRtmClient, uidsToUsers: Map[String, User], channelIdToUsers: Map[String, User], challengingUser: User, challengedUser: User): ActorRef = {
    lazy val listener: ActorRef = rtmClient.onMessage { message =>
      if (!checkForCommands(rtmClient, message, "Waiting for instant messages", Some(challengingUser), Some(challengedUser))) {
        // TODO: collect and record odds entries from both engaged users, and reset the timeout as appropriate

        channelIdToUsers.get(message.channel).map { user =>
          // TODO: user here is ID, and we have name in state class. Do we need both?
          println(s"[BOOM] im from ${user}")
        }
      }
    }
    listener
  }
}
