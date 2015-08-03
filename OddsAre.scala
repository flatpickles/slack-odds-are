package biz.man1.odds_are

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.io.Source
import scala.util.{Success, Failure}

import akka.actor.{ActorSystem, Cancellable, ActorRef}
import slack.api.SlackApiClient
import slack.models.{User, Im, Message}
import slack.rtm.SlackRtmClient

object OddsAre {
  implicit val system = ActorSystem("slack")
  val challengeTimeout = 2 minutes
  var nextReset: Option[Cancellable] = None

  def main(args: Array[String]) = {
    refreshAndRun()
  }

  def refreshAndRun(): Unit = {
    val token = Source.fromFile("token.txt").mkString.replace("\n", "")
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
      case Failure(err) => {
        println("[LOG] startup failed")
        println(err)
      }
    }
  }

  /**
   * We want the bot to time out if expected input is not received after a period of time. This method will clear any existing timeout (if exists),
   * and schedule another timeout with the anonymous function passed to it. This function will be executed after a period of time, if updateReset is
   * not called again.
   */
  def updateReset(onUpdate: () => Unit) = {
    nextReset.map(_.cancel())
    nextReset = Some(system.scheduler.scheduleOnce(challengeTimeout) {
      onUpdate()
    })
  }

  /**
   * Given an rtmClient and a message received with this client, check for command strings, and respond as necessary.
   * Options:
   * - restart: close the rtmClient and start the bot anew
   * - status: print the current status of the bot, as passed to this method
   * - help: print out info about the bot
   */
  def checkForCommands(rtmClient: SlackRtmClient, message: Message, status: String, challengingUserOpt: Option[User], challengedUserOpt: Option[User]): Boolean = {
      val sendMessage = (msg: String) => rtmClient.sendMessage(message.channel, msg)
      val messageContains = (substring: String) => message.text.toLowerCase().contains(substring) : Boolean

      if (!messageContains("odds_are")) {
        return false
      }

      // Restart bot
      if (messageContains("restart")) {
        sendMessage("Restarting...")
        rtmClient.close()
        refreshAndRun()
        return true
      } else

      // Log state to channel
      if (messageContains("status")) {
        (challengingUserOpt, challengedUserOpt) match {
          case (Some(challengingUser), Some(challengedUser)) =>
            sendMessage(s"Current status: ${status}.\nChallenge: ${challengingUser.name} -> ${challengedUser.name}")
          case _ =>
            sendMessage(s"Current status: ${status}")
        }
        return true
      } else

      // Print helpful info
      if (messageContains("help")) {
        sendMessage(s"*odds_are is an automated referee for games of Odds Are.*\nAny message containing a teammate's username and the phrase 'odds are' will start a game. The bot will then prompt participants for input as needed. If a necessary response is not received within ${challengeTimeout}, the current game will be forfeited.\n\nType these commands to manage the bot:\n*odds_are help:* displays this menu.\n*odds_are restart:* restarts the bot, resetting the game in progress (if one exists). Will also reload the list of available users.\n*odds_are status:* outputs the current status of the bot.\n\nRead more about this project and the beautiful game of Odds Are: https://github.com/man1/slack-odds-are")
      }

      return false
  }

  /**
   * Start a listenern using rtmClient which will listen for any "odds are" challenges. This should be running whenever a challenge has not been initiated.
   */
  def listenForOddsAre(rtmClient: SlackRtmClient, uidsToUsers: Map[String, User], channelIdToUsers: Map[String, User]): ActorRef = {
    val usernamesToUsers = uidsToUsers.map { case (id, user) =>
      (user.name, user)
    }.toMap
    val usernameRegex = uidsToUsers.values.map(_.name).mkString("|").r
    val userIdRegex = uidsToUsers.keys.mkString("|").r

    lazy val listener: ActorRef = rtmClient.onMessage { message =>
      val sendMessage = (msg: String) => rtmClient.sendMessage(message.channel, msg)
      val messageContains = (substring: String) => message.text.toLowerCase().contains(substring) : Boolean

      if (message.user != rtmClient.state.self.id && !checkForCommands(rtmClient, message, "listening for challenges", None, None) && messageContains("odds are")) {
        uidsToUsers.get(message.user) match {
          case Some(author) => {
            val challengeUser = (foundIdentifier: String, toUserMap: Map[String, User]) => {
              toUserMap.get(foundIdentifier) match {
                case Some(challengedUser) => {
                  sendMessage(s"@${challengedUser.name}: reply to ${author.name}'s challenge with your upper bound.")
                  rtmClient.removeEventListener(listener)
                  val upperBoundListener = listenForUpperBound(rtmClient, uidsToUsers, channelIdToUsers, author, challengedUser)
                  updateReset(() => {
                    sendMessage(s"@${challengedUser.name}: ya done waited too long.")
                    rtmClient.removeEventListener(upperBoundListener)
                    listenForOddsAre(rtmClient, uidsToUsers, channelIdToUsers)
                  })
                  true
                }
                case _ => false
              }
            } : Boolean

            // Look for users mentioned either by ID or by name (Slack replaces @mentions with user ID)
            val foundName = usernameRegex.findFirstIn(message.text).map(challengeUser(_, usernamesToUsers)).getOrElse(false) || userIdRegex.findFirstIn(message.text).map(challengeUser(_, uidsToUsers)).getOrElse(false)
            if (!foundName) {
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

  /**
   * Start a listener using rtmClient which will listen for an upper bound from the challenged user.
   */
  def listenForUpperBound(rtmClient: SlackRtmClient, uidsToUsers: Map[String, User], channelIdToUsers: Map[String, User], challengingUser: User, challengedUser: User): ActorRef = {
    lazy val listener: ActorRef = rtmClient.onMessage { message =>
      val sendMessage = (msg: String) => rtmClient.sendMessage(message.channel, msg)

      if (message.user != rtmClient.state.self.id && !checkForCommands(rtmClient, message, s"waiting for upper bound from @${challengedUser.name}", Some(challengingUser), Some(challengedUser))) {
        "[0-9]+".r.findFirstIn(message.text).map { bound =>
          if (bound.toInt > 0) {
            sendMessage(s"The odds are set! @${challengingUser.name} and @${challengedUser.name}, IM me a number 1-${bound}")
            rtmClient.removeEventListener(listener)
            val imOddsListener = listenForImOdds(rtmClient, message.channel, bound.toInt, uidsToUsers, channelIdToUsers, challengingUser, challengedUser, None, None)
            updateReset(() => {
              sendMessage(s"@${challengingUser.name} and @${challengedUser.name}: ya done waited too long.")
              rtmClient.removeEventListener(imOddsListener)
              listenForOddsAre(rtmClient, uidsToUsers, channelIdToUsers)
            })
          } else {
            sendMessage("Please enter an upper bound greater than zero.")
          }
        }
      }
    }
    listener
  }

  /**
   * Start a listener using rtmClient which will listen for IMs from the two users in a current challenge.
   */
  def listenForImOdds(rtmClient: SlackRtmClient, groupChannel: String, upperBound: Int, uidsToUsers: Map[String, User], channelIdToUsers: Map[String, User], challengingUser: User, challengedUser: User, firstEntry: Option[Int], firstToRespond: Option[User]): ActorRef = {
    lazy val listener: ActorRef = rtmClient.onMessage { message =>
      val sendMessage = (msg: String) => rtmClient.sendMessage(message.channel, msg)

      val statusMessage = firstToRespond match {
        case Some(first) => s"waiting for an IM from @${if (first == challengingUser) challengedUser.name else challengingUser.name}"
        case _ => s"waiting for IMs from @${challengingUser.name} and @${challengedUser.name}"
      }
      if (message.user != rtmClient.state.self.id && !checkForCommands(rtmClient, message, statusMessage, Some(challengingUser), Some(challengedUser))) {
        channelIdToUsers.get(message.channel).map { user =>
          // Don't want the same user to be able to interact after their first submission
          val doubleMessage = firstToRespond match {
            case Some(firstUser) => firstUser.id == user.id
            case None => false
          }
          if ((user.id == challengingUser.id || user.id == challengedUser.id) && !doubleMessage) {
            // This is the first instant message from one of the engaged users
            "[0-9]+".r.findFirstIn(message.text).map(_.toInt).map { number =>
              if (number <= 0 || number > upperBound) {
                // Invalid entry
                sendMessage(s"Please enter a number 1-${upperBound}")
              } else {
                // Valid entry, either compare entries if we have both, or wait for the second entry
                val currentUser = if (user.id == challengingUser.id) challengingUser else challengedUser
                (firstToRespond, firstEntry) match {
                  case (Some(firstUser), Some(firstNumber)) if (firstUser.id != user.id) => {
                    // This is the second IM response, we have both!
                    val baseMessage = s"The results are in! Out of ${upperBound}, ${firstUser.name} entered *${firstNumber}*, and ${currentUser.name} entered *${number}*. "
                    val finalMessage = baseMessage + (if (number == firstNumber) s"Suck it @${challengedUser.name}!!" else s"You'll get 'em next time, @${challengingUser.name}.")
                    rtmClient.sendMessage(groupChannel, finalMessage)
                    rtmClient.removeEventListener(listener)
                    listenForOddsAre(rtmClient, uidsToUsers, channelIdToUsers)
                    updateReset(() => {})
                  }
                  case _ => {
                    // This is the first IM response
                    rtmClient.removeEventListener(listener)
                    var nextImOddsListener = listenForImOdds(rtmClient, groupChannel, upperBound, uidsToUsers, channelIdToUsers, challengingUser, challengedUser, Some(number), Some(currentUser))
                    updateReset(() => {
                      val slowUser = if (user.id == challengingUser.id) challengedUser else challengingUser
                      rtmClient.sendMessage(groupChannel, s"@${slowUser.name}: ya done waited too long.")
                      rtmClient.removeEventListener(nextImOddsListener)
                      listenForOddsAre(rtmClient, uidsToUsers, channelIdToUsers)
                    })
                  }
                }
              }
            }
          }
        }
      }
    }
    listener
  }
}
