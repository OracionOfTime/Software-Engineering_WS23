package de.uni_saarland.cs.se
package runtime

import util.*

import scala.Console.BLACK
import scala.collection.immutable.HashMap

class ChatConfig(
    val authentication: Boolean,
    val color: Boolean,
    val encryption: Option[EncryptionMethod],
    val logging: Boolean
) {}

//==============================================================================
// Messages
//==============================================================================
sealed trait Message(val sender: Int)
case class AuthenticationMessage(
    override val sender: Int,
    username: String,
    password: String
) extends Message(sender) {
  override def toString: String = s"[$sender] u=$username p=$password"
}

case class TextMessage(
    override val sender: Int,
    message: String,
    color: String = Console.BLACK
) extends Message(sender) {
  override def toString: String = s"[$sender] $message"
}

//==============================================================================
// Server
//==============================================================================
class ChatServer(
    val config: ChatConfig,
    val serverId: Int,
    private val registeredUsers: Map[String, String] = HashMap()
) extends Server[Message] {
  val logger: Logger = Logger()
  private val logging = config.logging
  private val authentication:Boolean = config.authentication
  private var clients: Map[Int, ClientConnection[Message]] = new HashMap()
  private var unauthenticatedClients: Map[Int, ClientConnection[Message]] = new HashMap()

  override def handleMessage(message: Message): Unit = message match {
    case authMess: AuthenticationMessage =>
      authenticate(authMess)
    case texMess: TextMessage =>
      broadcast(texMess)
  }

  override def acceptClientConnection(
      clientConnection: ClientConnection[Message]
  ): Unit = {
    if(logging){
      logger.log(s"New client: ${clientConnection.clientId}")
    }
    if(authentication){
      unauthenticatedClients += (clientConnection.clientId, clientConnection)
    }
    else{
      clients += (clientConnection.clientId, clientConnection)
    }
  }


  private def authenticate(message: AuthenticationMessage): Unit = {
    val sender = message.sender
    val encryption: Option[EncryptionMethod] = config.encryption

    if(!authentication){
      throw ConfigurationError()
    }

    val Array(username, password) =
    encryption match {
      case Some(encryptionMethod: EncryptionMethod) =>
        Array(
          encryptionMethod.decrypt(message.username),
          encryptionMethod.decrypt(message.password)
        )
      case None =>
        Array(message.username, message.password)
    }

    if (registeredUsers.get(username).contains(password)) {
      if(logging){
        logger.log(s"Successfully authenticated client: $sender")
      }
      val connection = unauthenticatedClients(sender)
      unauthenticatedClients -= sender
      clients  += (sender -> connection)
    }
    else {
      if(logging) {
        logger.log(s"Failed to authenticate client: $sender")
      }
      sendMessage(
        unauthenticatedClients(sender),
        TextMessage(serverId, "Authentication failed.")
      )
    }

  }

   private def broadcast(message : TextMessage):Unit = {
    val sender :Int = message.sender

     if(authentication){
       if(!isAuthenticated(sender)) {
         if (logging) {
           logger.log(s"Rejected message from unauthenticated client: ${sender}"
           )
         }
         sendMessage(unauthenticatedClients(sender), TextMessage(serverId,
           "You must authenticate before sending messages."))
         return
       }
     }
     if(logging){
       logger.log(s"Broadcasting message from sender ${message.sender}")
     }
     for (connection <- clients.values) {
       connection.sendMessage(message)
     }
  }


   private def isAuthenticated(clientId: Int):Boolean = {
     val v: Boolean = clients.keySet.contains(clientId)
     return v
   }

   private def sendMessage(client:ClientConnection[Message], message:Message): Unit = {
     val encryption: Option[EncryptionMethod] = config.encryption
     encryption match {
       case Some(encryptionMethod: EncryptionMethod) =>
         val encryptedMessage = message match {
           case AuthenticationMessage(sender, username, password) =>
             AuthenticationMessage(
               sender,
               encryptionMethod.encrypt(username),
               encryptionMethod.encrypt(password)
             )
           case TextMessage(sender, message, color) =>
             TextMessage(sender, encryptionMethod.encrypt(message), color)
         }
         client.sendMessage(encryptedMessage)

       case None =>
         client.sendMessage(message)
     }
   }
}
//==============================================================================
// Client
//==============================================================================
class ChatClient(
    val config: ChatConfig,
    val clientId: Int,
    val serverId: Int,
    networkSimulator: NetworkSimulator[Message]
) extends Client[Message] {
  val view: View     = View()
  val logger: Logger = Logger()
  private val logging = config.logging
  private val authentication = config.authentication
  private val serverConnection = networkSimulator
    .connectToServer(clientId, serverId)
    .getOrElse(throw IllegalStateException("Unable to connect to server."))
  private var isAuthenticated = false

  override def handleMessage(message: Message): Unit = {
    if(logging){
      logger.log(s"Received message from sender ${message.sender}")
    }
    message match {
      case authMessage: AuthenticationMessage =>
        if(authentication){
          if (authMessage.sender == serverId) {
            isAuthenticated = true
          }
        }
        else {
          throw ConfigurationError()
        }
      case textMessage: TextMessage =>
        displayMessage(textMessage)
    }
  }

  private def displayMessage(message: TextMessage):Unit = {
    val colorCheck  = config.color
    val encryption: Option[EncryptionMethod] = config.encryption
    val text =
      encryption match {
        case Some(encryptionMethod: EncryptionMethod) =>
          encryptionMethod.decrypt(message.message)
        case None =>
          message.message
      }
    if(colorCheck){
      view.printMessage(message.sender, text, message.color)
    } else {
      view.printMessage(message.sender, text)
    }
  }


  def send(message: String, color: String = BLACK): Unit = {
    val colorCheck = config.color
    val encryption: Option[EncryptionMethod] = config.encryption
    val text =
      encryption match {
        case Some(encryptionMethod: EncryptionMethod) =>
          encryptionMethod.encrypt(message)
        case None =>
          message
      }
    val textMessage =
      if(colorCheck){
        TextMessage(clientId, text, color)
      }  else{
        TextMessage(clientId, text)
      }
    if(logging){
      logger.log(s"Sending message: ${TextMessage(clientId, message)}")
    }
    serverConnection.sendMessage(textMessage)
  }


  def authenticate(username: String, password: String): Unit = {
    val encryption: Option[EncryptionMethod] = config.encryption
    if(!authentication){
      throw ConfigurationError()
    }
    if (!isAuthenticated) {
      val message =
        encryption match {
          case Some(encryptionMethod: EncryptionMethod) =>
            AuthenticationMessage(
              clientId,
              encryptionMethod.encrypt(username),
              encryptionMethod.encrypt(password)
            )
          case None =>
            AuthenticationMessage(clientId, username, password)
        }
    if(logging){
      logger.log(
        s"Sending authentication request: ${AuthenticationMessage(clientId, username, password)}"
      )
    }
      serverConnection.sendMessage(message)
    }
    }
}
