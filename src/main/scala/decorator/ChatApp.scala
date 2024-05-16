package de.uni_saarland.cs.se
package decorator

import util.*

import scala.collection.immutable.HashMap

//==============================================================================
// Messages
//==============================================================================
trait Message(val sender: Int)
case class AuthenticationMessage(
    override val sender: Int,
    username: String,
    password: String
) extends Message(sender) {
  override def toString: String = s"[$sender] u=$username p=$password"
}

trait TextMessage(val message: String) extends Message {
  override def toString: String = s"[$sender] $message"
}

case class TextMessageBase(
    override val sender: Int,
    override val message: String
) extends TextMessage(message),
      Message(sender)

abstract class TextMessageDecorator(private val decorated: TextMessage)
    extends TextMessage(decorated.message),
      Message(decorated.sender)

case class ColoredTextMessage(
    private val parent: TextMessage,
    color: String
) extends TextMessageDecorator(parent) {
  override def toString: String = parent.toString
}

//==============================================================================
// Server
//==============================================================================

trait ChatServer() extends Server[Message] {
  val serverId: Int
  //val logger: Logger

  //new values
  var registeredUsers: Map[String, String]

  override def handleMessage(message: Message): Unit

  override def acceptClientConnection(
      clientConnection: ClientConnection[Message]
  ): Unit

  //getters
  def getUnauthenticatedClients(client: Int): ClientConnection[Message]
  def getRegisteredUsers(): Map[String, String]
  def getClients(): Map[Int, ClientConnection[Message]]
  def getEncryptionMethod(): EncryptionMethod

  //setters
  def removeUnauthenticatedClients(sender: Int): Unit
  def addClients(client: Int, connection: ClientConnection[Message]): Unit

  //new methods
  def broadcast(message: TextMessage): Unit
  def authenticate(message: AuthenticationMessage): Unit
  def isAuthenticated(clientId: Int): Boolean
  def sendMessage(client: ClientConnection[Message], message: Message): Unit

  // helper
  def helperEncrypt(): Boolean
  def createArray(username: String, password: String) : Array[String]
  def helperAuthenticate(): Boolean
  def helperBroadcastSend(sender: Int): Unit
  def helperBroadcastLoop(message: Message): Unit
  def helperAuthenticateClients(sender:Int): Unit
  def helperAuthenticateSend(sender:Int): Unit
}

////////////////////BASE SERVER///////////////////////////////////////////////////

class ChatServerBase(val serverId: Int) extends ChatServer {
  //override val logger:Logger = Logger()
  var clients: Map[Int, ClientConnection[Message]] = new HashMap()
  var registeredUsers: Map[String, String] = new HashMap()

  //getters
  override def getRegisteredUsers(): Map[String, String] = {
    //getRegisteredUsers()
    throw ConfigurationError()
   }
  override def getClients(): Map[Int, ClientConnection[Message]] = {
    clients
   //throw ConfigurationError()
  }
  override def getUnauthenticatedClients(client: Int): ClientConnection[Message] = {
    //getUnauthenticatedClients(client)
    throw ConfigurationError()
  }
  override def getEncryptionMethod(): EncryptionMethod = {
    throw ConfigurationError()
  }

  //setters
  override def addClients(client: Int, connection: ClientConnection[Message]) = {
    clients += (client, connection)
  }
  override def removeUnauthenticatedClients(sender: Int): Unit = {
    //removeUnauthenticatedClients(sender)
    throw ConfigurationError()
  }
 override def helperAuthenticateSend(sender: Int): Unit  = {
   //helperAuthenticateSend(sender)
    throw ConfigurationError()
  }
  override def helperBroadcastSend(sender: Int): Unit = {
    //helperBroadcastSend(sender)
    throw ConfigurationError()
  }

  //helper
  def helperEncrypt(): Boolean = {
    false
  }
  def createArray(username: String, password: String): Array[String] = {
    Array(username, password)
  }
  def helperAuthenticate(): Boolean = {
    false
  }
  def helperBroadcastLoop(message: Message): Unit = {
    for (connection <- clients.values) {
      connection.sendMessage(message)
    }
  }
  def helperAuthenticateClients(sender: Int): Unit = {
    throw ConfigurationError()
    /*val connection = getUnauthenticatedClients(sender)
    removeUnauthenticatedClients(sender)
    clients += (sender, connection)*/
  }

  //base functions
  override def handleMessage(message: Message): Unit = {
    message match {
      case authMessage: AuthenticationMessage =>
        authenticate(authMessage)
      case textMessage: TextMessage =>
        broadcast(textMessage)
    }
  }
  def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
     addClients(clientConnection.clientId, clientConnection)
   //clients += (clientConnection.clientId, clientConnection)
  }
  def broadcast(message: TextMessage): Unit = {
    helperBroadcastLoop(message)
  }
   def authenticate(message: AuthenticationMessage): Unit = {
    throw ConfigurationError()
  }
   def isAuthenticated(clientId: Int): Boolean = {
    clients.keySet.contains(clientId)
  }
   def sendMessage(client: ClientConnection[Message], message: Message): Unit ={
    client.sendMessage(message)
  }
}

////////////////////////Abstract Decorator/////////////////////////////////////////////////////

 abstract class ServerDecorator(private val AbstractServer: ChatServer) extends ChatServer{
   override val serverId: Int = AbstractServer.serverId
   //override val logger: Logger = AbstractServer.logger
   var unauthenticatedClients: Map[Int, ClientConnection[Message]] =
     new HashMap()
   var registeredUsers: Map[String,String] = new HashMap()
   var encryption: EncryptionMethod = null

    //getters
   def getUnauthenticatedClients(sender:Int): ClientConnection[Message] = {
    AbstractServer.getUnauthenticatedClients(sender)
   }
   def getRegisteredUsers(): Map[String, String] = {
     AbstractServer.getRegisteredUsers()
   }
   override def getClients(): Map[Int, ClientConnection[Message]] = {
     AbstractServer.getClients()
   }
   def getEncryptionMethod(): EncryptionMethod = {
     AbstractServer.getEncryptionMethod()
   }

   //setters
   def removeUnauthenticatedClients(sender: Int): Unit = {
    AbstractServer.removeUnauthenticatedClients(sender)
   }
   override def addClients(client: Int, connection: ClientConnection[Message]): Unit = {
     AbstractServer.addClients(client, connection)
   }

   //helper
   def helperEncrypt(): Boolean = {
     AbstractServer.helperEncrypt()
   }
   def createArray(username: String, password: String): Array[String] = {
     AbstractServer.createArray(username, password)
   }
   def helperAuthenticate(): Boolean = {
     AbstractServer.helperAuthenticate()
   }
   def helperBroadcastSend(sender: Int): Unit = {
     AbstractServer.helperBroadcastSend(sender)
   }
   def helperBroadcastLoop(message: Message): Unit = {
     AbstractServer.helperBroadcastLoop(message)
   }
   def helperAuthenticateClients(sender: Int): Unit = {
     AbstractServer.helperAuthenticateClients(sender)
   }
   def helperAuthenticateSend(sender: Int): Unit = {
     AbstractServer.helperAuthenticateSend(sender)
   }
    def handleMessage(message: Message): Unit = {
      AbstractServer.handleMessage(message)
   }

    //base functions
    def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
     AbstractServer.acceptClientConnection(clientConnection)
   }
    def broadcast(message: TextMessage): Unit = {
     AbstractServer.broadcast(message)
   }
    def authenticate(message: AuthenticationMessage): Unit = {
     AbstractServer.authenticate(message)
   }
   def isAuthenticated(clientId: Int): Boolean = {
     AbstractServer.isAuthenticated(clientId)
   }
    def sendMessage(client: ClientConnection[Message], message: Message): Unit = {
     AbstractServer.sendMessage(client, message)
   }
}

/////////////////////EncryptingDecorator////////////////////////////////

 class EncryptingServer(base: ChatServer, encryptionMethod: EncryptionMethod) extends
  ServerDecorator(base){
   encryption  = encryptionMethod

   //base functions
   override def handleMessage(message: Message): Unit = {
     message match {
       case authMessage: AuthenticationMessage =>
         super.authenticate(authMessage)
       case textMessage: TextMessageBase =>
         val message = encryptionMethod.decrypt(textMessage.message)
         val texMess = TextMessageBase(textMessage.sender, message)
         super.broadcast(texMess)
     }
   }
   override def sendMessage(client: ClientConnection[Message],
    message: Message): Unit =  {
    val encryptedMessage = message match {
      case AuthenticationMessage(sender, username, password) =>
        AuthenticationMessage(
          sender,
          encryption.encrypt(username),
          encryption.encrypt(password)
        )
      case ColoredTextMessage(TextMessageBase(sender, message), color) =>
        ColoredTextMessage(TextMessageBase(sender, encryptionMethod.encrypt(message)), color)
    }
    client.sendMessage(encryptedMessage)
  }

  //helper
  override def createArray(username: String, password: String): Array[String] = {
    Array(encryption.decrypt(username),
      encryption.decrypt(password))
  }
  override def helperEncrypt(): Boolean = {
     true
  }
  override def getEncryptionMethod(): EncryptionMethod = {
     encryption
   }
}

////////////////////////AuthenticatingDecorator////////////////////////////////////

 class AuthenticatingServer(base: ChatServer, map: Map[String, String]) extends
  ServerDecorator(base){
  registeredUsers =  registeredUsers ++ map

  //getter
  override def getUnauthenticatedClients(sender: Int): ClientConnection[Message] = {
    unauthenticatedClients(sender)
  }
  override def getRegisteredUsers(): Map[String, String] = {
     registeredUsers
   }

  //setter
  override def removeUnauthenticatedClients(sender: Int): Unit = {
    unauthenticatedClients -= sender
  }


  //base functions
  override def handleMessage(message: Message): Unit = {
     message match {
       case authMessage: AuthenticationMessage =>
         authenticate(authMessage)
       case textMessage: TextMessage =>
         if(helperEncrypt()){
           val message = getEncryptionMethod().decrypt(textMessage.message)
           val texMess = TextMessageBase(textMessage.sender, message)
           broadcast(texMess)
         } else {
           broadcast(textMessage)
         }
     }
   }
  override def broadcast(message: TextMessage): Unit = {
     val sender = message.sender

     if (!isAuthenticated(sender)) {
       helperBroadcastSend(sender)
       return
     }
     helperBroadcastLoop(message)
   }
  override def authenticate(message: AuthenticationMessage): Unit = {
     val sender = message.sender

     val Array(username, password) =
       createArray(message.username, message.password)

     if (registeredUsers.get(username).contains(password)) {
       helperAuthenticateClients(sender)
     } else {
       helperAuthenticateSend(sender)
     }
   }
  override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
     unauthenticatedClients += (clientConnection.clientId, clientConnection)
   }


  //helper
  override def helperAuthenticateClients(sender: Int): Unit = {
     val connection = getUnauthenticatedClients(sender)
     removeUnauthenticatedClients(sender)
     addClients(sender, connection)
   }
  override def helperBroadcastLoop(message: Message): Unit = {
     for (connection <- getClients().values) {
       connection.sendMessage(message)
     }
   }
  override def helperAuthenticate(): Boolean = {
     true
   }
  override def helperBroadcastSend(sender: Int): Unit = {
     sendMessage(
       unauthenticatedClients(sender),
       TextMessageBase(
         serverId,
         "You must authenticate before sending messages."
       )
     )
   }
  override def helperAuthenticateSend(sender: Int): Unit = {
     sendMessage(
       unauthenticatedClients(sender),
       TextMessageBase(serverId, "Authentication failed.")
     )
   }
}


////////////////////////LoggingDecorator///////////////////////////////////

class LoggingServer(base: ChatServer) extends ServerDecorator(base) {
  val logger = Logger()

  //helper
  override def helperAuthenticateClients(sender: Int): Unit = {
    val connection = getUnauthenticatedClients(sender)
    removeUnauthenticatedClients(sender)
    addClients(sender, connection)
  }
  override def helperBroadcastLoop(message: Message): Unit = {
    for (connection <- getClients().values) {
      connection.sendMessage(message)
    }
  }

  //base functions
  override def handleMessage(message: Message): Unit = {
    message match {
      case authMessage: AuthenticationMessage =>
        authenticate(authMessage)
      case textMessage: TextMessage =>
        if (helperEncrypt()) {
          val message = getEncryptionMethod().decrypt(textMessage.message)
          val texMess = TextMessageBase(textMessage.sender, message)
          broadcast(texMess)
        } else {
          broadcast(textMessage)
        }
    }
  }
  override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
    logger.log(s"New client: ${clientConnection.clientId}")
    super.acceptClientConnection(clientConnection)
  }
  override def broadcast(message: TextMessage): Unit = {
    val authCheck = helperAuthenticate()
    val sender = message.sender

    if(authCheck) {
      if (!isAuthenticated(sender)) {
        logger.log(s"Rejected message from unauthenticated client: ${message.sender}")

        helperBroadcastSend(sender)
        return
      }
    }

    logger.log(s"Broadcasting message from sender ${message.sender}")

    helperBroadcastLoop(message)
  }
  override def authenticate(message: AuthenticationMessage): Unit = {
    val authCheck = helperAuthenticate()

    if(authCheck){
      val sender = message.sender
      val Array(username, password) =
        createArray(message.username, message.password)

      if(getRegisteredUsers().get(username).contains(password)) {
        logger.log(s"Successfully authenticated client: $sender")
        helperAuthenticateClients(sender)
      }else {
        logger.log(s"Failed to authenticate client: $sender")
        helperAuthenticateSend(sender)
      }
    } else {
      super.authenticate(message)
    }
  }
}


//==============================================================================
// Client
//==============================================================================
trait ChatClient() extends Client[Message] {
  val clientId: Int
  val view: View
  val logger: Logger

  //new variables
  val serverConnection: ServerConnection[Message]
  var isAuthenticated: Boolean
  val encryptionMethod: EncryptionMethod

  //getter
  def getServerId(): Int

  //new methods
  override def handleMessage(message: Message): Unit
  def send(message: String): Unit
  def send(message: String, color: String): Unit
  def authenticate(username: String, password: String): Unit

  //helper
  def displayMessage(message: TextMessage): Unit
  def displayColMessage(message: ColoredTextMessage, color: String): Unit
  def createMessage(username: String, password:String): AuthenticationMessage
  def createSendText(encryptionMethod: EncryptionMethod, message: String): String
  def authCheck(): Boolean
}

//////////////////Base Client/////////////////////////////////////////////////////

class ChatClientBase(client: Int, serverId: Int, networkSimulator: NetworkSimulator[Message])
extends ChatClient{
  val clientId = client
  val view = View()
  val logger = Logger()

  //new values
  val encryptionMethod = encryptionMethod
  val serverConnection = networkSimulator
    .connectToServer(clientId, serverId)
    .getOrElse(throw IllegalStateException("Unable to connect to server."))
  var isAuthenticated = false

  //getter
  override def getServerId(): Int = {
    serverId
  }

  //helper
  override def displayMessage(message: TextMessage): Unit = {
    val text = message.message
    view.printMessage(message.sender, text)
  }
  override def displayColMessage(message: ColoredTextMessage, color: String): Unit = {
    if (authCheck()) {
      displayColMessage(message, color)
    } else {
      throw ConfigurationError()
    }
  }
  override def createMessage(username: String, password: String): AuthenticationMessage = {
    AuthenticationMessage(clientId, username, password)
  }
  override def createSendText(encryptionMethod: EncryptionMethod, message: String): String = {
    message
  }
  override def authCheck(): Boolean = {
    false
  }

  //base functions
  override def handleMessage(message: Message): Unit = {
    message match {
      case authMessage: AuthenticationMessage =>
        throw ConfigurationError()
      case textMessage: TextMessage =>
        displayMessage(textMessage)
    }
  }
  override def send(message: String): Unit = {
    val text = message

    val textMessage = TextMessageBase(clientId, text)

    serverConnection.sendMessage(textMessage)
  }
  override def send(message: String, color: String): Unit = {
    if(authCheck()) {
      send(message, color)
    } else {
      throw ConfigurationError()
    }
  }
  override def authenticate(username: String, password: String): Unit = {
    throw ConfigurationError()
  }
}

///////////////////////Abstract Decorator///////////////////////////////////////////////////////

abstract class ChatClientDecorator(private val AbstractClient: ChatClient) extends ChatClient {
  val clientId = AbstractClient.clientId
  val view = AbstractClient.view
  val logger = AbstractClient.logger

  //new variables
  val serverConnection = AbstractClient.serverConnection
  var isAuthenticated = AbstractClient.isAuthenticated
  val encryptionMethod = AbstractClient.encryptionMethod

  //getters
  override def getServerId(): Int = {
    AbstractClient.getServerId()
  }

  //helper
  override def displayMessage(message: TextMessage): Unit = {
    AbstractClient.displayMessage(message)
  }
  def displayColMessage(message: ColoredTextMessage, color: String): Unit = {
    AbstractClient.displayColMessage(message, color)
  }
  def checkColored(): Boolean = {
    false
  }
  override def createSendText(encryptionMethod: EncryptionMethod, message: String): String = {
    AbstractClient.createSendText(encryptionMethod, message)
  }
  override def authCheck(): Boolean = {
    AbstractClient.authCheck()
  }
  override def createMessage(username: String, password: String): AuthenticationMessage = {
    AbstractClient.createMessage(username, password)
  }

  //base functions
  override def handleMessage(message: Message): Unit = {
    AbstractClient.handleMessage(message)
  }
  override def send(message: String): Unit = {
    AbstractClient.send(message)
  }
  override def send(message: String, color: String): Unit = {
    AbstractClient.send(message, color)
  }
  override def authenticate(username: String, password: String): Unit = {
    AbstractClient.authenticate(username, password)
  }
}

////////////////////Color Decorator//////////////////////////////////////////////

class ColoringClient(base: ChatClient) extends ChatClientDecorator(base) {

  //helper
  override def checkColored(): Boolean = {
    true
  }
  override def displayColMessage(message: ColoredTextMessage, color: String): Unit = {
    val text = message.message
    view.printMessage(message.sender, text, color)
  }

  //base functions
  override def handleMessage(message: Message): Unit = {
    message match {
      case authMessage: AuthenticationMessage =>
        throw ConfigurationError()
      case coloredTextMessage: ColoredTextMessage =>
        displayColMessage(coloredTextMessage, coloredTextMessage.color)
      case textMessage: TextMessage =>
        displayMessage(textMessage)
    }
  }
  override def send(message: String, color: String): Unit = {
    val text = message
    val textMessage = ColoredTextMessage(TextMessageBase(clientId, text), color)
    serverConnection.sendMessage(textMessage)
  }
}

////////////////////Encrypting Decorator//////////////////////////////////////////////////

class EncryptingClient(base: ChatClient, encryptionMeth: EncryptionMethod) extends
  ChatClientDecorator(base) {
  override val encryptionMethod: EncryptionMethod = encryptionMeth

  //helper
  override def displayMessage(message: TextMessage): Unit = {
    val text = encryptionMethod.decrypt(message.message)
    view.printMessage(message.sender, text)
  }
  override def displayColMessage(message: ColoredTextMessage, color: String): Unit = {
    val text = encryptionMethod.decrypt(message.message)
    view.printMessage(message.sender, text, color)
  }
  override def createMessage(username: String, password: String): AuthenticationMessage = {
    AuthenticationMessage(clientId, encryptionMethod.encrypt(username),
      encryptionMethod.encrypt(password))
  }
  override def createSendText(encryptionMethod: EncryptionMethod, message: String): String = {
    encryptionMethod.encrypt(message)
  }

  //base function
  override def send(message: String): Unit = {
    val text = encryptionMethod.encrypt(message)
    val textMessage = TextMessageBase(clientId, text)
    serverConnection.sendMessage(textMessage)
  }
  override def send(message: String, color: String): Unit = {
    val text = encryptionMethod.encrypt(message)
    val textMessage = ColoredTextMessage(TextMessageBase(clientId, text), color)
  }
}

/////////////////////Authenticating Decorator//////////////////////////////////////////

class AuthenticatingClient(base: ChatClient) extends ChatClientDecorator(base) {

  //helper
  override def authenticate(username: String, password: String): Unit = {
    if (!isAuthenticated) {
      val message = createMessage(username, password)
      serverConnection.sendMessage(message)
    }
  }
  override def authCheck(): Boolean = {
    true
  }

  //base functions
    override def handleMessage(message: Message): Unit = {
      message match {
        case authMessage: AuthenticationMessage =>
          if(authMessage.sender == getServerId()){
            isAuthenticated = true
          }
        case colTextMessage: ColoredTextMessage =>
          if(checkColored()) {
            displayColMessage(colTextMessage, colTextMessage.color)
          }
        case  textMessage: TextMessageBase =>
          displayMessage(textMessage)
      }
    }
}

/////////////////////Logging Decorator///////////////////////////////////////

class LoggingClient(base: ChatClient) extends ChatClientDecorator(base) {

  //base function
  override def handleMessage(message: Message): Unit = {
    logger.log(s"Received message from sender ${message.sender}")
    super.handleMessage(message)
  }
  override def send(message: String): Unit = {
    val text = createSendText(encryptionMethod, message)
    val textMessage = TextMessageBase(clientId, text)
    logger.log(s"Sending message: ${TextMessageBase(clientId, message)}")
    serverConnection.sendMessage(textMessage)
  }
  override def send(message: String, color: String): Unit = {
    val text = createSendText(encryptionMethod, message)
    val textMessage = ColoredTextMessage(TextMessageBase(clientId,text), color)

    logger.log(s"Sending message: ${TextMessageBase(clientId, message)}")
    serverConnection.sendMessage(textMessage)
  }
  override def authenticate(username: String, password: String): Unit = {
    if(authCheck()){
      val message = createMessage(username, password)
      logger.log(
        s"Sending authentication request: ${AuthenticationMessage(clientId, username, password)}"
      )
      serverConnection.sendMessage(message)
    }
  }
}