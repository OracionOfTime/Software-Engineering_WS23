package de.uni_saarland.cs.se
package traits

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

trait Colored(val color: String)

case class PlainTextMessage(
    override val sender: Int,
    override val message: String
) extends TextMessage(message),
      Message(sender)

case class ColoredTextMessage(
    override val sender: Int,
    override val message: String,
    override val color: String
) extends TextMessage(message),
      Message(sender),
      Colored(color)

//==============================================================================
// Server
//==============================================================================

trait ChatServer() extends Server[Message] {
  val serverId: Int

  //additional variables
  var encryptionMethod: EncryptionMethod
  var clients: Map[Int, ClientConnection[Message]]
  var logger = Logger()

  //base functions
  override def handleMessage(message: Message): Unit
  override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit

  //getters
  protected def getClientsMap(): Map[Int, ClientConnection[Message]]
  protected def getUnauthenticatedClients(): Map[Int, ClientConnection[Message]]

  //setters
  protected def setClientsMap(client: Int, clientConnection: ClientConnection[Message]): Unit
  protected def addUnauthenticatedClients(client: Int, clientConnection: ClientConnection[Message]): Unit
  protected def removeUnauthenticatedClients(client: Int): Unit

  //helpers
  def authenticate(message: AuthenticationMessage): Unit
  def broadcast(message: TextMessage): Unit
  def isAuthenticated(clientId: Int): Boolean
  def sendMessage(client: ClientConnection[Message], message: Message): Unit
  def createArray(username:String, password:String): Array[String]
}


//////////////////Base Server//////////////////////////////////////////////////////////////////////////////////////


class ChatServerBase(sId: Int) extends ChatServer {
  override val serverId: Int = sId

  //additional variables
  var encryptionMethod = null
  var clients: Map[Int, ClientConnection[Message]] = new HashMap()
  private var unauthenticatedClientsMap: Map[Int, ClientConnection[Message]] = new HashMap()

  //base functions
  override def handleMessage(message: Message): Unit = {
    message match {
      case authMessage: AuthenticationMessage =>
        authenticate(authMessage)
      case textMessage: TextMessage =>
        broadcast(textMessage)
    }
  }
  override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
    setClientsMap(clientConnection.clientId, clientConnection)
  }

  //getters
  override protected def getUnauthenticatedClients(): Map[Int, ClientConnection[Message]] = {
    unauthenticatedClientsMap
  }
  override protected def getClientsMap(): Map[Int, ClientConnection[Message]] = {
    clients
  }

  //setters
  override def addUnauthenticatedClients(client: Int, clientConnection: ClientConnection[Message]): Unit = {
    unauthenticatedClientsMap += (client, clientConnection)
  }
  override def removeUnauthenticatedClients(client: Int): Unit = {
    unauthenticatedClientsMap -= client
  }
  override protected def setClientsMap(client: Int, clientConnection: ClientConnection[Message]): Unit = {
    clients += (client, clientConnection)
  }

  //helpers
  override def authenticate(message: AuthenticationMessage): Unit = {
    throw ConfigurationError()
  }
  override def broadcast(message: TextMessage): Unit = {
    val sender = message.sender

    for(connection <- getClientsMap().values) {
      connection.sendMessage(message)
    }
  }
  override def isAuthenticated(clientId: Int): Boolean = {
    getClientsMap().keySet.contains(clientId)
  }
  override def createArray(username: String, password: String): Array[String] = {
    Array(username, password)
  }
  override def sendMessage(client: ClientConnection[Message], message: Message): Unit = {
    client.sendMessage(message)
  }
}


//////////////////////Authenticating Server//////////////////////////////////////////////////////////////////////


trait ServerAuthentication(registeredUsersMap: Map[String, String]) extends ChatServer {
  val registeredUsers: Map[String, String] = registeredUsersMap

  //base functions
  override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
    addUnauthenticatedClients(clientConnection.clientId, clientConnection)
  }

  //helpers
  override def broadcast(message: TextMessage): Unit = {
    val sender = message.sender

    if(!isAuthenticated(sender)){
      sendMessage(getUnauthenticatedClients().get(sender).get,
        PlainTextMessage(serverId, "You must authenticate before sending messages."))
      return
    }
    for (connection <- getClientsMap().values) {
      connection.sendMessage(message)
    }
  }
  override def isAuthenticated(clientId: Int): Boolean = {
    getClientsMap().keySet.contains(clientId)
  }
  override def sendMessage(client: ClientConnection[Message], message: Message): Unit = {
    client.sendMessage(message)
  }
  override def authenticate(message: AuthenticationMessage): Unit = {
    val sender = message.sender
    val Array(username, password) = createArray(message.username, message.password)

    authenticateHelper(username, password, sender)
  }
  def authenticateHelper(username:String, password: String, sender: Int ): Unit = {
    if (registeredUsers.get(username).contains(password)) {
      val connection = getUnauthenticatedClients().get(sender).get
      removeUnauthenticatedClients(sender)
      setClientsMap(sender, connection)
    } else {
      sendMessage(getUnauthenticatedClients().get(sender).get, PlainTextMessage(serverId, "Authentication failed.")
      )
    }
  }
}

//------------->Authentication: Encryption

trait ServerAuthenticationEncryption() extends ServerAuthentication{

  //helpers
  override def authenticate(message: AuthenticationMessage): Unit = {
    val sender = message.sender
    val Array(username, password) = createArray(message.username, message.password)

    super.authenticateHelper(username, password, sender)
  }
  override def createArray(username: String, password: String): Array[String] = {
    Array(encryptionMethod.encrypt(username),
      encryptionMethod.encrypt(password))
  }
  override def sendMessage(client: ClientConnection[Message], message: Message): Unit = {
    val encryptedMessage = message match {
      case AuthenticationMessage(sender, username, password) =>
        AuthenticationMessage(
          sender,
          encryptionMethod.encrypt(username),
          encryptionMethod.encrypt(password)
        )
      case ColoredTextMessage(sender, message, color) =>
        ColoredTextMessage(sender, encryptionMethod.encrypt(message), color)
    }
    client.sendMessage(encryptedMessage)
  }
}

//------------->Authentication: Logging

trait ServerAuthenticationLogging() extends ServerAuthentication{

  //base functions
  override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
    logger.log(s"New client: ${clientConnection.clientId}")
    super.acceptClientConnection(clientConnection)
  }

  //helpers
  override def broadcast(message: TextMessage): Unit = {
    val sender = message.sender

    if (!isAuthenticated(sender)) {
      logger.log(
        s"Rejected message from unauthenticated client: ${message.sender}"
      )
      sendMessage(getUnauthenticatedClients().get(sender).get,
        PlainTextMessage(serverId, "You must authenticate before sending messages."))
      return
    }
    logger.log(s"Broadcasting message from sender ${message.sender}")

    for (connection <- getClientsMap().values) {
      connection.sendMessage(message)
    }
  }
  override def authenticate(message: AuthenticationMessage): Unit = {
    val sender = message.sender
    val Array(username, password) = createArray(message.username, message.password)

    if(registeredUsers.get(username).contains(password)) {
      logger.log(s"Successfully authenticated client: $sender")
      val connection = getUnauthenticatedClients().get(sender).get
      removeUnauthenticatedClients(sender)
      setClientsMap(sender, connection)
    } else {
      logger.log(s"Failed to authenticate client: $sender")
      super.sendMessage(
        getUnauthenticatedClients().get(sender).get,
        PlainTextMessage(serverId, "Authentication failed.")
      )
    }
  }
}


////////////////////////Encryption Server///////////////////////////////////////////////////////////////////////////


trait ServerEncryption(encryptionMeth: EncryptionMethod) extends ChatServer {
  encryptionMethod = encryptionMeth

  //base functions
  override def handleMessage(message: Message): Unit = {
    message match {
      case authMessage: AuthenticationMessage =>
        authenticate(authMessage)
      case textMessage: TextMessage =>
        val message = encryptionMethod.decrypt(textMessage.message)
        val texMess = PlainTextMessage(textMessage.sender, message)
        broadcast(texMess)
    }
  }

  //helpers
  override def sendMessage(client: ClientConnection[Message], message: Message): Unit = {
    val encryptedMessage = message match {
      case AuthenticationMessage(sender, username, password) =>
        AuthenticationMessage(
          sender,
          encryptionMethod.encrypt(username),
          encryptionMethod.encrypt(password)
        )
      case ColoredTextMessage(sender, message, color) =>
        ColoredTextMessage(sender, encryptionMethod.encrypt(message), color)
    }
    client.sendMessage(encryptedMessage)
  }
}


////////////////////////Logging Server///////////////////////////////////////////////////////////////////////////


trait ServerLogging() extends ChatServer {

  override def acceptClientConnection(clientConnection: ClientConnection[Message]): Unit = {
    logger.log(s"New client: ${clientConnection.clientId}")

    setClientsMap(clientConnection.clientId, clientConnection)
  }

  override def broadcast(message: TextMessage): Unit = {
    val sender = message.sender

    logger.log(s"Broadcasting message from sender ${message.sender}")

    for (connection <- getClientsMap().values) {
      connection.sendMessage(message)
    }
  }
}



//==============================================================================
// Client
//==============================================================================
trait ChatClient() extends Client[Message] {
  val clientId: Int
  val serverId: Int
  val view: View

  //additional variables
  var isAuthenticated: Boolean = false
  val serverConnection: ServerConnection[Message]
  var logger: Logger = Logger()
  var encryptionMethod: EncryptionMethod

  //base functions
  override def handleMessage(message: Message): Unit
  def send(message: String): Unit

  //helpers
  def send(message: String, color: String): Unit
  def displayMessage(message: Message): Unit
  def authenticate(username: String, password: String): Unit
}


////////////////////////////////////Base Client/////////////////////////////////////////////////////////////////


class ChatClientBase(cId: Int, sId: Int, networkSimulator: NetworkSimulator[Message]) extends ChatClient {
  override val view = View()
  override val serverId: Int = sId
  var encryptionMethod: EncryptionMethod = null
  override val clientId = cId
  override val serverConnection = networkSimulator
    .connectToServer(clientId, serverId)
    .getOrElse(throw IllegalStateException("Unable to connect to server."))


  //base functions
  override def handleMessage(message: Message): Unit = {
    message match {
      case authMessage: AuthenticationMessage =>
        throw ConfigurationError()
      case textMessage: TextMessage =>
        displayMessage(message)
    }
  }

  override def send(message: String): Unit = {
    serverConnection.sendMessage(PlainTextMessage(clientId, message))
  }

  //helpers
  override def authenticate(username: String, password: String): Unit = {
    throw ConfigurationError()
  }
  override def send(message: String, color: String): Unit = {
    throw ConfigurationError()
  }
  override def displayMessage(message: Message): Unit = {
   message match {
     case textMessage: PlainTextMessage =>
       view.printMessage(message.sender, textMessage.message)
     case coloredTextMessage: ColoredTextMessage =>
       throw ConfigurationError()
   }
  }
}


////////////////////////////////////Coloring Client/////////////////////////////////////////////////////////////////


trait ClientColoring() extends ChatClient {

  //helpers
  override def send(message: String, color: String): Unit = {
    serverConnection.sendMessage(ColoredTextMessage(clientId, message, color))
  }
  override def displayMessage(message: Message): Unit = {
    message match {
      case textMessage: PlainTextMessage =>
        throw ConfigurationError()
      case coloredTextMessage: ColoredTextMessage =>
        view.printMessage(coloredTextMessage.sender, coloredTextMessage.message, coloredTextMessage.color)
    }
  }
}

//---------------------->Coloring: Encryption

trait ClientColorEncryption() extends ClientColoring {

  //helpers
  override def displayMessage(message: Message): Unit = {
    message match {
      case textMessage: PlainTextMessage =>
        throw ConfigurationError()
      case coloredTextMessage: ColoredTextMessage =>
        view.printMessage(coloredTextMessage.sender, encryptionMethod.decrypt(coloredTextMessage.message),
          coloredTextMessage.color)
    }
  }
  override def send(message: String, color: String): Unit = {
    serverConnection.sendMessage(ColoredTextMessage(clientId, encryptionMethod.encrypt(message), color))
  }
}

//---------------------->Coloring: Logging

trait ClientColorLogging() extends ClientColoring {

  //base function
  override def send(message: String, color: String): Unit = {
    logger.log(s"Sending message: ${PlainTextMessage(clientId, message)}")
    super.send(message, color)
  }
}


////////////////////////////////////Authenticating Client///////////////////////////////////////////////////


trait ClientAuthentication() extends ChatClient {

  //base functions
  override def handleMessage(message: Message): Unit = {
    message match {
      case authMessage: AuthenticationMessage =>
        if(authMessage.sender == serverId) {
          isAuthenticated = true
        }
      case textMessage: TextMessage =>
        displayMessage(message)
    }
  }

  //helpers
  override def authenticate(username: String, password: String): Unit = {
    if (!isAuthenticated) {
      val message = AuthenticationMessage(clientId, username, password)
      helperForAuthenticate(message,  username, password)
    }
  }
  def helperForAuthenticate(message: AuthenticationMessage, username: String, password: String): Unit = {
    serverConnection.sendMessage(message)
  }
}

//---------------------->Authentication: Encryption

trait  ClientAuthenticationEncryption() extends ClientAuthentication {

  //helper
  override def authenticate(username: String, password: String): Unit = {
    if (!isAuthenticated) {
      val message = AuthenticationMessage(clientId,
        encryptionMethod.encrypt(username),
        encryptionMethod.encrypt(password))
      helperForAuthenticate(message, username, password)
    }
  }
}

//---------------------->Authentication: Logging

trait ClientAuthenticationLogging() extends ClientAuthentication {

  //base functions
  override def handleMessage(message: Message): Unit = {
    logger.log(s"Received message from sender ${message.sender}")
    super.handleMessage(message)
  }
  override def send(message: String): Unit = {
    logger.log(s"Sending message: ${PlainTextMessage(clientId, message)}")
    serverConnection.sendMessage(PlainTextMessage(clientId, message))
  }

  //helper
  override def helperForAuthenticate(message: AuthenticationMessage, username: String, password: String): Unit = {
    logger.log(
      s"Sending authentication request: ${AuthenticationMessage(clientId, username, password
      )}"
    )
    super.helperForAuthenticate(message, username, password)
  }
}


////////////////////////////////////Encrypting Client///////////////////////////////////////////////////


trait ClientEncryption(encryptionMeth: EncryptionMethod) extends ChatClient {
   encryptionMethod = encryptionMeth

  //helper
  override def send(message: String): Unit = {
    val text = encryptionMethod.encrypt(message)
    serverConnection.sendMessage(PlainTextMessage(clientId, text))
  }
}


////////////////////////////////////Logging Client///////////////////////////////////////////////////


trait ClientLogging() extends ChatClient {

  //base functions
  override def handleMessage(message: Message): Unit = {
    logger.log(s"Received message from sender ${message.sender}")

    message match {
      case authMessage: AuthenticationMessage =>
        throw ConfigurationError()
      case textMessage: TextMessage =>
        displayMessage(message)
    }
  }
  override def send(message: String): Unit = {
    logger.log(s"Sending message: ${PlainTextMessage(clientId, message)}")

    serverConnection.sendMessage(PlainTextMessage(clientId, message))
  }

  //helper
  override def authenticate(username: String, password: String): Unit = {
    throw ConfigurationError()
  }
}