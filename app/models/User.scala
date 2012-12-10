package models

import org.anormcypher._
import org.anormcypher.CypherParser._

case class User(mail: String,
                firstName: String,
                lastName: String,
                facebookUserName: String,
                facebookAccessToken: String,
                facebookId: String)

object User {

  val userParser: CypherRowParser[User] = {
    str("user.email") ~
    str("user.firstName") ~ 
    str("user.lastName") ~ 
    str("user.facebookUserName") ~
    str("user.facebookAccessToken") ~
    str("user.facebookId") map {
      case email ~ firstName ~ lastName ~ facebookUserName ~ facebookAccessToken ~ facebookId =>
        User(email, firstName, lastName, facebookUserName, facebookAccessToken, facebookId)
    }
  }

  def findByUserName(userName: String): Option[User] = {
    Cypher(
    """
      start user=node:node_auto_index(facebookUserName={userName})
      return user.email, user.firstName, user.lastName, user.facebookUserName, user.facebookAccessToken, user.facebookId
    """)
    .on("userName" -> userName)
    .as(userParser *)
    .headOption
  }

  def create(email: String,
             firstName: String,
             lastName: String, 
             facebookUserName: String, 
             facebookId: String, 
             facebookAccessToken: String) = {
    Cypher("""
      create
        (user {
          email: {email},
          firstName: {firstName},
          lastName: {lastName},
          facebookUserName: {facebookUserName},
          facebookId: {facebookId},
          facebookAccessToken: {facebookAccessToken}
        })
      return user.email, user.firstName, user.lastName, user.facebookUserName, user.facebookAccessToken, user.facebookId
      """).on("email" -> email,
              "firstName" -> firstName, 
              "lastName" -> lastName,
              "facebookUserName" -> facebookUserName,
              "facebookId" -> facebookId,
              "facebookAccessToken" -> facebookAccessToken)
          .as(userParser *)
          .headOption
  }

}
