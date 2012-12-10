package models

import org.anormcypher._
import org.anormcypher.CypherParser._

import java.util.Date

case class User(mail: String,
                firstName: String,
                lastName: String,
                facebookUserName: String,
                facebookAccessToken: String,
                facebookId: String)

case class ActivityRow(friendFirstName: String,
                       friendFacebookUserName: String,
                       activityType: String,
                       locus: Locus)

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
      CREATE
        (user {
          email: {email},
          firstName: {firstName},
          lastName: {lastName},
          facebookUserName: {facebookUserName},
          facebookId: {facebookId},
          facebookAccessToken: {facebookAccessToken}
        })
      RETURN user.email, user.firstName, user.lastName, user.facebookUserName, user.facebookAccessToken, user.facebookId
      """).on("email" -> email,
              "firstName" -> firstName, 
              "lastName" -> lastName,
              "facebookUserName" -> facebookUserName,
              "facebookId" -> facebookId,
              "facebookAccessToken" -> facebookAccessToken)
          .as(userParser *)
          .headOption
  }

  def getFriendsActivity(user: User) = {

    val activityParser: CypherRowParser[ActivityRow] = {
      str("friend.firstName") ~
      str("friend.facebookUserName") ~ 
      str("activityType") ~
      str("locus.foursquareId") map {
        case firstName ~ facebookUserName ~ activityType ~ foursquareId =>
          ActivityRow(firstName, facebookUserName, activityType, Locus.loadByFoursquareId(foursquareId).get)
      }
    }

    Cypher("""
      START user=node:node_auto_index(facebookUserName={userName})
      MATCH user-[:FRIEND_OF]->friend-[activity]->locus
      WHERE locus.type="locus"
      RETURN friend.firstName, friend.facebookUserName, type(activity) as activityType, locus.foursquareId
      ORDER BY activity.date DESC
      LIMIT 20
      """).on("userName" -> user.facebookUserName)
          .as(activityParser *)
  }
  
  def likeLocus(user: User, foursquareId: String) = {
    /* Delete DISLIKES relationship if exists */
    Cypher("""
      START user=node:node_auto_index(facebookUserName={userName})
      MATCH user-[dislikeActivity:DISLIKES]->locus
      WHERE locus.foursquareId = {foursquareId}
      DELETE dislikeActivity
      """).on("userName" -> user.facebookUserName, "foursquareId" -> foursquareId)
          .execute()
    
    /* Create LIKES relationship, and locus node if doesn't exist */
    Cypher("""
      START user=node:node_auto_index(facebookUserName={userName})
      CREATE UNIQUE user-[:LIKES {date: {nowDate}}]->(m {type: "locus", foursquareId:{foursquareId}})
      """).on("userName" -> user.facebookUserName, "nowDate" -> new Date().getTime(), "foursquareId" -> foursquareId)
          .execute()
  }

  def dislikeLocus(user: User, foursquareId: String) = {
    /* Delete LIKES relationship if exists */
    Cypher("""
      START user=node:node_auto_index(facebookUserName={userName})
      MATCH user-[likeActivity:LIKES]->locus
      WHERE locus.foursquareId = {foursquareId}
      DELETE likeActivity
      """).on("userName" -> user.facebookUserName, "foursquareId" -> foursquareId)
          .execute()
    
    /* Create DISLIKES relationship, and locus node if doesn't exist */
    Cypher("""
      START user=node:node_auto_index(facebookUserName={userName})
      CREATE UNIQUE user-[:DISLIKES {date: {nowDate}}]->(m {type: "locus", foursquareId:{foursquareId}})
      """).on("userName" -> user.facebookUserName, "nowDate" -> new Date().getTime(), "foursquareId" -> foursquareId)
          .execute()
  }
  
  def wantLocus(user: User, foursquareId: String) = {
    // TODO
  }
  
  def dontwantLocus(user: User, foursquareId: String) = {
    // TODO
  }

}
