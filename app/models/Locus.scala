package models

import play.api.libs.json._

import org.anormcypher._
import org.anormcypher.CypherParser._

case class LocusActivityRow(friendFirstName: String,
                            friendFacebookUserName: String,
                            activityType: String)

case class Locus(foursquareId: String,
                 name: String = "",
                 address: Option[String] = None, 
                 postalCode: Option[String] = None,
                 city: Option[String] = None,
                 latitude: Float = 0,
                 longitude: Float = 0,
                 category: String = "", 
                 icon: String = "")

object Locus {

  def fromJson(j: JsValue) = {
    val location = j \ "location"
    val primaryCategory = (j \ "categories").as[List[JsValue]].head

    play.Logger.info(location.toString)
    play.Logger.info(primaryCategory.toString)

    Locus((j \ "id").as[String],
        (j \ "name").as[String],
        (location \ "address").asOpt[String],
        (location \ "postalCode").asOpt[String],
        (location \ "city").asOpt[String],
        (location \ "lat").as[Float],
        (location \ "lng").as[Float],
        (primaryCategory \ "shortName" ).as[String],
        (primaryCategory \ "icon" \ "prefix").as[String] + "bg_64" +
          (primaryCategory \ "icon" \ "suffix").as[String]
    )
  }

  def getFriendsActivity(user: User, foursquareId: String) = {
    val locusActivityParser: CypherRowParser[LocusActivityRow] = {
      str("friend.firstName") ~
      str("friend.facebookUserName") ~ 
      str("activityType") map {
        case firstName ~ facebookUserName ~ activityType =>
          LocusActivityRow(firstName, facebookUserName, activityType)
      }
    }

    Cypher("""
      START user=node:node_auto_index(facebookUserName={userName})
      MATCH user-[:FRIEND_OF]->friend-[activity]->locus
      WHERE locus.foursquareId={locusFoursquareId}
      RETURN friend.firstName, friend.facebookUserName, type(activity) as activityType
      ORDER BY activity.date DESC
      LIMIT 20
      """).on("userName" -> user.facebookUserName, "locusFoursquareId" -> foursquareId)
          .as(locusActivityParser *)
  }

  def getUserActivity(user: User, foursquareId: String) = {
    val activities = Cypher("""
      START user=node:node_auto_index(facebookUserName={userName})
      MATCH user-[activity]->locus
      WHERE locus.foursquareId={locusFoursquareId}
      RETURN type(activity) as atype
      """).on("userName" -> user.facebookUserName, "locusFoursquareId" -> foursquareId)
          .as(str("atype") *)

    ({
        if(activities.contains("LIKES")) Some(true)
        else if(activities.contains("DISLIKES")) Some(false)
        else None
    }, {
        if(activities.contains("WANTS")) Some(true)
        else if(activities.contains("DOESNTWANT")) Some(false)
        else None
    })
  }


}
