package models

import play.api.libs.json._

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

}
