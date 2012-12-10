package controllers

import models._
import play.api.mvc._
import play.api.Play._

import play.api.libs.ws.WS
import play.api.Play.current

import play.api.libs.json._

/**
 * Main Application front controller.
 */
object Application extends Controller with Secured {

  def index = Action { implicit request =>
    Ok(views.html.index())
  }

  def feed = withUser { user => implicit request =>
    val activityList = User.getFriendsActivity(user)
    Ok(views.html.feed(user, activityList))
  }

  lazy val foursquareAppId = current.configuration.getString("foursquare.app.id").getOrElse {
    throw current.configuration.reportError("foursquare.app.id", "Application not properly configured, foursquare.app.id is not defined in application.conf")
  } 

  lazy val foursquareAppSecret = current.configuration.getString("foursquare.app.secret").getOrElse {
    throw current.configuration.reportError("foursquare.app.secret", "Application not properly configured, foursquare.app.secret is not defined in application.conf")
  }

  def nearby = Action { implicit request =>
    val foursquareUrl = "https://api.foursquare.com/v2/venues/search?ll=45.508663,-73.567335&client_id=" +
    foursquareAppId +
    "&client_secret=" +
    foursquareAppSecret +
    "&v=20121210"

    val ws = WS.url(foursquareUrl).get()
      play.Logger.info(foursquareUrl)
    Async {
      ws.map { result =>
        result.status match {
          case 200 => {
            val nearbyPlaces = (Json.parse(result.body) \ "response" \ "venues").as[List[JsValue]].map { v =>
                Locus.fromJson(v)
            }.toList
            Ok(views.html.nearby(nearbyPlaces))
          }
          case 400 => {
            NotFound
          }
          case _ => {
            NotFound
          }
        }
      }
    } 
  }

  def locus(foursquareId: String) = withUser { user => implicit request =>
    val foursquareUrl = "https://api.foursquare.com/v2/venues/" +
      foursquareId +
      "?client_id=" +
      foursquareAppId +
      "&client_secret=" +
      foursquareAppSecret +
      "&v=20121210"

    val ws = WS.url(foursquareUrl).get()
    Async {
      ws.map { result =>
        result.status match {
          case 200 => {
            val json = Json.parse(result.body) \ "response" \ "venue"
            Ok(views.html.locus(Locus.fromJson(json)))
          }
          case 400 => {
            NotFound
          }
          case _ => {
            NotFound
          }
        }
      }
    }
  }
}

trait Secured {

  /* Retrieve the connected user's username */
  private def userName(request: RequestHeader) = request.session.get(Security.username)

  /* Redirect to login if the user in not authorized */
  private def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Application.index)

  /* Get the connected user */
  def getUser(request: RequestHeader): Option[User] = userName(request).map(User.findByUserName(_)).getOrElse(None)

  def withAuth(f: => String => Request[AnyContent] => Result) = {
    Security.Authenticated(userName, onUnauthorized) { user =>
      Action(request => f(user)(request))
    }
  }

  /**
   * This method shows how you could wrap the withAuth method to also fetch your user
   * You will need to implement UserDAO.findOneByUsername
   */
  def withUser(f: User => Request[AnyContent] => Result) = withAuth { userName => implicit request =>
    User.findByUserName(userName).map { user =>
      f(user)(request)
    }.getOrElse(onUnauthorized(request))
  }

}
