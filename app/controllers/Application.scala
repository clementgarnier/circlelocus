package controllers

import models.User
import play.api.mvc._
import play.api.Play._

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
