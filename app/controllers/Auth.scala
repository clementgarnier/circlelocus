package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import models._
import play.api.libs.Crypto

import play.api.libs.ws.WS
import com.restfb.DefaultFacebookClient
import java.net.URLEncoder

import play.api.Play.current

object Auth extends Controller with Secured {

  lazy val facebookAppId = current.configuration.getString("facebook.app.id").getOrElse {
    throw current.configuration.reportError("facebook.app.id", "Application not properly configured, facebook.app.id is not defined in application.conf")
  } 

  lazy val facebookAppSecret = current.configuration.getString("facebook.app.secret").getOrElse {
    throw current.configuration.reportError("facebook.app.secret", "Application not properly configured, facebook.app.secret is not defined in application.conf")
  }

  /* Extends the client-side User Access Token and logs the user in */
  def doFacebookAuth(code: String) = Action { implicit request =>
    val facebookUrl = "https://graph.facebook.com/oauth/access_token?grant_type=fb_exchange_token&" +
                      "client_id=" +
                      facebookAppId +
                      "&client_secret=" +
                      facebookAppSecret +
                      "&fb_exchange_token=" +
                      code
    
    val ws = WS.url(facebookUrl).get()
    Async {
      ws.map {
        result =>
          result.status match {
            case 200 => {
              extractAccessToken(result.body).map(createOrAuthenticateUser(_, request.session))
                .getOrElse(ServiceUnavailable("Unable to authenticate with Facebook 2"))                    
            }
            case 400 => {
              ServiceUnavailable("Invalid request")
            }
            case _ => {
              ServiceUnavailable("Unable to authenticate with Facebook")
            }
          }
      }
    }
  }
  
  /**
   * Using the Facebook access token, this method loads from Facebook Graph API the user's details, then either create
   * a new user or retrieve an existing one
   * @param  accessToken is the Facebook secure token
   * @return an Action
   */
  private def createOrAuthenticateUser(accessToken: String, session: play.api.mvc.Session) = {
    val facebookClient = new DefaultFacebookClient(accessToken)
    val user = facebookClient.fetchObject("me", classOf[com.restfb.types.User])
    val maybeUser = User.findByUserName(user.getUsername)
    maybeUser match {
      case Some(user) => {
        loginRedirect(user)
      }
      case None => {
        val newUser =
          User.create(user.getEmail,
            user.getFirstName,
            user.getLastName,
            user.getUsername,
            user.getId,
            accessToken
          )

        loginRedirect(newUser.get)
      }
    }
  }

  /* Sets the session and redirects the user */
  private def loginRedirect(user: User) = {
    Redirect(routes.Application.feed())
      .withSession(
        Security.username -> user.facebookUserName
      )
  }

  /**
   * Parse a string access_token=[access_token]&expires=5181202
   */
  private def extractAccessToken(body: String): Option[String] = {
    body.split("&").foreach {
      token: String =>
        val params = token.split("=")
        if (params(0) == "access_token") {
          return Some(params(1))
        }
    }
    None
  }

}

