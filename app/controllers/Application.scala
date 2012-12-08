package controllers

import play.api.mvc._
import play.api.Play._

/**
 * Main Application front controller.
 */
object Application extends Controller {

  def index = Action {
    Ok("Success!")
  }

}
