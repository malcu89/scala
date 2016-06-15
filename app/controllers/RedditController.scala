package controllers

import javax.inject._

import play.api.mvc._

import play.api.data._
import play.api.data.Forms._

import play.api.Play.current
import play.api.i18n.Messages.Implicits._

import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits._
/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class RedditController @Inject()(ws: WSClient) extends Controller {

  val userForm = Form(
    mapping(
      "tag" -> text
    )(UserData.apply)(UserData.unapply)
  )

  def index = Action { implicit request =>
    if(request.session.isEmpty)
      {
        Redirect("/login");
      }
    else
      {
        Ok(views.html.index(userForm.fill(UserData("politics"))))
      }
  }

  def userPost = Action.async(parse.form(userForm)) { implicit request =>
    ws.url("https://www.reddit.com/r/" + request.body.tag + "/top.json").get().map(response => Ok(views.html.getReddit(response.body)))
  }
}

case class UserData(tag: String)