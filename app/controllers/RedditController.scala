package controllers

import javax.inject._

import play.api.Logger
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.twirl.api.Html
/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class RedditController @Inject()(ws: WSClient) extends Controller {

  val userForm = Form(
    mapping(
      "subreddit" -> text
    )(SelectSubredditData.apply)(SelectSubredditData.unapply)
  )

  val redditsForm: Form[RedditsFormData] = Form(
    mapping(
      "redditsList" -> seq(
        mapping(
          "id" -> number,
          "checked" -> boolean
        )
        (RedditFormData.apply)
        (RedditFormData.unapply)
      )
    )
    (RedditsFormData.apply)
    (RedditsFormData.unapply)
  )

  def index = Action { implicit request =>
    if(request.session.isEmpty)
      {
        Redirect("/login");
      }
    else
      {
        Ok(views.html.index(userForm.fill(SelectSubredditData("politics"))))
      }
  }

  var jsonBody : String = _
  var redditsJson : List[RedditJsonData] = _
  val redditsDataReadsBuilder = (
    (__ \ "data" \ "ups").read[Int] and
      (__ \ "data" \ "thumbnail").read[String] and
      (__ \ "data" \ "url").read[String] and
      (__ \ "data" \ "title").read[String]
    )(RedditJsonData.apply _)

  def postSubreddit = Action.async(parse.form(userForm)) { implicit request =>
    ws.url("https://www.reddit.com/r/" + request.body.subreddit + "/top.json").get().map(response => {
      jsonBody = response.body
      Redirect(routes.RedditController.reddits())})
  }

  def reddits = Action { implicit  req => {
    val jsonVal: JsValue = Json.parse(jsonBody)
    val redditsEntries: List[JsValue] = (jsonVal \ "data" \ "children").as[List[JsValue]]

    redditsJson = redditsEntries.map(jsval => jsval.as(redditsDataReadsBuilder))

    Ok(views.html.getReddit(redditsForm.fill(
      RedditsFormData(List.fill(redditsEntries.length)(RedditFormData(0, false)))), redditsJson))
    }
  }

  def pickedRedditsPost = Action { implicit  request =>

    redditsForm.bindFromRequest.fold(
      formWithError => {
        formWithError.errors.foreach(er => Logger.debug(er.message))
        Ok(views.html.main("asd")(Html("error")))
      },
      goodOne => {
        val pickedToTwitter: List[RedditJsonData] = redditsJson.filter(el => goodOne.redditsList(redditsJson.indexOf(el)).checked)

        Ok(views.html.main("asd")(Html(pickedToTwitter.mkString)))
      }
    )
  }
}

case class SelectSubredditData(subreddit: String)
case class RedditsFormData(redditsList: Seq[RedditFormData])
case class RedditFormData(id: Int, checked: Boolean)
case class RedditJsonData(ups: Int, thumbnail: String, url: String, title: String)