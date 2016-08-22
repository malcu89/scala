package controllers

import javax.inject.{Inject, Singleton}
import play.api.cache
import play.api.cache._
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.oauth._
import play.api.mvc._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Json
import play.twirl.api.Html

import scala.concurrent.Future

case class TwitterLoginData(login: String, password: String)

@Singleton
class TwitterController @Inject()(ws: WSClient, @NamedCache("session-cache") sessionCache: CacheApi) extends Controller {
  val KEY = ConsumerKey("Y9tC3nXQB02sABews7WXo5ndk", "vmCC7kOIyWe6PxaFTdAqOrO57YdDrWrx5sEvTcF6OG2UwgSrLC")

  val TWITTER = OAuth(ServiceInfo(
    "https://api.twitter.com/oauth/request_token",
    "https://api.twitter.com/oauth/access_token",
    "https://api.twitter.com/oauth/authorize", KEY),
    true)

  def authenticate = Action { request =>

    request.queryString.get("oauth_verifier").flatMap(_.headOption).map { verifier =>
      val tokenPair = sessionTokenPair(request).get
      // We got the verifier; now get the access token, store it and back to index
      TWITTER.retrieveAccessToken(tokenPair, verifier) match {
        case Right(t) => {
          // We received the authorized tokens in the OAuth object - store it before we proceed
          Redirect(routes.RedditController.index).withSession("token" -> t.token, "secret" -> t.secret)
        }
        case Left(e) => throw e
      }
    }.getOrElse(
      TWITTER.retrieveRequestToken("http://localhost:9000/login") match {
        case Right(t) => {
          // We received the unauthorized tokens in the OAuth object - store it before we proceed
          Redirect(TWITTER.redirectUrl(t.token)).withSession("token" -> t.token, "secret" -> t.secret)
        }
        case Left(e) => throw e
      })
  }

  def sessionTokenPair(implicit request: RequestHeader): Option[RequestToken] = {
    for {
      token <- request.session.get("token")
      secret <- request.session.get("secret")
    } yield {
      RequestToken(token, secret)
    }
  }

  def send = Action.async { request =>

    var rt: RequestToken = new RequestToken(request.session.get("token").get, request.session.get("secret").get)

    sessionCache.get[List[RedditJsonData]]("pickedToTwitter") match {
      case Some(pttw) => {
        val posts = pttw.map(tweet => ws.url("https://api.twitter.com/1.1/statuses/update.json?status=" + tweet.url)
          .sign(OAuthCalculator(KEY, rt))
          .post("ignored"))

        Future.sequence(posts).map(response => Redirect(routes.RedditController.index))
      }
    }
  }
}
