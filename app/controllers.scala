package controllers

import _root_.klout.{KShow, KScore, KUser}
import play._
import play.cache.Cache
import play.mvc._
import org.scribe.oauth.{OAuth20ServiceImpl, OAuthService}
import org.apache.commons.lang.StringUtils
import org.scribe.model.Token
import org.scribe.model.Verifier
import org.scribe.oauth.OAuth20ServiceImpl
import org.scribe.oauth.OAuthService
import oauth.{OAuthCallback, OAuthProvider}

/**
 * Application Controller
 *
 * @author Felipe Oliveira [@_felipera]
 */
object Application extends Controller {

    import views.Application._

    def index = {
        html.index("Your Scala application is ready!")
    }

}

/**
 * OAuth Controller
 *
 * @author Felipe Oliveira [@_felipera]
 */
object OAuth extends Controller {

    /**
     * Gets the login url.
     *
     * @return the login url
     */
    def getLoginUrl(provider: oauth.OAuthProvider) = {
        // Get Service
        val service = provider.service(Router.getFullUrl("OAuth.callback") + "?provider=" + provider.name)

        // Check if this is an oauth 2 dance just redirect since it's an
        // one-legged dance
        if (service.isInstanceOf[OAuth20ServiceImpl]) {
            val authUrl = service.getAuthorizationUrl(null)
            Logger.info("Authorization Url: %s", authUrl)
            authUrl
        } else {
            // Get Token
            val token = service.getRequestToken()

            // Log Debug
            Logger.info("Request Token - " + token.getToken() + " with secret " + token.getSecret())

            // Cache Token
            Cache.add(token.getToken(), token.getSecret())

            // Return Auth Url
            service.getAuthorizationUrl(token)
        }
    }

    /**
     * Login.
     */
    def login(provider: String) = {
        oauth.OAuth.getProvider(provider) match {
            case Some(p) => Redirect(getLoginUrl(p))
            case _ => Forbidden("Invalid OAuth Provider: " + provider)
        }
    }

    /**
     * Callback.
     */
    def callback(provider: String) = {
        // Get Provider
        oauth.OAuth.getProvider(provider) match {

            case Some(p) => {

                // Get Service
                var service = p.serviceNoCallback
                if (service.isInstanceOf[OAuth20ServiceImpl]) {
                    service = p.service(Router.getFullUrl("socialprofile.OAuth.callback") + "?provider=" + provider)
                }

                // Get Query String Token and Verifier
                Logger.info("OAuth Callback - Params: " + params)
                val oauthToken = params.get("oauth_token")
                var oauthVerifier = params.get("oauth_verifier")
                if (oauthVerifier == null) {
                    oauthVerifier = params.get("code")
                }

                val verifier = new Verifier(oauthVerifier)
                Logger.info("Token: " + oauthToken)
                Logger.info("Verifier: " + oauthVerifier)

                // Request Access Token
                var token: Token = null
                if (!(service.isInstanceOf[OAuth20ServiceImpl])) {
                    val secret: Option[Token] = Cache.get(oauthToken)
                    secret match {
                        case Some(s) => token = new Token(oauthToken, s.getSecret)
                    }
                }

                val accessToken = service.getAccessToken(token, verifier)

                // Log Debug
                Logger.info("Access Token: " + accessToken)

                // Get Profile Details
                val socialProfile = p.profile(service, accessToken)

                // Log Debug
                Logger.info("Social Profile: %s", socialProfile)

                // Do Callback to Calling App
                invoke(p, socialProfile)
            }
            case _ => Forbidden("Invalid OAuth Provider: " + provider)
        }
    }

    /**
     * Invoke.
     *
     * @param m
     *            the m
     * @param args
     *            the args
     * @return the object
     * @throws Throwable
     *             the throwable
     */
    def invoke(provider: oauth.OAuthProvider, socialProfile: oauth.SocialProfile) = {
        val classes = Play.classloader.getAssignableClasses(classOf[OAuthCallback])
        if (classes.isEmpty()) {
            throw new IllegalStateException("Please define a class that implements OAuthCallback")
        }

        val callback = classes.get(0).newInstance.asInstanceOf[ {def callback(provider: oauth.OAuthProvider, socialProfile: oauth.SocialProfile)}]
        Action(callback.callback(provider, socialProfile))
    }

}


object Klout extends Controller {
    implicit val KLOUT_API_KEY: String = "bfs2g93vm5dn7qr72ju7baxq"

    /**
     * Gets the score of a user.
     *
     */
    def score(name: String) = {
      val user = KUser("mandubian")
      Json(""" {"username": "%s", "twitter_name": "%s", "score":"%s"}""".format(user.name, user.twitter_screen_name, user.kscore))
    }

    def show(name: String) = {
      val user = KUser("mandubian") :: KShow
      Json(""" {"username": "%s", "twitter_name": "%s", "score":"%s"}""".format(user.name, user.twitter_screen_name, user.score))
    }
}

