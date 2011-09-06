package oauth

import org.scribe.oauth.OAuthService
import org.scribe.builder.ServiceBuilder
import org.scribe.builder.api.TwitterApi
import org.scribe.model.Token

/**
 * Social Profile
 *
 * @author Felipe Oliveira [@_felipera]
 */
case class SocialProfile(id: Option[String] = None, token: Option[String] = None, secret: Option[String] = None)


/**
 * OAuth Provider
 *
 * @author Felipe Oliveira [@_felipera]
 */
trait OAuthProvider {

    /**
     * OAuth Provider Name
     */
    def name: String

    /**
     * Scribe OAuth Service
     */
    def service(callback: String): OAuthService

    /**
     * Scribe Service With No Callback
     */
    def serviceNoCallback: OAuthService

    /**
     * Social Profile
     */
    def profile(service: OAuthService, token: Token) = new SocialProfile(token = Some(token.getToken), secret = Some(token.getSecret))

}

/**
 * Twitter OAuth Provider
 *
 * @author Felipe Oliveira [@_felipera]
 */
class Twitter extends oauth.OAuthProvider {

    /**
     * Name
     */
    def name: String = "twitter"

    /**
     * Scribe Service
     */
    def service(callback: String): OAuthService = new ServiceBuilder().provider(classOf[TwitterApi]).apiKey(play.Play.configuration.getProperty("twitter.apiKey")).apiSecret(play.Play.configuration.getProperty("twitter.secret")).callback(callback).build()

    /**
     * Scribe Service With No Callback
     */
    def serviceNoCallback: OAuthService = new ServiceBuilder().provider(classOf[TwitterApi]).apiKey(play.Play.configuration.getProperty("twitter.apiKey")).apiSecret(play.Play.configuration.getProperty("twitter.secret")).build()

}

/**
 * OAuth
 *
 * @author Felipe Oliveira [@_felipera]
 */
object OAuth {

    /**
     * Get Provider
     */
    def getProvider(provider: String): Option[oauth.OAuthProvider] = {
        provider match {
            case "twitter" => Some[oauth.OAuthProvider](new oauth.Twitter)
            case _ => None
        }
    }

}


/**
 * OAuth Callback which should be used on a Play Framework controller
 *
 * @author Felipe Oliveira [@_felipera]
 */
trait OAuthCallback {

    def callback(provider: oauth.OAuthProvider, socialProfile: SocialProfile)

}