import play._
import play.mvc._
import play.libs.WS
import play.libs.WS.HttpResponse

package klout {

/**
 * KApi
 *
 * @author Pascal Voitot [@_mandubian]
 */
abstract class KApi {
  protected[klout] def loadAndParse(patternUrl: String, format: Symbol = 'json, name: String)
                                   (parser: (HttpResponse) => Unit)
                                   (implicit KLOUT_API_KEY: String) = {
    val url = patternUrl.format(format.name, KLOUT_API_KEY, name)
    val resp = WS.url(url).get()

    parser(resp)
  }

  def load(implicit KLOUT_API_KEY:String):KBase
}

abstract case class KBase(val name: String) extends KApi {
  override def load(implicit KLOUT_API_KEY: String) = this
}

// dynamic mixin trick from http://stackoverflow.com/questions/3893274/scala-and-traits-on-object-instances
trait DynamicMixinCompanion[MixinTrait] {
  implicit def baseObject[MixedClass](o: Mixin): KBase = o.obj

  def ::(o: KBase)(implicit KLOUT_API_KEY: String): Mixin with MixinTrait

  class Mixin(val obj: KBase) extends KBase(obj.name)

}

trait KScore extends KApi {
  self: KBase =>

  private val URL_SCORE = "http://api.klout.com/1/klout.%s?key=%s&users=%s"

  var kscore: Float = 0.0F
  var twitter_screen_name: String = ""

  abstract override def load(implicit KLOUT_API_KEY: String) = {
    loadAndParse(patternUrl = URL_SCORE, name = name) { resp: HttpResponse =>
      val json = resp.getJson
      val status = json.getAsJsonObject().get("status")
      val users = json.getAsJsonObject().get("users").getAsJsonArray
      val user = users.get(0).getAsJsonObject

      twitter_screen_name = user.get("twitter_screen_name").getAsString
      kscore = user.get("kscore").getAsFloat
    }

    super.load
  }
}

object KScore extends DynamicMixinCompanion[KScore] {
  def ::(o: KBase)(implicit KLOUT_API_KEY: String) = new Mixin(o) with KScore
}


case class Score (kscore:Float, slope: Float, description: String,
                  kclass_id:Int, kclass: String, kclass_description: String,
                  kscore_description: String, network_score: Float, amplification_score: Float,
                  true_reach: Int, delta_1day: Float, delta_5day: Float)

trait KShow extends KApi {
  self: KBase =>

  private val URL_SCORE ="http://api.klout.com/1/users/show.%s?key=%s&users=%s"

  var twitter_id: String = ""
  var twitter_screen_name: String = ""
  var score: Score = _

  /*{
    "status": 200,
    "users": [
        {
            "twitter_id": "200653266",
            "twitter_screen_name": "mandubian",
            "score": {
                "kscore": 49.49,
                "slope": 0.04,
                "description": "is effectively using social media to influence their network across a variety of topics",
                "kclass_id": 11,
                "kclass": "Specialist",
                "kclass_description": "You may not be a celebrity, but within your area of expertise your opinion is second to none. Your content is likely focused around a specific topic or industry with a focused, highly-engaged audience.",
                "kscore_description": "is effectively using social media to influence their network across a variety of topics",
                "network_score": 49.64,
                "amplification_score": 25.94,
                "true_reach": 14,
                "delta_1day": -0.03,
                "delta_5day": -0.43
            }
        }
    ]
  }*/
  abstract override def load(implicit KLOUT_API_KEY: String) = {
    loadAndParse(patternUrl = URL_SCORE, name = name) { resp: HttpResponse =>
      val json = resp.getJson
      val status = json.getAsJsonObject().get("status")
      val users = json.getAsJsonObject().get("users").getAsJsonArray
      val user = users.get(0).getAsJsonObject

      twitter_id = user.get("twitter_id").getAsString
      twitter_screen_name = user.get("twitter_screen_name").getAsString
      val sc = user.get("score").getAsJsonObject

      score = Score(
          sc.get("kscore").getAsFloat,
          sc.get("slope").getAsFloat,
          sc.get("description").getAsString,
          sc.get("kclass_id").getAsInt,
          sc.get("kclass").getAsString,
          sc.get("kclass_description").getAsString,
          sc.get("kscore_description").getAsString,
          sc.get("network_score").getAsFloat,
          sc.get("amplification_score").getAsFloat,
          sc.get("true_reach").getAsInt,
          sc.get("delta_1day").getAsFloat,
          sc.get("delta_5day").getAsFloat
      )
    }

    super.load
  }
}


object KShow extends DynamicMixinCompanion[KShow] {
  def ::(o: KBase)(implicit KLOUT_API_KEY: String) = { val ret = new Mixin(o) with KShow; ret.load; ret }
}

case class KUser(override val name: String)(implicit KLOUT_API_KEY: String) extends KBase(name) with KScore {
  load
}

case class KFullUser(override val name: String)(implicit KLOUT_API_KEY: String) extends KBase(name) with KShow {
  load
}

}