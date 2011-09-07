package klout {

import play._
import play.mvc._
import play.libs.WS
import play.libs.WS.HttpResponse
import collection.mutable.ListBuffer

/**
 * Lowest level class defining the load function
 *
 * @author Pascal Voitot [@_mandubian]
 */
abstract class KApi {
  protected[klout] def loadAndParse(patternUrl: String, format: Symbol = 'json, name: String)
                                   (parser: (HttpResponse) => KBase)
                                   (implicit KLOUT_API_KEY: String) = {
    val url = patternUrl.format(format.name, KLOUT_API_KEY, name)
    val resp = WS.url(url).get()
    resp.getStatus.intValue() match {
      case 200 => parser(resp)
      case _ => // TODO
    }
  }

  /**
   * the protected function that can't be called from outside
   */
  protected[klout] def load(implicit KLOUT_API_KEY:String):KBase

  /**
   * the unprotected function that can be called from outside to force reload all data
   */
  def reload(implicit KLOUT_API_KEY:String):KBase
}

/**
 * Base data from Klout (the name & the score)
 *
 * @author Pascal Voitot [@_mandubian]
 */
case class KBase(val name: String, var score: Float = 0.0F) extends KApi {
  override def load(implicit KLOUT_API_KEY: String) = this
  override def reload(implicit KLOUT_API_KEY: String) = this
}


/**
 * Dynamic mixin trick directly inspired from http://stackoverflow.com/questions/3893274/scala-and-traits-on-object-instances
 * It allows to add dynamic mixin (as if new MyClass with MyTrait) to an instance.
 * This class is locked in with Klout API (Kbase class) but could be generalized.
 *
 * To use it, you simply require a trait which can be mixed in a class inheriting KBase.
 * Then you create a companion object to your Trait by overriding right-associative operator ::
 *
 * object KScore extends DynamicMixinCompanion[KScore] {
 *   override def ::[T <: KBase](o: T)(implicit KLOUT_API_KEY: String): Mixin[T] = { val ret = new Mixin(o) with KScore; ret.load; ret }
 * }
 *
 * Then you can mixin instances of KBase such as
 *
 * var user = new KBase("bob")
 * user = user::KScore
 * user = user::KTopics
 * etc...

 *
 * @author Pascal Voitot [@_mandubian]
 */
class Mixin[MixedClass <: KBase](val obj: MixedClass) extends KBase(obj.name)

trait DynamicMixinCompanion[MixinTrait] {
  implicit def baseObject[MixedClass <: KBase](o: Mixin[MixedClass] with MixinTrait): MixedClass = o.obj

  def ::[MixedClass <: KBase](o: MixedClass)(implicit KLOUT_API_KEY: String):Mixin[MixedClass] with MixinTrait
}

/**
 * Klout Score API
 *
 * @author Pascal Voitot [@_mandubian]
 */
trait KScore extends KApi {
  self: KBase =>

  private val URL_SCORE = "http://api.klout.com/1/klout.%s?key=%s&users=%s"
  private var isLoaded = false

  private def parser = { resp: HttpResponse =>
    val json = resp.getJson
    val status = json.getAsJsonObject().get("status")

    // TODO ERROR MANAGEMENT

    val users = json.getAsJsonObject().get("users").getAsJsonArray
    val user = users.get(0).getAsJsonObject

    //name = user.get("twitter_screen_name").getAsString
    score = user.get("kscore").getAsFloat

    isLoaded = true
    this
  }

  abstract override def load(implicit KLOUT_API_KEY: String) = { if(!isLoaded) loadAndParse(patternUrl = URL_SCORE, name = self.name)(parser); super.load }

  abstract override def reload(implicit KLOUT_API_KEY: String) = { loadAndParse(patternUrl = URL_SCORE, name = self.name)(parser); super.load }

}

/**
 * Klout Score Companion object providing dynamic Mixin
 *
 * @author Pascal Voitot [@_mandubian]
 */
object KScore extends DynamicMixinCompanion[KScore] {
  override def ::[T <: KBase](o: T)(implicit KLOUT_API_KEY: String):Mixin[T] with KScore = { val ret = new Mixin(o) with KScore; ret.load; ret }
}


/**
 * Klout FullScore API (calls show GET)
 *
 * @author Pascal Voitot [@_mandubian]
 */
trait KFullScore extends KApi {
  self: KBase =>

  private val URL_SHOW ="http://api.klout.com/1/users/show.%s?key=%s&users=%s"
  private var isLoaded = false

  var twitter_id: String = ""

  case class Score (kscore:Float, slope: Float, description: String,
                  kclass_id:Int, kclass: String, kclass_description: String,
                  kscore_description: String, network_score: Float, amplification_score: Float,
                  true_reach: Int, delta_1day: Float, delta_5day: Float)

  var fullscore: Score = _

  /*Sample
    {
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

  private def parser =  { resp: HttpResponse =>
    val json = resp.getJson
    val status = json.getAsJsonObject().get("status")
    val users = json.getAsJsonObject().get("users").getAsJsonArray
    val user = users.get(0).getAsJsonObject

    twitter_id = user.get("twitter_id").getAsString
    val twitter_screen_name = user.get("twitter_screen_name").getAsString
    val sc = user.get("score").getAsJsonObject

    fullscore = Score(
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

    isLoaded = true

    this
  }

  abstract override def load(implicit KLOUT_API_KEY: String) = { if(!isLoaded) loadAndParse(patternUrl = URL_SHOW, name = self.name)(parser); super.load }

  abstract override def reload(implicit KLOUT_API_KEY: String) = { loadAndParse(patternUrl = URL_SHOW, name = self.name)(parser); super.load }

}

/**
 * Klout FullScore Companion object providing dynamic Mixin
 *
 * @author Pascal Voitot [@_mandubian]
 */
object KFullScore extends DynamicMixinCompanion[KFullScore] {
  override def ::[T <: KBase](o: T)(implicit KLOUT_API_KEY: String):Mixin[T] with KFullScore = { val ret = new Mixin(o) with KFullScore; ret.load; ret }
}

/**
 * Klout Topics API
 *
 * @author Pascal Voitot [@_mandubian]
 */
trait KTopics extends KApi {
  self: KBase =>

  private val URL_TOPICS ="http://api.klout.com/1/users/topics.%s?key=%s&users=%s"
  private var isLoaded = false

  var topics: ListBuffer[String] = new ListBuffer[String]

  /* Sample
    {
    "status": 200,
    "users": [
        {
            "twitter_screen_name": "mandubian",
            "topics": [
                "stack overflow",
                "java",
                "scala",
                "apps",
                "nosql"
            ]
        }
    ]
  }*/

  private def parser = {  resp: HttpResponse =>
    val json = resp.getJson
    val status = json.getAsJsonObject().get("status")
    val users = json.getAsJsonObject().get("users").getAsJsonArray
    val user = users.get(0).getAsJsonObject

    val twitter_screen_name = user.get("twitter_screen_name").getAsString
    val ts = user.get("topics").getAsJsonArray
    for(val i <- 1 until ts.size){
      topics += ts.get(i).getAsString
    }
    isLoaded = true

    this
  }

  abstract override def load(implicit KLOUT_API_KEY: String) = { if(!isLoaded) loadAndParse(patternUrl = URL_TOPICS, name = self.name)(parser); super.load }

  abstract override def reload(implicit KLOUT_API_KEY: String) = { loadAndParse(patternUrl = URL_TOPICS, name = self.name)(parser); super.load }


}

/**
 * Klout Topics Companion object providing dynamic Mixin
 *
 * @author Pascal Voitot [@_mandubian]
 */
object KTopics extends DynamicMixinCompanion[KTopics] {
  override def ::[T <: KBase](o: T)(implicit KLOUT_API_KEY: String):Mixin[T] with KTopics = { val ret = new Mixin(o) with KTopics; ret.load; ret }
}

/**
 * Klout Influencers API (calls influenced_by)
 *
 * @author Pascal Voitot [@_mandubian]
 */
trait KInfluencers extends KApi {
  self: KBase =>

  private val URL_INFLUENCERS ="http://api.klout.com/1/soi/influenced_by.%s?key=%s&users=%s"
  private var isLoaded = false

  var influencers = new ListBuffer[KBase]

  /* Samples
  {

    "status": 200,
    "users": [
        {
            "twitter_screen_name": "mandubian",
            "influencers": [
                {
                    "twitter_screen_name": "_felipera",
                    "kscore": 60.38
                },
                {
                    "twitter_screen_name": "playframework",
                    "kscore": 52.62
                },
                {
                    "twitter_screen_name": "klout",
                    "kscore": 84.04
                },
                {
                    "twitter_screen_name": "nicolasleroux",
                    "kscore": 41.29
                },
                {
                    "twitter_screen_name": "vijaykiran",
                    "kscore": 37.01
                }
            ]
        }
    ]
  }*/
  private def parser = { resp: HttpResponse =>
    val json = resp.getJson
    val status = json.getAsJsonObject().get("status")
    val users = json.getAsJsonObject().get("users").getAsJsonArray
    val user = users.get(0).getAsJsonObject

    val twitter_screen_name = user.get("twitter_screen_name").getAsString
    val infs = user.get("influencers").getAsJsonArray
    for(val i <- 1 until infs.size){
      val inf = infs.get(i).getAsJsonObject
      influencers += KBase(inf.get("twitter_screen_name").getAsString, inf.get("kscore").getAsFloat)
    }
    isLoaded = true

    this
  }

  abstract override def load(implicit KLOUT_API_KEY: String) = { if(!isLoaded) loadAndParse(patternUrl = URL_INFLUENCERS, name = self.name)(parser); super.load }

  abstract override def reload(implicit KLOUT_API_KEY: String) = { loadAndParse(patternUrl = URL_INFLUENCERS, name = self.name)(parser); super.load }

}

/**
 * Klout Influencers Companion object providing dynamic Mixin
 *
 * @author Pascal Voitot [@_mandubian]
 */
object KInfluencers extends DynamicMixinCompanion[KInfluencers] {
  override def ::[T <: KBase](o: T)(implicit KLOUT_API_KEY: String):Mixin[T] with KInfluencers = { val ret = new Mixin(o) with KInfluencers; ret.load; ret }
}

/**
 * Klout Influencees API (calls influencer_of)
 *
 * @author Pascal Voitot [@_mandubian]
 */
trait KInfluencees extends KApi {
  self: KBase =>

  private val URL_INFLUENCEES = "http://api.klout.com/1/soi/influencer_of.%s?key=%s&users=%s"
  private var isLoaded = false

  var influencees = new ListBuffer[KBase]

  /* Samples
  {
    "status": 200,
    "users": [
        {
            "twitter_screen_name": "mandubian",
            "influencees": [
                {
                    "twitter_screen_name": "_felipera",
                    "kscore": 60.38
                },
                {
                    "twitter_screen_name": "vijaykiran",
                    "kscore": 37.01
                },
                {
                    "twitter_screen_name": "ziglionz",
                    "kscore": 44.54
                },
                {
                    "twitter_screen_name": "nicolasleroux",
                    "kscore": 41.29
                },
                {
                    "twitter_screen_name": "ngirardin",
                    "kscore": 40.24
                }
            ]
        }
    ]
  }*/
  private def parser = { resp: HttpResponse =>
    val json = resp.getJson
    val status = json.getAsJsonObject().get("status")
    val users = json.getAsJsonObject().get("users").getAsJsonArray
    val user = users.get(0).getAsJsonObject

    val twitter_screen_name = user.get("twitter_screen_name").getAsString
    val infs = user.get("influencees").getAsJsonArray
    for(val i <- 1 until infs.size){
      val inf = infs.get(i).getAsJsonObject
      influencees += KBase(inf.get("twitter_screen_name").getAsString, inf.get("kscore").getAsFloat)
    }
    isLoaded = true

    this
  }

  abstract override def load(implicit KLOUT_API_KEY: String) = { if(!isLoaded) loadAndParse(patternUrl = URL_INFLUENCEES, name = self.name)(parser); super.load }

  abstract override def reload(implicit KLOUT_API_KEY: String) = { loadAndParse(patternUrl = URL_INFLUENCEES, name = self.name)(parser); super.load }

}

/**
 * Klout Influencees Companion object providing dynamic Mixin
 *
 * @author Pascal Voitot [@_mandubian]
 */
object KInfluencees extends DynamicMixinCompanion[KInfluencees] {
  override def ::[T <: KBase](o: T)(implicit KLOUT_API_KEY: String):Mixin[T] with KInfluencees = { val ret = new Mixin(o) with KInfluencees; ret.load; ret }
}

/**
 *
 *The default Klout user with name (twitter_name) & a score
 *
 */
case class KUser(override val name: String)(implicit KLOUT_API_KEY: String) extends KBase(name) with KScore {
  load
}

/**
 *
 *The full Klout user with name (twitter_name) & full score topics + influencers + influencees
 *
 */
case class KFullUser(override val name: String)(implicit KLOUT_API_KEY: String) extends KBase(name) with KFullScore with KTopics with KInfluencers with KInfluencees {
  load
}

}