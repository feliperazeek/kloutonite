package klout {

import play._
import play.mvc._
import play.libs.WS
import play.libs.WS.HttpResponse
import collection.mutable.ListBuffer
import com.google.gson.JsonObject
import scala.collection.mutable.{Map, HashMap}




/**
 * Lowest level class defining the load function
 *
 * @author Pascal Voitot [@_mandubian]
 */
abstract class KApi {
  protected[klout] def loadAndParse[T <: KSingle](patternUrl: String, format: Symbol = 'json, user: T)
                                   (parser: (KSingle, JsonObject) => Unit)
                                   (implicit KLOUT_API_KEY: String) = {
    val url = patternUrl.format(format.name, KLOUT_API_KEY, user.name)
    val resp = WS.url(url).get()
    resp.getStatus.intValue() match {
      case 200 =>
        val json = resp.getJson
        val status = json.getAsJsonObject().get("status")
        // TODO Status ERROR MANAGEMENT

        val users = json.getAsJsonObject().get("users").getAsJsonArray
        parser(user, users.get(0).getAsJsonObject)
      case _ => // TODO
    }
  }

  protected[klout] def loadAndParseMulti[T <: KSingle](patternUrl:String, format:Symbol = 'json, userMap:Map[String, T])
                                   (parser: (KSingle, JsonObject) => Unit)
                                   (implicit KLOUT_API_KEY: String) = {
    val url = patternUrl.format(format.name, KLOUT_API_KEY, userMap.keys.mkString(","))
    val resp = WS.url(url).get()

    resp.getStatus.intValue() match {
      case 200 =>
        val json = resp.getJson
        val status = json.getAsJsonObject().get("status")
        // TODO Status ERROR MANAGEMENT

        val users = json.getAsJsonObject().get("users").getAsJsonArray
        for(i <- 0 until users.size){
          val user = users.get(i).getAsJsonObject
          val name = user.get("twitter_screen_name").getAsString
          userMap.get(name) match {
            case Some(kuser:KSingle) => parser(kuser, user)
            case _ =>
          }
        }
      case _ =>  // TODO
    }
  }

  /**
   * the protected function that can't be called from outside
   */
  protected[klout] def load(name:String)(implicit KLOUT_API_KEY:String) = {}
  protected[klout] def loadMulti[T <: KSingle](userMap:Map[String, T])(implicit KLOUT_API_KEY:String) = {}

  /**
   * the unprotected function that can be called from outside to force reload all data
   */
  protected[klout] def reload(name:String)(implicit KLOUT_API_KEY:String) = {}
}

/**
 * Base data from Klout (the name & the score)
 *
 * @author Pascal Voitot [@_mandubian]
 */
case class KSingle(val name: String, var score: Float = 0.0F) extends KApi {
  //override def load(implicit KLOUT_API_KEY:String) = {}
  //override def reload(implicit KLOUT_API_KEY:String) = {}
  //override def loadMulti(userMap:Map[String,KSingle])(implicit KLOUT_API_KEY:String) = {}
}


/*class KMulti(val names:Seq[String]) extends KApi {
  override def load(userMap:Map[String, KSingle])(implicit KLOUT_API_KEY:String):Map[String, KSingle] = new HashMap[String, KSingle]
  override def reload(names:Seq[String])(implicit KLOUT_API_KEY:String):Map[String, KSingle] = new HashMap[String, KSingle]
} */

/**
 * Klout Score API
 *
 * @author Pascal Voitot [@_mandubian]
 */
trait KScore extends KApi {
  self:KSingle =>

  val URL_SCORE = "http://api.klout.com/1/klout.%s?key=%s&users=%s"
  var isLoaded = false

  private def parser = { (user:KSingle with KScore, jsonUser: JsonObject) =>
    user.score = jsonUser.get("kscore").getAsFloat
  }

  abstract override def load(name:String)(implicit KLOUT_API_KEY:String) = {
    if(!isLoaded) {
      loadAndParse(patternUrl = URL_SCORE, user=this)(parser)
      isLoaded = true
    }
    super.load(name)
  }

  abstract override def reload(name:String)(implicit KLOUT_API_KEY:String) = {
    loadAndParse(patternUrl = URL_SCORE, user=this)(parser)
    super.load(name)
  }

  abstract override def loadMulti[T <: KSingle](userMap:Map[String, T])(implicit KLOUT_API_KEY:String) = {
    if(!isLoaded) {
      loadAndParseMulti(patternUrl = URL_SCORE, userMap=userMap)(parser)
      isLoaded = true
    }
    super.loadMulti(userMap)
  }
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
class Mixin[MixedClass <: KSingle](val obj: MixedClass) extends KSingle(obj.name)

trait DynamicMixinCompanion[MixinTrait] {
  implicit def baseObject[MixedClass <: KSingle](o: Mixin[MixedClass] with MixinTrait): MixedClass = o.obj

  def ::[MixedClass <: KSingle](o: MixedClass)(implicit KLOUT_API_KEY: String):Mixin[MixedClass] with MixinTrait
  //def ::[MixedClass <: KDefault](o: List[MixedClass])(implicit KLOUT_API_KEY: String):List[Mixin[MixedClass] with MixinTrait]
}



/**
 * Klout Score Companion object providing dynamic Mixin
 *
 * @author Pascal Voitot [@_mandubian]
 */
object KScore extends DynamicMixinCompanion[KScore] {
  override def ::[T <: KSingle](o: T)(implicit KLOUT_API_KEY: String):Mixin[T] with KScore = { val ret = new Mixin(o) with KScore; ret.load(o.name); ret }
  /*override def ::[T <: KDefault](o: List[T])(implicit KLOUT_API_KEY: String):List[Mixin[T] with KScore] = {
    o.map( x => new Mixin(x) with KScore)
    //val ret = new Mixin(o) with KScore; ret.mixload; ret
  }*/
}


/**
 * Klout FullScore API (calls show GET)
 *
 * @author Pascal Voitot [@_mandubian]
 */
trait KFullScore extends KApi {
  self: KSingle =>

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


  private def parser = { (user:KSingle with KScore, jsonUser: JsonObject) =>
    user.score = jsonUser.get("kscore").getAsFloat
  }

  abstract override def load(name:String)(implicit KLOUT_API_KEY:String) = {
    if(!isLoaded) {
      loadAndParse(patternUrl = URL_SCORE, user=this)(parser)
      isLoaded = true
    }
    super.load(name)
  }

  abstract override def reload(name:String)(implicit KLOUT_API_KEY:String) = {
    loadAndParse(patternUrl = URL_SCORE, user=this)(parser)
    super.load(name)
  }

  abstract override def loadMulti[T <: KSingle](userMap:Map[String, T])(implicit KLOUT_API_KEY:String) = {
    if(!isLoaded) {
      loadAndParseMulti(patternUrl = URL_SCORE, userMap=userMap)(parser)
      isLoaded = true
    }
    super.loadMulti(userMap)
  }
}
/*
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
*/

/**
 *
 *The default Klout user with name (twitter_name) & a score
 *
 */
case class KUser(override val name:String)(implicit KLOUT_API_KEY: String) extends KSingle(name) with KScore {
  load(name)
}

/**
 *
 *The full Klout user with name (twitter_name) & full score topics + influencers + influencees
 *
 */
/*
case class KFullUser(override val name: String)(implicit KLOUT_API_KEY: String) extends KBase(name) with KFullScore with KTopics with KInfluencers with KInfluencees {
  load
}
*/

object KUsers extends KApi {

  def apply(names: String*)(implicit KLOUT_API_KEY: String): Map[String, KUser] = {
    val map = HashMap.empty[String, KUser] ++ names.map(name => (name, new KUser(name)))
    loadMulti(map)
    map
  }
}

}