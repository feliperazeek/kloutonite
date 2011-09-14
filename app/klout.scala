package klout {

import play._
import play.mvc._
import play.libs.WS
import play.libs.WS.HttpResponse
import collection.mutable.ListBuffer
import scala.collection.mutable.{Map, HashMap}
import com.google.gson.{JsonElement, JsonObject}


/**
 * Lowest level class defining the load function
 *
 * @author Pascal Voitot [@mandubian]
 */
trait KApi {
  protected[klout] def loadAndParse[T <: KDefaultUser](patternUrl: String, format: Symbol = 'json, name:String)
                                   (implicit KLOUT_API_KEY: String) = {
    val url = patternUrl.format(format.name, KLOUT_API_KEY, name)
    val resp = WS.url(url).get()
    resp.getStatus.intValue() match {
      case 200 =>
        val json = resp.getJson
        val status = json.getAsJsonObject().get("status")
        // TODO Status ERROR MANAGEMENT

        val users = json.getAsJsonObject().get("users").getAsJsonArray
        parse(users.get(0).getAsJsonObject)
      case _ => // TODO
    }
  }

  /**
   * the protected function that can't be called from outside
   */
  protected[klout] def load(implicit KLOUT_API_KEY:String)  = {}

  protected[klout] def parse(jsonUser:JsonObject) = {}

  /**
   * the unprotected function that can be called from outside to force reload all data
   */
  protected[klout] def reload(implicit KLOUT_API_KEY:String) = {}
}

trait KApiMulti {
  protected[klout] def loadAndParseMulti[T <: KDefaultUser](patternUrl:String, format:Symbol = 'json, userMap:Map[String, T])
                                   (implicit KLOUT_API_KEY: String):Map[String, T] = {
    val url = patternUrl.format(format.name, KLOUT_API_KEY, userMap.keys.mkString(","))
    val resp = WS.url(url).get()

    resp.getStatus.intValue() match {
      case 200 =>
        val json = resp.getJson
        val status = json.getAsJsonObject().get("status")
        // TODO Status ERROR MANAGEMENT

        val users = json.getAsJsonObject().get("users").getAsJsonArray
        for(i <- 0 until users.size){
          val jsonUser = users.get(i).getAsJsonObject
          val name = jsonUser.get("twitter_screen_name").getAsString
          userMap.get(name) match {
            case Some(kuser:T) => kuser.parse(jsonUser)
            case _ =>
          }
        }
        userMap
      case _ => Map.empty // TODO
    }
  }

  protected[klout] def loadMulti[T <: KDefaultUser](userMap:Map[String, T])(implicit KLOUT_API_KEY:String):Map[String, T] = { Map.empty }
}

/**
 * Base data from Klout (the name & the score)
 *
 * @author Pascal Voitot [@mandubian]
 */
case class KDefaultUser(name:String) extends KApi

/**
 * Klout Score API
 *
 * @author Pascal Voitot [@mandubian]
 */
trait KScore extends KApi {
  self:KDefaultUser =>

  private var isLoaded = false

  var score:Float = 0.0F

  abstract override def parse(jsonUser:JsonObject) = {
    Option(jsonUser.get("kscore")) match {
      case Some(obj:JsonElement) => score = obj.getAsFloat
      case _ =>
    }
  }

  abstract override def load(implicit KLOUT_API_KEY:String) = {
    if(!isLoaded) {
      loadAndParse(patternUrl = KScore.URL, name = name)
      isLoaded = true
    }
    super.load
  }

  abstract override def reload(implicit KLOUT_API_KEY:String) = {
    loadAndParse(patternUrl = KScore.URL, name = name)
    super.load
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
 * @author Pascal Voitot [@mandubian]
 */
class Mixin[T <: KDefaultUser](val obj: T) extends KDefaultUser(obj.name) {
  //override def load(implicit KLOUT_API_KEY:String) = obj.load
  //override def parse(jsonUser:JsonObject) = obj.parse(jsonUser)
}
//class MixinMulti[T <: KDefaultUser](val obj: KDefaultUserMulti[T]) extends KDefaultUserMulti[T]

trait DynamicMixinCompanion[MixinTrait] {
  implicit def baseObject[T <: KDefaultUser](o: Mixin[T] with MixinTrait):T = o.obj

  def ::[T <: KDefaultUser](o: T)(implicit KLOUT_API_KEY: String):Mixin[T] with MixinTrait

  def :::[T <: KDefaultUser](map:Map[String, T])(implicit KLOUT_API_KEY: String):Map[String, Mixin[T] with MixinTrait]
}


/**
 * Klout Score Companion object providing dynamic Mixin
 *
 * @author Pascal Voitot [@mandubian]
 */
object KScore extends DynamicMixinCompanion[KScore] with KApiMulti {
  override def ::[T <: KDefaultUser](o: T)(implicit KLOUT_API_KEY: String):Mixin[T] with KScore = {
    val ret = new Mixin(o) with KScore
    ret.load(o.name)
    ret
  }

  val URL = "http://api.klout.com/1/klout.%s?key=%s&users=%s"

  override def :::[T <: KDefaultUser](map:Map[String, T])(implicit KLOUT_API_KEY: String):Map[String, Mixin[T] with KScore] = {
    val map2 = map.map{ case (str, user) => (str , new Mixin(user) with KScore) }
    val (str, user) = map2.head

    loadAndParseMulti(patternUrl = URL, userMap=map2)
  }

  override def loadMulti[T <: KDefaultUser](userMap:Map[String, T])(implicit KLOUT_API_KEY:String):Map[String, T] = {
    loadAndParseMulti(patternUrl = URL, userMap = userMap)
  }
}


/**
 * Klout FullScore API (calls show GET)
 *
 * @author Pascal Voitot [@mandubian]
 */
trait KFullScore extends KApi {
  self: KDefaultUser =>

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
  abstract override def parse(jsonUser:JsonObject) = {
    Option(jsonUser.get("twitter_id")) match {
      case Some(obj:JsonElement) =>
        twitter_id = obj.getAsString
        Option(jsonUser.get("twitter_screen_name")) match {
          case Some(obj:JsonElement) =>
            val twitter_screen_name = obj.getAsString
            Option(jsonUser.get("score")) match {
              case Some(obj:JsonElement) =>
                val sc = obj.getAsJsonObject
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
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }
  }

  abstract override def load(implicit KLOUT_API_KEY:String) = {
    if(!isLoaded) {
      loadAndParse(patternUrl = KFullScore.URL, name = name)
      isLoaded = true
    }
    super.load(name)
  }

  override def reload(implicit KLOUT_API_KEY:String) = {
    loadAndParse(patternUrl = KFullScore.URL, name = name)
    super.load(name)
  }
}

/**
 * Klout FullScore Companion object providing dynamic Mixin
 *
 * @author Pascal Voitot [@mandubian]
 */
object KFullScore extends DynamicMixinCompanion[KFullScore] with KApiMulti {
  val URL ="http://api.klout.com/1/users/show.%s?key=%s&users=%s"

  override def ::[T <: KDefaultUser](o: T)(implicit KLOUT_API_KEY: String):Mixin[T] with KFullScore = {
    val ret = new Mixin(o) with KFullScore
    ret.load
    ret
  }

  override def :::[T <: KDefaultUser](map:Map[String, T])(implicit KLOUT_API_KEY: String):Map[String, Mixin[T] with KFullScore] = {
    loadMulti(map.map{ case (str, user) => (str , new Mixin(user) with KFullScore) })
  }

  override def loadMulti[T <: KDefaultUser](userMap:Map[String, T])(implicit KLOUT_API_KEY:String):Map[String, T] = {
    loadAndParseMulti(patternUrl = URL, userMap = userMap)
  }

}


/**
 * Klout Topics API
 *
 * @author Pascal Voitot [@mandubian]
 */
trait KTopics extends KApi {
  self: KDefaultUser =>

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
  abstract override def parse(jsonUser:JsonObject) = {
    Option(jsonUser.get("twitter_screen_name")) match {
      case Some(obj:JsonElement) =>
        val twitter_screen_name = obj.getAsString

        Option(jsonUser.get("topics")) match {
          case Some(obj:JsonElement) =>
            val ts = obj.getAsJsonArray
            for(val i <- 1 until ts.size){
              topics += ts.get(i).getAsString
            }
            isLoaded = true
          case _ =>
        }
      case _ =>
    }
  }

  abstract override def load(implicit KLOUT_API_KEY:String) = {
    if(!isLoaded) {
      loadAndParse(patternUrl = KTopics.URL, name = name)
      isLoaded = true
    }
    super.load(name)
  }

  override def reload(implicit KLOUT_API_KEY:String) = {
    loadAndParse(patternUrl = KTopics.URL, name = name)
    super.load(name)
  }
}

/**
 * Klout Topics Companion object providing dynamic Mixin
 *
 * @author Pascal Voitot [@mandubian]
 */
object KTopics extends DynamicMixinCompanion[KTopics] with KApiMulti {
  val URL ="http://api.klout.com/1/users/topics.%s?key=%s&users=%s"

  override def ::[T <: KDefaultUser](o: T)(implicit KLOUT_API_KEY: String):Mixin[T] with KTopics = {
    val ret = new Mixin(o) with KTopics
    ret.load
    ret
  }

  override def :::[T <: KDefaultUser](map:Map[String, T])(implicit KLOUT_API_KEY: String):Map[String, Mixin[T] with KTopics] = {
    loadMulti(map.map{ case (str, user) => (str , new Mixin(user) with KTopics) })
  }

  override def loadMulti[T <: KDefaultUser](userMap:Map[String, T])(implicit KLOUT_API_KEY:String):Map[String, T] = {
    loadAndParseMulti(patternUrl = URL, userMap = userMap)
  }
}


/**
 * Klout Influencers API (calls influenced_by)
 *
 * @author Pascal Voitot [@mandubian]
 */
trait KInfluencers extends KApi {
  self: KDefaultUser =>

  private var isLoaded = false

  var influencers = ListBuffer.empty[KDefaultUser]

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
  abstract override def parse(jsonUser:JsonObject) = {
    Option(jsonUser.get("twitter_screen_name")) match {
      case Some(obj:JsonElement) =>
        val twitter_screen_name = obj.getAsString
        Option(jsonUser.get("influencers")) match {
          case Some(obj: JsonElement) =>
            val infs = obj.getAsJsonArray

            for(val i <- 1 until infs.size){
              val inf = infs.get(i).getAsJsonObject
              val user = new KDefaultUser(inf.get("twitter_screen_name").getAsString) with KScore
              user.score = inf.get("kscore").getAsFloat
              influencers += user
            }
            isLoaded = true
          case _ =>
        }
      case _ =>
    }
  }

  abstract override def load(implicit KLOUT_API_KEY:String) = {
    if(!isLoaded) {
      loadAndParse(patternUrl = KInfluencers.URL, name = name)
      isLoaded = true
    }
    super.load(name)
  }

  override def reload(implicit KLOUT_API_KEY:String) = {
    loadAndParse(patternUrl = KInfluencers.URL, name = name)
    super.load(name)
  }
}

/**
 * Klout Influencers Companion object providing dynamic Mixin
 *
 * @author Pascal Voitot [@mandubian]
 */
object KInfluencers extends DynamicMixinCompanion[KInfluencers] with KApiMulti {
  val URL ="http://api.klout.com/1/soi/influenced_by.%s?key=%s&users=%s"

  override def ::[T <: KDefaultUser](o: T)(implicit KLOUT_API_KEY: String):Mixin[T] with KInfluencers = {
    val ret = new Mixin(o) with KInfluencers
    ret.load
    ret
  }

  override def :::[T <: KDefaultUser](map:Map[String, T])(implicit KLOUT_API_KEY: String):Map[String, Mixin[T] with KInfluencers] = {
    loadMulti(map.map{ case (str, user) => (str , new Mixin(user) with KInfluencers) })
  }

  override def loadMulti[T <: KDefaultUser](userMap:Map[String, T])(implicit KLOUT_API_KEY:String):Map[String, T] = {
    loadAndParseMulti(patternUrl = URL, userMap = userMap)
  }
}



/**
 * Klout Influencees API (calls influencer_of)
 *
 * @author Pascal Voitot [@mandubian]
 */
trait KInfluencees extends KApi {
  self: KDefaultUser =>

  private val URL_INFLUENCEES = "http://api.klout.com/1/soi/influencer_of.%s?key=%s&users=%s"
  private var isLoaded = false

  var influencees = ListBuffer.empty[KDefaultUser]

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
  abstract override def parse(jsonUser:JsonObject) = {
    Option(jsonUser.get("twitter_screen_name")) match {
      case Some(obj:JsonElement) =>
        val twitter_screen_name = obj.getAsString
        Option(jsonUser.get("influencees")) match {
          case Some(obj: JsonElement) =>
            val infs = obj.getAsJsonArray

            for(val i <- 1 until infs.size){
              val inf = infs.get(i).getAsJsonObject
              val user = new KDefaultUser(inf.get("twitter_screen_name").getAsString) with KScore
              user.score = inf.get("kscore").getAsFloat
              influencees += user
            }
            isLoaded = true
          case _ =>
        }
      case _ =>
    }
  }

  abstract override def load(implicit KLOUT_API_KEY:String) = {
    if(!isLoaded) {
      loadAndParse(patternUrl = KInfluencees.URL, name = name)
      isLoaded = true
    }
    super.load(name)
  }

  override def reload(implicit KLOUT_API_KEY:String) = {
    loadAndParse(patternUrl = KInfluencees.URL, name = name)
    super.load(name)
  }
}

/**
 * Klout Influencees Companion object providing dynamic Mixin
 *
 * @author Pascal Voitot [@mandubian]
 */
object KInfluencees extends DynamicMixinCompanion[KInfluencees] with KApiMulti {
  val URL = "http://api.klout.com/1/soi/influencer_of.%s?key=%s&users=%s"

  override def ::[T <: KDefaultUser](o: T)(implicit KLOUT_API_KEY: String):Mixin[T] with KInfluencees = {
    val ret = new Mixin(o) with KInfluencees
    ret.load
    ret
  }

  override def :::[T <: KDefaultUser](map:Map[String, T])(implicit KLOUT_API_KEY: String):Map[String, Mixin[T] with KInfluencees] = {
    loadMulti(map.map{ case (str, user) => (str , new Mixin(user) with KInfluencees) })
  }

  override def loadMulti[T <: KDefaultUser](userMap:Map[String, T])(implicit KLOUT_API_KEY:String):Map[String, T] = {
    loadAndParseMulti(patternUrl = URL, userMap = userMap)
  }
}


/**
 *
 *The default Klout user with name (twitter_name) & a score
 *
 */
case class KUser(override val name:String)(implicit KLOUT_API_KEY: String, multi:Boolean = false)
  extends KDefaultUser(name) with KScore {
  if(!multi) load
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

/*class KUsers[T <: KDefaultUser](override val map:Map[String, T])(implicit KLOUT_API_KEY: String)
  extends KDefaultUserMulti[T](map) with KScoreMulti {
}*/

object KUsers {
  def apply(names:String*)(implicit KLOUT_API_KEY: String, multi:Boolean = true):Map[String, KUser] = {
    //new KUsers[KUser](HashMap.empty[String, KUser] ++ names.map(name => (name, new KUser(name))))
    val map = HashMap.empty[String, KUser] ++ names.map(name => (name, new KUser(name)))
    KScore.loadMulti(map)
  }
}

}