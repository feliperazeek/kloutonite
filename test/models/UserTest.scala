package models

import java.util._
import play.Logger
import play.test._
import org.scalatest.junit._
import org.scalatest._
import org.scalatest.matchers._
import models._

/**
 * User Test Cases
 *
 * @author Felipe Oliveira [@_felipera]
 */
class UserTest extends UnitTest with FlatSpec with ShouldMatchers {

    /**
     * Test for User.findOrCreate()
     */
    "Calling findOrCreate() twice on User" should "only create one record in the db" in {
			val r = new Random
			val name = r.nextInt.toString
			val token = "token"
			val secret = "secret"

            val r1 = User.findOrCreate(name, token, secret)
            val r2 = User.findOrCreate(name, token, secret)

            (r1) should be (r2)
    }

    /**
     * Test for User.exist
     */
    "Calling exists() on User" should "be true if user is defined or false if otherwise" in {
			val r = new Random
			val name = r.nextInt.toString
			val token = "token"
			val secret = "secret"
			
            val b1 = User.exists(name)
            (b1) should be (false)

            val c = User.findOrCreate(name, token, secret)
			Logger.debug("Created: " + c)

            val b2 = User.exists(name)
            (b2) should be (true)
    }

}
