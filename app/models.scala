package models

import play.db.anorm._
import play.db.anorm.defaults._
import play.db.anorm
import play.db.anorm.SqlParser._

import play.data.validation.Required
import play.Logger


case class User(
    var id:Pk[Long],
    val twitterHandle: String,
    val token: String,
	val secret: String) {

    def this(twitterHandle: String, token: String, secret: String) {
        this (NotAssigned, twitterHandle, token, secret)
    }
}



object User extends Magic[User] {


    def exists(twitterHandle: String): Boolean = {
        val count: Long = SQL("select count(*) from User where twitterHandle = {u}")
        .on("u" -> twitterHandle)
        .as(scalar[Long])
        count match {
            case count if count > 0 => true
            case _ => false
        }
    }


    def findOrCreate(twitterHandle: String, token: String, secret: String): User = {
        // Look for an existing user
        find("twitterHandle = {r}").on("r" -> twitterHandle).first() match {

            // Ok just return the user that was found
            case Some(o: User) => {
                Logger.debug("Found User: " + o)
                o
            }

            // Nothing was found
            case None => {
                val o = new User(twitterHandle, token, secret)
                val created = insert(o)
                Logger.debug("Created User: " + created)
                o
            }
        }
    }

}
