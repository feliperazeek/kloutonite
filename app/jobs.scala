package jobs

import models._
import play._
import play.jobs._
import java.util._

/**
 * Daily Status Update Job
 *
 * @author Felipe Oliveira [@_felipera]
 */
class DailyStatusUpdateJob extends Job {
	
	/**
	 * List all users registered, get their updated Klout score and update their Twitter status with it
	 */
	override def doJob = {
		// Time Start Time
		val start = new Date
		
		// Log Debug
		Logger.info("Daily Status Update Job - Start: " + start)
		
		// Time End Time
		val end = new Date
		val total = end.getTime - start.getTime
		
		// Log Debug
		Logger.info("Daily Status Update Job - End: " + end + ", Total: " + total)
	}
	
}