package bot

import java.sql.Timestamp
import javax.inject.Inject

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.concurrent.Execution.Implicits._
import slick.driver.JdbcProfile
import slick.lifted.{PrimaryKey, ProvenShape}

import scala.concurrent.Future

/**
  * Storage for user verification steps.
  */
class VerificationSteps @Inject() (
  protected val dbConfigProvider: DatabaseConfigProvider
) extends HasDatabaseConfigProvider[JdbcProfile] {

  import driver.api._

  private class VerificationStepsTable(tag: Tag) extends Table[VerificationStep](tag, "verification_step") {
    def guildID: Rep[String] = column[String]("guild_id")
    def verifySessionUUID: Rep[String] = column[String]("verify_session_uuid")
    def name: Rep[String] = column[String]("name")
    def ts: Rep[Timestamp] = column[Timestamp]("ts")
    def data: Rep[String] = column[String]("data") // TODO: repent sins, use step-specific types

    def pk: PrimaryKey = primaryKey("verification_step_pk", (guildID, verifySessionUUID, name, ts))

    def * : ProvenShape[VerificationStep] = (guildID, verifySessionUUID, name, ts, data) <> (VerificationStep.tupled, VerificationStep.unapply)
  }

  private val VerificationStepsQuery = TableQuery[VerificationStepsTable]

  def createTable(): Future[Unit] = {
    db.run(VerificationStepsQuery.schema.create)
  }

  def insert(step: VerificationStep): Future[Unit] = {
    db
      .run(VerificationStepsQuery += step)
      .map(_ => ())
  }

  def latest(guildID: String, verifySessionUUID: String): Future[Option[VerificationStep]] = {
    db
      .run(
        VerificationStepsQuery
          .filter(step => step.guildID === guildID && step.verifySessionUUID === verifySessionUUID)
          .sortBy(_.ts.desc)
          .take(1)
          .result
      )
      .map(_.headOption)
  }

  def all(guildID: String, verifySessionUUID: String): Future[Seq[VerificationStep]] = {
    db
      .run(
        VerificationStepsQuery
          .filter(step => step.guildID === guildID && step.verifySessionUUID === verifySessionUUID)
          .result
      )
  }

  def pagedForGuild(guildID: String, limit: Int, offset: Int): Future[Seq[VerificationStep]] = {
    db
      .run(
        VerificationStepsQuery
          .filter(step => step.guildID === guildID)
          .sortBy(_.ts.desc)
          .drop(offset)
          .take(limit)
          .result
      )
  }
}

/**
  * User verification step.
  */
case class VerificationStep(
  guildID: String,
  verifySessionUUID: String,
  name: String,
  ts: Timestamp,
  data: String
)

object VerificationSteps {

  val names: IndexedSeq[String] = IndexedSeq(
    "000_landing",
    "010_tos",
    "020_discord",
    "030_reddit",
    "040_invite"
  )

  def nextStepName(current: Option[String]): Option[String] = {
    current match {
      case None => names.headOption
      case Some(name) =>
        val idx = names.indexOf(name)
        if (idx == -1) {
          throw new IllegalArgumentException(s"$name isn't a valid verification step!")
        } else if (idx == names.length - 1) {
          None
        } else {
          Some(names(idx + 1))
        }
    }
  }
}
