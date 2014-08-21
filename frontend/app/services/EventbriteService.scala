package services

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.iteratee.{Iteratee, Enumerator}
import play.api.Logger

import model.Eventbrite._
import model.EventbriteDeserializer._
import configuration.Config
import play.api.libs.json.Reads
import utils.ScheduledTask

trait EventbriteService {

  val apiEventListUrl: String

  def get[A <: EBObject](url: String, params: (String, String)*)(implicit reads: Reads[A]): Future[A]
  def post[A <: EBObject](url: String, data: Map[String, Seq[String]])(implicit reads: Reads[A]): Future[A]

  def events: Seq[EBEvent]

  private def extract[A <: EBObject](response: WSResponse)(implicit reads: Reads[A]): A = {
    response.json.asOpt[A].getOrElse {
      Logger.error(s"Eventbrite request - Response body : ${response.body}")
      throw response.json.asOpt[EBError].getOrElse(EBError("internal", "Unable to extract object", 500))
    }
  }

  private def getPaginated[T](url: String)(implicit reads: Reads[EBResponse[T]]): Future[Seq[T]] = {
    val enumerator = Enumerator.unfoldM(Option(1)) {
      _.map { nextPage =>
        for {
          response <- get[EBResponse[T]](url, "page" -> nextPage.toString)
        } yield Some((response.pagination.nextPageOpt, response.data))
      }.getOrElse(Future.successful(None))
    }

    enumerator(Iteratee.consume()).flatMap(_.run)
  }

  private def getAllEvents: Future[Seq[EBEvent]] = getPaginated[EBEvent](apiEventListUrl)

  def getLiveEvents: Seq[EBEvent] = events.filter { event =>
    event.getStatus == EBEventStatus.SoldOut || event.getStatus == EBEventStatus.Live
  }

  /**
   * scuzzy implementation to enable basic 'filtering by tag' - in this case, just matching the event name.
   */
  def getEventsTagged(tag: String) = getLiveEvents.filter(_.name.text.toLowerCase.contains(tag))

  def getEvent(id: String): Future[EBEvent] = get[EBEvent](s"events/$id")

  def createOrGetDiscount(eventId: String, code: String): Future[EBDiscount] = {
    val uri = s"events/$eventId/discounts"

    for {
      discounts <- getPaginated[EBDiscount](uri)
      discount <- discounts.find(_.code == code).map(Future.successful).getOrElse {
        post[EBDiscount](uri, Map(
          "discount.code" -> Seq(code),
          "discount.percent_off" -> Seq("20"),
          "discount.quantity_available" -> Seq("2")
        ))
      }
    } yield discount
  }
}

object EventbriteService extends EventbriteService with ScheduledTask[Seq[EBEvent]] {
  val initialValue = Nil
  val initialDelay = 5.seconds
  val interval = 60.seconds

  def refresh(): Future[Seq[EBEvent]] = getAllEvents

  def events: Seq[EBEvent] = agent.get()

  val apiUrl = Config.eventbriteApiUrl
  val apiToken = Config.eventbriteApiToken
  val apiEventListUrl = Config.eventbriteApiEventListUrl

  def get[A <: EBObject](url: String, params: (String, String)*)(implicit reads: Reads[A]): Future[A] = {
    WS.url(s"$apiUrl/$url").withQueryString("token" -> apiToken).withQueryString(params: _*).get()
      .map(extract[A])
      .recover { case e =>
      Logger.error(s"Eventbrite request $url", e)
      throw e
    }
  }

  def post[A <: EBObject](url: String, data: Map[String, Seq[String]])(implicit reads: Reads[A]): Future[A] =
    WS.url(s"$apiUrl/$url").withQueryString("token" -> apiToken).post(data).map(extract[A])
}

