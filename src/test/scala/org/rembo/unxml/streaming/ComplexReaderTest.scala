package org.rembo.unxml.streaming

import java.util.Locale

import scala.concurrent.Await
import scala.concurrent.duration._

import akka.actor.ActorSystem
import akka.stream.FlowMaterializer
import akka.stream.scaladsl._
import akka.testkit.TestKit
import akka.util.ByteString

import XmlStreamingTypes._
import org.joda.time._
import org.joda.time.format._
import org.scalatest._

object AnnouncementsDataModel {
  case class Id(value: String)

  case class Address(zipcode: String, number: Option[String], suffix: Option[String])

  case class Announcement(
    id: Id,
    title: String,
    description: String,
    addresses: Set[Address],
    modified: DateTime,
    start: Option[DateTime],
    end: Option[DateTime],
    productType: String)
}

trait DateTimeXmlReads extends DefaultXmlReads {
  val LocaleNL = new Locale("nl", "NL")
  val TimeZoneNL = DateTimeZone.forID("Europe/Amsterdam")

  val governmentXmlDateTimeFormat = new DateTimeFormatterBuilder()
    .append(DateTimeFormat.forPattern("yyyy-MM-dd"))
    .appendOptional(DateTimeFormat.forPattern("'T'HH:mm:ss").getParser)
    .toFormatter.withZone(TimeZoneNL).withLocale(LocaleNL)

  implicit val dateTimeReads = stringReads.map(governmentXmlDateTimeFormat.parseDateTime)
}

trait AnnouncementXmlReads extends DateTimeXmlReads with DefaultXmlReads {
  import AnnouncementsDataModel._

  implicit val idReads = stringReads.map(Id)

  implicit val optionalAddressReads = (
    (XmlPath \ "adres" \ "postcodeHuisnummer" \ "postcode").read[String] and
    (XmlPath \ "adres" \ "postcodeHuisnummer" \ "huisnummer").readOptional[String] and
    (XmlPath \ "adres" \ "postcodeHuisnummer" \ "huisletter").readOptional[String]
  ).as(Address).mapResult(r ⇒ XmlSuccess(r.toOption))

  implicit val announcementReads = (XmlPath \ "searchRetrieveResponse" \ "records" \ "record" \ "recordData" \ "gzd" \ "originalData" \ "meta").read(
    (XmlPath \ "owmskern" \ "identifier").read[Id] and
      (XmlPath \ "owmskern" \ "title").read[String] and
      (XmlPath \ "owmsmantel" \ "description").read[String] and
      (XmlPath \ "bekendmakingenmeta").readAll[Set, Option[Address]]("object", 100).map(_.flatten) and
      (XmlPath \ "owmskern" \ "modified").read[DateTime] and
      (XmlPath \ "owmskern" \ "temporal" \ "start").readOptional[DateTime] and
      (XmlPath \ "owmskern" \ "temporal" \ "end").readOptional[DateTime] and
      (XmlPath \ "bekendmakingenmeta" \ "product" \ "producttype").read[String]
  ).as(Announcement)
}

class ComplexReaderTest extends TestKit(ActorSystem()) with WordSpecLike with Matchers with OptionValues with Inside with Inspectors
    with AnnouncementXmlReads {
  import AnnouncementsDataModel._

  implicit val mat = FlowMaterializer()

  "When parsing a document the streaming parser with XmlReader" should {
    "be able to combine many complex fields into a structure of case classes" in {
      val announcementsSource = UnXml.fromXml[Announcement](fakeSource("/announcements.xml"))

      val vectorSink = Sink.fold[Vector[XmlResult[Announcement]], XmlResult[Announcement]](Vector.empty)((acc, event) ⇒ acc :+ event)
      val resultFuture = announcementsSource.runWith(vectorSink)
      val result = Await.result(resultFuture, 10.seconds)

      result should have('size(200))
    }
  }

  /**
   * Convert a file into a Source of ByteString with chunks of 15 bytes
   * @param file
   * @return
   */
  def fakeSource(file: String) = {
    val stringData = scala.io.Source.fromInputStream(getClass.getResourceAsStream(file)).mkString
    Source(() ⇒ ByteString(stringData).grouped(15))
  }
}
