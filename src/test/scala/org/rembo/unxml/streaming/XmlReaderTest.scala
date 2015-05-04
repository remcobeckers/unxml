package org.rembo.unxml
package streaming

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._
import akka.testkit.TestKit
import akka.util.ByteString
import org.rembo.unxml.streaming.transformers.XmlTransformer
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.duration._

import XmlStreamingTypes._

class XmlReaderTest extends TestKit(ActorSystem()) with WordSpecLike with Matchers with OptionValues with Inside with Inspectors {

  implicit val mat = ActorFlowMaterializer()

  "When parsing a document the streaming parser with XmlReader" should {
    "reads a single string element as expected" in {
      val xmlByteString = ByteString(
        """<top>
          | <children>
          |   <child attr="1" hello="yeah">One</child>
          |   <child>Two</child>
          |   <child>Three</child>
          | </children>
          |</top>
        """.stripMargin)

      val byteSource = Source(() ⇒ xmlByteString.grouped(15))
      val reader = (XmlPath \ "top" \ "children" \ "child").read[String]
      val resultSource = UnXml.fromXml(byteSource)(reader)

      val foldSink = Sink.fold[Vector[XmlResult[String]], XmlResult[String]](Vector.empty)((acc, event) ⇒ acc :+ event)
      val result = Await.result(resultSource.runWith(foldSink), 5.seconds)

      result should contain inOrderOnly (XmlSuccess("One"), XmlSuccess("Two"), XmlSuccess("Three"))
    }

    "be able to combine multiple fields into a case class" in {
      val xmlByteString = ByteString(
        """<top>
          | <children>
          |   <child>
          |     <string>string</string>
          |     <int>1</int>
          |   </child>
          | </children>
          |</top>
        """.stripMargin)

      case class Person(name: String, age: Int)

      val reader = (
        (XmlPath \ "top" \ "children" \ "child").read(
          (XmlPath \ "string").read[String] and
            (XmlPath \ "int").read[Int]
        ).as(Person)
      )

      val byteSource = Source(() ⇒ xmlByteString.grouped(15))
      val resultSource = UnXml.fromXml(byteSource)(reader)

      val foldSink = Sink.fold[Vector[XmlResult[Person]], XmlResult[Person]](Vector.empty)((acc, event) ⇒ acc :+ event)
      val result = Await.result(resultSource.runWith(foldSink), 5.minutes)

      result should contain(XmlSuccess(Person("string", 1)))
    }

    "be able to parse into a traversable" in {
      val xmlByteString = ByteString(
        """<top>
          | <children>
          |   <child>one</child>
          |   <child>two</child>
          |   <child>three</child>
          | </children>
          | <children>
          |   <child>1</child>
          |   <child>2</child>
          |   <child>3</child>
          | </children>
          |</top>
        """.stripMargin)

      case class Person(name: String, age: Int)

      val reader = (XmlPath \ "top" \ "children").readAll[Vector, String]("child", 10)

      val byteSource = Source(() ⇒ xmlByteString.grouped(15))
      val resultSource = UnXml.fromXml(byteSource)(reader)

      val foldSink = Sink.fold[Vector[XmlResult[Vector[String]]], XmlResult[Vector[String]]](Vector.empty)((acc, event) ⇒ acc :+ event)
      val result = Await.result(resultSource.runWith(foldSink), 5.minutes)

      result should contain(XmlSuccess(Vector("one", "two", "three")))
      result should contain(XmlSuccess(Vector("1", "2", "3")))
    }
  }
}
