package org.rembo.unxml.streaming

import org.rembo.unxml.streaming.transformers.XmlTransformer

import scala.concurrent.duration._
import scala.concurrent.Await

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._
import akka.testkit.TestKit
import akka.util.ByteString

import org.scalatest._

class XmlTransformerTest extends TestKit(ActorSystem()) with WordSpecLike with Matchers with OptionValues with Inside with Inspectors {
  import XmlTransformer._
  implicit val mat = ActorFlowMaterializer()

  "When parsing a document the streaming parser" should {
    "generate the expected events" in {
      val xmlByteString = ByteString(
        """<top>
          | <children>
          |   <child1 attr="1" hello="yeah">One</child1>
          |   <child2>Two</child2>
          |   <child3>Three</child3>
          | </children>
          |</top>
        """.stripMargin)

      val xmlSource = Source(() ⇒ xmlByteString.grouped(15)).transform(() ⇒ new XmlTransformer())
      val foldSink = Sink.fold[Vector[XmlEvent], XmlEvent](Vector.empty)((acc, event) ⇒ acc :+ event)
      val result = Await.result(xmlSource.runWith(foldSink), 5.seconds)

      result should contain inOrder (
        StartElement("top", "", "", Vector.empty),
        StartElement("child1", "", "", Vector(Attribute("attr", "", "", "1"), Attribute("hello", "", "", "yeah"))),
        Characters("One"),
        EndElement("child1", "", ""),
        StartElement("child2", "", "", Vector.empty),
        Characters("Two"),
        EndElement("child2", "", ""),
        StartElement("child3", "", "", Vector.empty),
        Characters("Three"),
        EndElement("child3", "", ""),
        EndElement("top", "", "")
      )
    }
  }
}
