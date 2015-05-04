package org.rembo.unxml.streaming
package transformers

import javax.xml.stream.XMLStreamException

import akka.stream.stage._
import akka.util.ByteString
import com.fasterxml.aalto.stax.InputFactoryImpl
import org.rembo.unxml.streaming.transformers.XmlTransformer.XmlEvent

object XmlTransformer {
  sealed trait XmlEvent

  case class Attribute(localName: String, prefix: String, nsUri: String, value: String)
  case class StartElement(localName: String, prefix: String, nsUri: String, attributes: Seq[Attribute]) extends XmlEvent
  case class EndElement(localName: String, prefix: String, nsUri: String) extends XmlEvent
  case class Characters(value: String) extends XmlEvent

  case object StartDocument extends XmlEvent
  case object EndDocument extends XmlEvent
  case class NoMatch(path: XmlStreamingTypes.XmlPath) extends XmlEvent

  private[XmlTransformer] val xmlInputFactory = new InputFactoryImpl()
}

private[transformers] object JavaXmlEvents {
  val StartElement = 1
  val EndElement = 2
  val ProcessingInstruction = 3
  val Characters = 4
  val Comment = 5
  val Space = 6
  val StartDocument = 7
  val EndDocument = 8
  val EntityReference = 9
  val Attribute = 10
  val Dtd = 11
  val CData = 12
  val NameSpace = 13
  val NotationDeclaration = 14
  val EntityDeclaration = 15
}

class XmlTransformer extends StatefulStage[ByteString, XmlEvent] {
  import com.fasterxml.aalto.AsyncXMLStreamReader
  import org.rembo.unxml.streaming.transformers.XmlTransformer._

  private val asyncXMLStreamReader = xmlInputFactory.createAsyncXMLStreamReader()
  private val asyncInputFeeder = asyncXMLStreamReader.getInputFeeder()

  override def initial = new StageState[ByteString, XmlEvent] {
    override def onPush(elem: ByteString, ctx: Context[XmlEvent]): SyncDirective = {
      try {
        asyncInputFeeder.feedInput(elem.toArray[Byte], 0, elem.length)
        var eventType = asyncXMLStreamReader.next()
        var outEvents = Vector.empty[XmlEvent]
        while (eventType != AsyncXMLStreamReader.EVENT_INCOMPLETE) {
          eventType match {
            case JavaXmlEvents.StartDocument ⇒ outEvents :+= StartDocument

            case JavaXmlEvents.StartElement ⇒
              val name = asyncXMLStreamReader.getName
              val elem = StartElement(name.getLocalPart, name.getPrefix, name.getNamespaceURI, attributes())
              outEvents :+= elem

            case JavaXmlEvents.Characters ⇒
              val text: String = asyncXMLStreamReader.getText
              outEvents :+= Characters(text)

            case JavaXmlEvents.EndElement ⇒
              val name = asyncXMLStreamReader.getName
              val elem = EndElement(name.getLocalPart, name.getPrefix, name.getNamespaceURI)
              outEvents :+= elem

            case JavaXmlEvents.EndDocument ⇒ outEvents :+= EndDocument
            case _                         ⇒
          }
          eventType = asyncXMLStreamReader.next()
        }
        eventType match {
          case AsyncXMLStreamReader.EVENT_INCOMPLETE if outEvents.isEmpty ⇒ ctx.pull
          case JavaXmlEvents.EndDocument ⇒
            done()
            emitAndFinish(outEvents.toIterator, ctx)
          case _ if !outEvents.isEmpty ⇒ emit(outEvents.toIterator, ctx)
        }
      } catch {
        case error: XMLStreamException ⇒
          done()
          ctx.fail(error)
      }
    }
  }

  private def attributes(): Vector[Attribute] = {
    (0 until asyncXMLStreamReader.getAttributeCount).map { idx ⇒
      val name = asyncXMLStreamReader.getAttributeName(idx)
      Attribute(name.getLocalPart, name.getPrefix, name.getNamespaceURI, asyncXMLStreamReader.getAttributeValue(idx))
    }.toVector
  }

  private def done() = {
    asyncInputFeeder.endOfInput()
    asyncXMLStreamReader.close()
  }

  override def onUpstreamFinish(ctx: Context[XmlEvent]) = {
    done()
    super.onUpstreamFinish(ctx)
  }
  override def onDownstreamFinish(ctx: Context[XmlEvent]) = {
    done()
    super.onDownstreamFinish(ctx)
  }
  override def onUpstreamFailure(cause: Throwable, ctx: Context[XmlEvent]) = {
    done()
    super.onUpstreamFailure(cause, ctx)
  }
}
