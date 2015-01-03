package org.rembo.unxml
package streaming
package transformers

import akka.stream.stage._

import XmlStreamingTypes._
import XmlTransformer._

class XmlStringReader extends StatefulStage[XmlEvent, XmlResult[String]] {
  override def initial = outside

  val text = new StringBuilder()

  def outside: StageState[XmlEvent, XmlResult[String]] = new StageState[XmlEvent, XmlResult[String]] {
    override def onPush(elem: XmlEvent, ctx: Context[XmlResult[String]]): Directive = {
      elem match {
        case StartDocument ⇒
          text.clear()
          become(in)
          ctx.pull()
        case NoMatch(path) ⇒ ctx.push(XmlError("Node not found", path, true))
        case _             ⇒ ctx.pull()
      }
    }
  }

  def in: StageState[XmlEvent, XmlResult[String]] = new StageState[XmlEvent, XmlResult[String]] {
    override def onPush(elem: XmlEvent, ctx: Context[XmlResult[String]]): Directive = {
      elem match {
        case Characters(value) ⇒
          text ++= value
          ctx.pull()
        case EndDocument ⇒
          become(outside)
          ctx.push(XmlSuccess(text.toString.trim))
        case _ ⇒ ctx.pull()
      }
    }
  }
}
