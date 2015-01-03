package org.rembo.unxml
package streaming
package transformers

import akka.stream.stage._

import XmlStreamingTypes._
import XmlTransformer._

// TODO: Add attribute support
class XmlPathFilter(path: XmlPath) extends StatefulStage[XmlEvent, XmlEvent] {
  override def initial = outsideDocument

  def outsideDocument = new StageState[XmlEvent, XmlEvent] {
    override def onPush(elem: XmlEvent, ctx: Context[XmlEvent]): Directive = {
      elem match {
        case StartDocument ⇒
          if (path.isEmpty) become(insidePath(XmlPath.empty)) else become(outsidePath(XmlPath.empty, false))
        case _ ⇒
      }
      ctx.pull()
    }
  }

  def outsidePath(currentPath: ElemPath, wasInside: Boolean): StageState[XmlEvent, XmlEvent] = new StageState[XmlEvent, XmlEvent] {
    override def onPush(elem: XmlEvent, ctx: Context[XmlEvent]): Directive = {
      elem match {
        case StartElement(name, _, _, _) ⇒
          val newPath = currentPath \ name
          if (newPath == path) {
            become(insidePath(newPath))
            ctx.push(StartDocument)
          } else {
            become(outsidePath(newPath, wasInside))
            ctx.pull()
          }
        case EndElement(name, _, _) ⇒
          become(outsidePath(currentPath.init, wasInside))
          ctx.pull()
        case EndDocument ⇒
          become(outsideDocument)
          if (!wasInside) ctx.push(NoMatch(path))
          else ctx.pull()
        case _ ⇒ ctx.pull()
      }
    }
  }

  def insidePath(currentPath: ElemPath): StageState[XmlEvent, XmlEvent] = new StageState[XmlEvent, XmlEvent] {
    override def onPush(elem: XmlEvent, ctx: Context[XmlEvent]): Directive = {
      elem match {
        case evt @ StartElement(name, _, _, _) ⇒
          become(insidePath(currentPath \ name))
          ctx.push(evt)
        case evt @ EndElement(name, _, _) ⇒
          val newPath = currentPath.init
          if (newPath.startsWith(path)) {
            become(insidePath(currentPath.init))
            ctx.push(evt)
          } else {
            become(outsidePath(currentPath.init, true))
            ctx.push(EndDocument)
          }
        case EndDocument ⇒
          become(outsideDocument)
          ctx.pull()
        case evt: Characters  ⇒ ctx.push(evt)
        case evt @ NoMatch(_) ⇒ ctx.push(evt)
        case _                ⇒ ctx.pull()
      }
    }
  }
}
