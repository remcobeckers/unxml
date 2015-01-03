package org.rembo.unxml
package blocking

import scala.language.higherKinds
import scala.annotation.implicitNotFound
import scala.collection.generic
import scala.xml.NodeSeq

object XmlBlockingTypes extends XmlBaseTypes {
  object XmlReads {
    def apply[T](f: String ⇒ XmlResult[T]) = new XmlReads[T]() {
      def reads(nodeSeq: NodeSeq) = f(nodeSeq.text)
    }
  }

  def xmlReads[T](f: NodeSeq ⇒ XmlResult[T]) = new XmlReads[T]() {
    def reads(nodeSeq: NodeSeq) = f(nodeSeq)
  }

  @implicitNotFound(msg = "An implicit XmlReads for ${T} is required.")
  trait XmlReads[T] {
    def reads(nodeSeq: NodeSeq): XmlResult[T]

    def map[R](f: T ⇒ R): XmlReads[R] = xmlReads { node ⇒ reads(node).map(v ⇒ f(v)) }
    def flatMap[R](f: T ⇒ XmlReads[R]): XmlReads[R] = xmlReads { node ⇒ reads(node).flatMap(t ⇒ f(t).reads(node)) }

    def mapResult[R](f: XmlResult[T] ⇒ XmlResult[R]): XmlReads[R] = xmlReads(node ⇒ f(reads(node)))
  }

  implicit class XmlPathOps(path: XmlPath) {
    def apply(node: NodeSeq): NodeSeq = path match {
      case AttrPath(elems, attr) ⇒ elems.foldLeft(node) { (nodeSeq, elem) ⇒ nodeSeq \ elem } \ s"@$attr"
      case ElemPath(elems)       ⇒ elems.foldLeft(node) { (nodeSeq, elem) ⇒ nodeSeq \ elem }
    }

    def read[T](implicit r: XmlReads[T]): XmlReads[T] = xmlReads { node ⇒
      val n = apply(node)
      if (n.isEmpty) XmlError("Node not found", path)
      r.reads(n).addErrorPathPrefix(path)
    }
  }

  implicit class ElemPathOps(path: ElemPath) {
    def readAll[F[_], T](child: String, maxSize: Int = Int.MaxValue)(implicit reads: XmlReads[T], bf: generic.CanBuildFrom[F[_], T, F[T]]): XmlReads[F[T]] = {
      xmlReads[F[T]] { node ⇒
        val childPath = path \ child
        val n = childPath(node)
        XmlResult.sequence(n.map(reads.reads(_).addErrorPathPrefix(childPath))).map { values ⇒
          val builder = bf()
          values.foreach(v ⇒ builder += v)
          builder.result()
        }
      }
    }
  }
}