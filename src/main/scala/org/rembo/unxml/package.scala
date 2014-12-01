package org.rembo.unxml

import scala.util.control.NonFatal
import scala.xml.NodeSeq

object XmlErrors {
  class DeserializationException(msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)

  def deserializationError(msg: String, cause: Throwable = null) = throw new DeserializationException(msg, cause)
}

import XmlErrors._

object XmlRead {
  def apply[T](f: NodeSeq ⇒ T) = new XmlRead[T]() {
    def read(nodeSeq: NodeSeq) = f(nodeSeq)
  }
}

trait XmlRead[T] {
  def read(nodeSeq: NodeSeq): T

  def map[R](f: T ⇒ R): XmlRead[R] = XmlRead(node ⇒ f(read(node)))
  def flatMap[R](f: T ⇒ XmlRead[R]): XmlRead[R] = XmlRead(node ⇒ f(read(node)).read(node))
}

object XmlPath {
  def \(elem: String): XmlPath = XmlPath(_ \ elem)
  def \\(elem: String): XmlPath = XmlPath(_ \\ elem)

  def apply(f: NodeSeq ⇒ NodeSeq) = new XmlPath {
    override def apply(nodeSeq: NodeSeq) = f(nodeSeq)
  }
}

trait XmlPath {
  def apply(nodeSeq: NodeSeq): NodeSeq

  def \(elem: String): XmlPath = XmlPath(apply(_) \ elem)
  def \\(elem: String): XmlPath = XmlPath(apply(_) \\ elem)

  def read[T](implicit r: XmlRead[T]): XmlRead[T] = XmlRead { node ⇒
    val n = apply(node)
    if (n.isEmpty) deserializationError("Node not found.")
    try {
      r.read(n)
    } catch {
      case NonFatal(t) ⇒ deserializationError("Error while parsing ", t)
    }
  }

  def readOptional[T](implicit r: XmlRead[T]): XmlRead[Option[T]] = XmlRead { node ⇒
    val n = apply(node)
    if (n.isEmpty) None
    else Some {
      try {
        r.read(n)
      } catch {
        case NonFatal(t) ⇒ deserializationError("Error while parsing ", t)
      }
    }
  }
}
