package org.rembo.unxml

import annotation._
import scala.util.control.NonFatal
import scala.xml.NodeSeq

object XmlReads {
  def apply[T](f: NodeSeq ⇒ XmlResult[T]) = new XmlReads[T]() {
    def reads(nodeSeq: NodeSeq) = f(nodeSeq)
  }
}

@implicitNotFound(msg = "An implicit XmlRead for ${T} is required.")
trait XmlReads[T] {
  def reads(nodeSeq: NodeSeq): XmlResult[T]

  def map[R](f: T ⇒ R): XmlReads[R] = XmlReads { node ⇒ reads(node).map(v ⇒ f(v)) }
  def flatMap[R](f: T ⇒ XmlReads[R]): XmlReads[R] = XmlReads { node ⇒ reads(node).flatMap(t ⇒ f(t).reads(node)) }
}

object XmlPath {
  val empty = ElemPath(List.empty)

  def \(elem: String): ElemPath = ElemPath(List(elem))
  def apply(): XmlPath = ElemPath(List.empty)
}

trait XmlPath {
  def elems: List[String]
  def apply(node: NodeSeq): NodeSeq
  def init: ElemPath
  def startsWith(other: XmlPath): Boolean

  def read[T](implicit r: XmlReads[T]): XmlReads[T] = XmlReads { node ⇒
    val n = apply(node)
    if (n.isEmpty) XmlError("Node not found", this)
    r.reads(n)
  }

  def readOptional[T](implicit r: XmlReads[T]): XmlReads[Option[T]] = XmlReads { node ⇒
    val n = apply(node)
    if (n.isEmpty) XmlSuccess(None)
    else r.reads(n).map(Some(_))
  }
}

case class ElemPath(elems: List[String]) extends XmlPath {
  def \(elem: String): ElemPath = ElemPath(elems :+ elem)

  def `@`(attribute: String): AttrPath = AttrPath(elems, attribute)

  def apply(node: NodeSeq): NodeSeq = elems.foldLeft(node) { (nodeSeq, elem) ⇒ nodeSeq \ elem }

  def init = ElemPath(elems.init)

  def startsWith(other: XmlPath) = other match {
    case ElemPath(otherElems) ⇒ elems.startsWith(otherElems)
    case AttrPath(_, _)       ⇒ false
  }

  def ++(other: XmlPath) = other match {
    case ElemPath(otherElems)       ⇒ ElemPath(elems ::: otherElems)
    case AttrPath(otherElems, attr) ⇒ AttrPath(elems ::: otherElems, attr)
  }

  override def toString: String = elems.mkString("\\")
}

case class AttrPath(elems: List[String], attr: String) extends XmlPath {
  override def toString: String = elems.mkString("\\") + s"@$attr"

  override def init: ElemPath = ElemPath(elems)

  override def readOptional[T](implicit r: XmlReads[T]): XmlReads[Option[T]] = ???

  override def read[T](implicit r: XmlReads[T]): XmlReads[T] = ???

  def apply(node: NodeSeq): NodeSeq = elems.foldLeft(node) { (nodeSeq, elem) ⇒ nodeSeq \ elem } \ s"@$attr"

  def startsWith(other: XmlPath) = other match {
    case ElemPath(otherElems)            ⇒ elems.startsWith(otherElems)
    case AttrPath(otherElems, otherAttr) ⇒ elems == otherElems && attr == otherAttr
  }
}

object XmlResult {
  def apply[T](valueProducer: ⇒ T) = {
    try {
      XmlSuccess(valueProducer)
    } catch {
      case NonFatal(t) ⇒ XmlError(s"Error while parsing ${t.getMessage}")
    }
  }
}

sealed trait XmlResult[+T] {
  def map[R](f: T ⇒ R): XmlResult[R] = this match {
    case XmlSuccess(v) ⇒ XmlSuccess(f(v))
    case e: XmlError   ⇒ e
  }

  def flatMap[R](f: T ⇒ XmlResult[R]): XmlResult[R] = this match {
    case XmlSuccess(v) ⇒ f(v)
    case e: XmlError   ⇒ e
  }

  def addErrorPathPrefix(path: ElemPath) = this match {
    case XmlError(e, errorPath) ⇒ XmlError(e, errorPath = path ++ errorPath)
    case s: XmlSuccess[T]       ⇒ s
  }

  def orElse[TT >: T](f: ⇒ XmlResult[TT]) = this match {
    case XmlError(_, _)        ⇒ f
    case other @ XmlSuccess(_) ⇒ other
  }

  def getOrElse[TT >: T](otherwise: ⇒ TT) = this match {
    case XmlError(_, _) ⇒ otherwise
    case XmlSuccess(v)  ⇒ v
  }
}

case class XmlSuccess[T](value: T) extends XmlResult[T]
case class XmlError(error: String, errorPath: XmlPath = XmlPath.empty) extends XmlResult[Nothing]
