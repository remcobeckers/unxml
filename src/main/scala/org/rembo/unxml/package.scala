package org.rembo.unxml

import scala.util.control.NonFatal

trait XmlBaseTypes {
  object XmlPath {
    val empty = ElemPath(List.empty)

    def \(elem: String): ElemPath = ElemPath(List(elem))
    def apply(): XmlPath = ElemPath(List.empty)
  }

  trait XmlPath {
    def elems: List[String]
    def init: ElemPath
    def startsWith(other: XmlPath): Boolean
    def isEmpty: Boolean
  }

  case class ElemPath(elems: List[String]) extends XmlPath {
    def \(elem: String): ElemPath = ElemPath(elems :+ elem)

    def `@`(attribute: String): AttrPath = AttrPath(elems, attribute)

    def init = ElemPath(elems.init)

    def isEmpty = elems.isEmpty

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

    def isEmpty = false

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

    //TODO: Accumulate all errors instead of stopping after the first
    def sequence[T](seq: Seq[XmlResult[T]]): XmlResult[Seq[T]] = {
      seq.zipWithIndex.foldLeft(XmlSuccess(Vector.empty): XmlResult[Seq[T]]) {
        case (r @ XmlError(_, _, _), _) ⇒ r
        case (acc, (res, index)) ⇒ (acc, res) match {
          case (XmlSuccess(results), XmlSuccess(v))   ⇒ XmlSuccess(results :+ v)
          case (_, XmlError(error, path, notFound))   ⇒ XmlError(s"$error at index $index", path, notFound)
          case (e @ XmlError(_, _, _), XmlSuccess(v)) ⇒ e
        }
      }
    }
  }

  sealed trait XmlResult[+T] {
    def map[R](f: T ⇒ R): XmlResult[R] = this match {
      case XmlSuccess(v) ⇒ XmlResult(f(v))
      case e: XmlError   ⇒ e
    }

    def flatMap[R](f: T ⇒ XmlResult[R]): XmlResult[R] = this match {
      case XmlSuccess(v) ⇒ f(v)
      case e: XmlError   ⇒ e
    }

    def addErrorPathPrefix(path: XmlPath) = this match {
      case XmlError(e, errorPath, notFound) ⇒
        path match {
          case p: ElemPath                      ⇒ XmlError(e, errorPath = p ++ errorPath, notFound)
          case p: AttrPath if errorPath.isEmpty ⇒ XmlError(e, errorPath = p, notFound)
          case _                                ⇒ throw new IllegalStateException("Error path prefix cannot be an attribute path.")
        }
      case s: XmlSuccess[T] ⇒ s
    }

    def orElse[TT >: T](f: ⇒ XmlResult[TT]) = this match {
      case XmlError(_, _, _)     ⇒ f
      case other @ XmlSuccess(_) ⇒ other
    }

    def getOrElse[TT >: T](otherwise: ⇒ TT) = this match {
      case XmlError(_, _, _) ⇒ otherwise
      case XmlSuccess(v)     ⇒ v
    }
  }

  case class XmlSuccess[T](value: T) extends XmlResult[T]
  case class XmlError(error: String, errorPath: XmlPath = XmlPath.empty, notFound: Boolean = false) extends XmlResult[Nothing]
}
