package controllers

import play.api.mvc.{AnyContent, Request}

sealed trait RequestType

object RequestType {
  def requestType(implicit request:Request[AnyContent]) = apply(request)

  private[this] def acc(t: String)(implicit request: Request[AnyContent]): Boolean =
    request.accepts("application/" + t) || request.accepts("text/" + t)
  def apply(implicit request:Request[AnyContent]): RequestType = {
    if (acc("html") || acc("x-www-form-urlencoded")) HttpRequest
    else if (acc("xml")) XmlRequest
    else if (acc("json")) JsonRequest
    else InvalidRequest
  }
}
case object JsonRequest extends RequestType
case object XmlRequest extends RequestType
case object HttpRequest extends RequestType
case object InvalidRequest extends RequestType
