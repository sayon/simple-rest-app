package controllers

import models.{Department, Student}
import play.api.libs.json.JsValue
import play.api.libs.json.Json._

import scala.util.control.Exception._
import scala.xml.NodeSeq

object DepartmentSerializers {
  def toXmlString(deps:List[Department]): String = deps.map(departmentToXml).map(_.toString).foldLeft("")(_ + _)
  implicit def departmentToJson(dep: Department): JsValue = {
    toJson(
      Map(
        "id" -> toJson(dep.id.get),
        "img" -> toJson(dep.imageSrc),
        "code" -> toJson(dep.code),
        "name" -> toJson(dep.name)
      )
    )
  }

  implicit def jsonToDepartment(jso: Option[JsValue]): Option[Department] = jso flatMap { js =>
    val id: Long = (js \ "id").asOpt[Long].getOrElse(-1L)
    val img: String = (js \ "img").asOpt[String].getOrElse("")
    for (name <- (js \ "name").asOpt[String];
         code <- (js \ "code").asOpt[String]) yield new Department(anorm.Id(id), name, img, code)
  }

  implicit def departmentToXml(dep: Department): NodeSeq = <department>
    <id>
      {dep.id.get}
    </id>
    <img>
      {dep.imageSrc}
    </img>
    <code>
      {dep.code}
    </code>
    <name>
      {dep.name}
    </name>
  </department>

  def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined

  def getLong(s: String): Option[Long] = if (isLongNumber(s)) Some(s.toLong) else None

  implicit def xmlToDepartment(xml: NodeSeq): Option[Department] = {
    val dep = xml \\ "department" headOption
    val id: Long = dep flatMap(_ \\ "id" headOption) flatMap(n => getLong(n.text.trim)) getOrElse(-1)
    val img: String = dep.flatMap(_ \\ "img" headOption).map(_ text).getOrElse("")
    for (dep <- dep;
         name <- dep \\ "name" headOption;
         code <- (dep \\ "code" headOption))
    yield new Department(anorm.Id(id), name.text.trim(), img, code.text.trim())

  }
}
