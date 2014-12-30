package controllers

import models.{Department, Student}
import play.api.libs.json.JsValue
import play.api.libs.json.Json._
import scala.util.control.Exception.allCatch
import scala.xml.NodeSeq

object StudentSerializers {

  def toXmlString(studs:List[Student]): String = studs.map(studentToXml).map(_.toString).foldLeft("")(_ + _)
  implicit def studentToJson(student: Student): JsValue = {
    toJson(
      Map(
        "id" -> toJson(student.id.get),
        "name" -> toJson(student.name)
      )
    )
  }

  def studentToJson(std: (Student, Department)) = {
    val (student, department) = std
    toJson(
      Map(
        "id" -> toJson(student.id.get),
        "name" -> toJson(student.name),
        "department" -> toJson(department.name)
      )
    )
  }

  implicit def jsonToStudent(jso: Option[JsValue]): Option[Student] = jso flatMap { js =>
    val id: Long = (js \ "id").asOpt[Long].getOrElse(-1L)
    ((js \ "name").asOpt[String], (js \ "department").asOpt[Long]) match {
      case (Some(name), Some(dep)) => Some(new Student(anorm.Id(id), name, dep))
      case _ => None
    }
  }

  implicit def studentToXml(student: Student): NodeSeq = <student>
    <id>{student.id}</id>
    <name>{student.name}</name>
    <department>{student.department}</department>
  </student>

  def isLongNumber(s: String): Boolean = (allCatch opt s.toLong).isDefined

  def getLong(s: String): Option[Long] = if (isLongNumber(s)) Some(s.toLong) else None

  implicit def xmlToStudent(xml: NodeSeq): Option[Student] = {
    val id: Long = (xml \\ "student" headOption).
      flatMap(_ \\ "id" headOption).
      flatMap(n => getLong(n.text)).
      getOrElse(-1)
    for (stud <- xml \\ "student" headOption;
         name <- stud \\ "name" headOption;
         dep <- (stud \\ "department" headOption ) if isLongNumber(dep.text.trim()))
    yield new Student(anorm.Id(id), name.text.trim(), dep.text.trim().toLong)

  }


}
