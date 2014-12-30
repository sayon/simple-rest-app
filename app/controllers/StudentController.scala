package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import models.{Department, Student}
import views._
import anorm._
import play.api.libs.json.Json.toJson
import StudentSerializers._
import RequestType.requestType


object StudentController extends Controller {

  val studentForm = Form(
    mapping(
      "id" -> ignored(NotAssigned: Pk[Long]),
      "name" -> nonEmptyText(minLength = 5, maxLength = 45),
      "department" -> longNumber
    )(Student.apply)(Student.unapply)
  )


  def list = Action {
    implicit request => requestType match {
      case JsonRequest => Ok(toJson(Student.list().map(studentToJson)))
      case XmlRequest => Ok(Student.list().map(studentToXml).map(_.toString).foldLeft("")(_ + _))
      case HttpRequest => Ok(html.student.list(Student.list()))
      case _ => BadRequest
    }
  }

  def createDialog = Action { implicit request => requestType match {
    case HttpRequest => Ok(html.student.createForm(studentForm, Department.options()))
    case _ => BadRequest("Only HTML requests are supported here")
  }
  }

  def create = Action {
    implicit request => requestType match {
      case JsonRequest => jsonToStudent(request.body.asJson) match {
        case None => BadRequest("Bad json")
        case Some(stud) => Student.insert(stud); Ok("Inserted student")
      }
      case XmlRequest =>request.body.asXml flatMap xmlToStudent match {
        case None => BadRequest("Bad xml")
        case Some(stud) => Student.insert(stud); Ok("Inserted student")
      }
      case HttpRequest => studentForm.bindFromRequest.fold(
        formWithErrors => BadRequest(html.student.createForm(formWithErrors, Department.options())),
        student => {
          Student.insert(student)
          Ok(html.operationResult(s"Inserted student ${student.name} successfully"))
        })
      case InvalidRequest => BadRequest("Invalid creation query")
    }
  }

  def editDialog(id: Long) = Action { implicit request => requestType match {
    case HttpRequest => Student.findById(id) match {
      case Some(student) => Ok(html.student.editForm(id, studentForm.fill(student), Department.options()))
      case None => NotFound(s"Student with id $id does not exist")
    }
    case _ => BadRequest("Only HTTP requests are supported here")
  }
  }

  def update(id: Long) = Action {
    implicit request => requestType match {
      case JsonRequest => Ok("")
      case XmlRequest => request.body.asXml flatMap xmlToStudent match {
        case None => BadRequest("Bad xml")
        case Some(stud) if stud.id.get == -1 => BadRequest("You need to provide a valid id")
        case Some(stud) => Student.update(stud.id.get, stud); Ok(s"Updated student ${stud .name} successfully")
      }
      case HttpRequest => studentForm.bindFromRequest.fold(
        formWithErrors => BadRequest(html.student.editForm(id, formWithErrors, Department.options())),
        student => {
          Student.update(id, student)
          Ok(html.operationResult(s"Updated student ${student.name} successfully"))
        }
      )
      case InvalidRequest => BadRequest("Bad update request")
    }
  }

  def remove(id: Long) = Action {
    implicit request => {
      try {
        if (Student.delete(id)) {
          val msg = s"Successfully deleted student with id $id"
          if (requestType == HttpRequest) Ok(html.operationResult(msg)) else Ok(msg)
        } else {
          val msg = s"Student with id $id does not exist"
          if (requestType == HttpRequest) NotFound(html.operationResult(msg)) else NotFound(msg)
        }
      }
      catch {
        case e: Throwable => val msg = s"Can't delete student with id $id"
          if (requestType == HttpRequest) InternalServerError(html.operationResult(msg)) else InternalServerError(msg)
      }
    }
  }

  def createStudent(name: String, dep: Long) = Action {
    implicit request =>
      try {
        Student.insert(new Student(NotAssigned, name, dep))
        requestType match {
          case InvalidRequest => BadRequest("Bad attempt to insert")
          case _ => Ok(html.operationResult(s"Successfully inserted student $name"))
        }
      }
      catch {
        case e: Throwable => println(e.getMessage)
          Status(INTERNAL_SERVER_ERROR)("Can't execute insert")
      }
  }

  final val notFoundMsg = "Student not found"

  def show(id: Long) = Action {
    implicit request => (requestType(request), Student.findById(id)) match {
      case (JsonRequest, None) => NotFound(notFoundMsg)
      case (HttpRequest, None) => NotFound(html.operationResult(notFoundMsg))
      case (XmlRequest, None) => NotFound(notFoundMsg)
      case (JsonRequest, Some(stud)) => Ok(studentToJson(stud))
      case (HttpRequest, Some(stud)) => Ok(html.student.showForm(stud))
      case (XmlRequest, Some(stud)) => Ok(studentToXml(stud))
      case _ => BadRequest("Can't show student with id " + id)
    }
  }
}
