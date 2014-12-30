package controllers

import anorm.{NotAssigned, Pk}
import models.Department
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.{Action, Controller}
import views.html
import RequestType._
import DepartmentSerializers._
import play.api.libs.json.Json.toJson

object DepartmentController extends Controller {
  val departmentForm = Form(
    mapping(
      "id" -> ignored(NotAssigned: Pk[Long]),
      "name" -> nonEmptyText(minLength = 5, maxLength = 45),
      "imageSrc" -> text(),
      "code" -> nonEmptyText(maxLength = 7)
    )(Department.apply)(Department.unapply)
  )

  //ok
  def list = Action {
    implicit request => requestType match {
      case JsonRequest => Ok(toJson(Department.list().map(departmentToJson)))
      case XmlRequest => Ok(toXmlString(Department.list()))
      case HttpRequest => Ok(html.department.list(Department.list()))
      case InvalidRequest => BadRequest
    }
  }

  //  def save = Action {
  //    implicit request =>
  //      departmentForm.bindFromRequest.fold(
  //        formWithErrors => BadRequest(html.department.createForm(formWithErrors)),
  //        department => {
  //          Department.insert(department)
  //          Ok(html.operationResult("Good"))
  //        }
  //      )
  //  }

  //request => json, xml, form , ok
  def createFromRequest = Action { implicit request => requestType match {
    case JsonRequest => jsonToDepartment(request.body.asJson) match {
      case None => BadRequest("Bad json")
      case Some(dep) => if (Department.insert(dep))
        Ok("Insert successful")
      else InternalServerError("Insert unsuccessful")
    }
    case XmlRequest => request.body.asXml.flatMap(xmlToDepartment) match {
      case None => BadRequest("Bad xml")
      case Some(dep) => if (Department.insert(dep))
        Ok("Insert successful")
      else InternalServerError("Insert failed")
    }
    case HttpRequest => departmentForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.department.createForm(formWithErrors)),
      department =>
        if (Department.insert(department))
          Ok(html.operationResult("Insert successful"))
        else InternalServerError(html.operationResult("Insert failed")))
    case InvalidRequest => BadRequest("Bad request")
  }

  }

  //all types - ok
  def update(id: Long) = Action {
    implicit request => requestType match {
      case JsonRequest => jsonToDepartment(request.body.asJson) match {
        case None => BadRequest("Bad json")
        case Some(dep) => if (Department.update(id, dep))
          Ok("Edit successful")
        else InternalServerError("Edit unsuccessful")
      }
      case XmlRequest => request.body.asXml.flatMap(xmlToDepartment) match {
        case None => BadRequest("Bad xml")
        case Some(dep) => if (Department.update(id, dep))
          Ok("Edit successful")
        else InternalServerError("Edit failed")
      }
      case HttpRequest => departmentForm.bindFromRequest.fold(
        formWithErrors => BadRequest(html.department.editForm(id, formWithErrors)),
        department =>
          if (Department.update(id, department))
            Ok(html.operationResult("Edit successful"))
          else InternalServerError(html.operationResult("Edit failed")))
      case InvalidRequest => BadRequest("Bad request type")
    }
  }

  //ok
  def create(id: Long, name: String, image: String, code: String) = Action {
    implicit request => {
      requestType match {
        case JsonRequest => Department.insert(new Department(anorm.Id(id), name, image, code));
          Ok(s"created department $name")
        case XmlRequest => Department.insert(new Department(anorm.Id(id), name, image, code));
          Ok(s"created department $name")
        case HttpRequest => Department.insert(new Department(anorm.Id(id), name, image, code));
          Ok(html.operationResult(s"created department $name"))
        case InvalidRequest => BadRequest
      }
    }
  }


  def remove(id: Long) = Action {
    implicit request => {
      val success = Department.delete(id)
      requestType match {
        case JsonRequest | XmlRequest => if (success) Ok else InternalServerError
        case HttpRequest => if (success) Ok(html.operationResult("remove successful"))
        else InternalServerError(html.operationResult("remove failed"))
        case InvalidRequest => BadRequest
      }
    }
  }

  //form - ok
  def createDialog() = Action { implicit request => {
    requestType match {
      case JsonRequest | XmlRequest => BadRequest("Only HTTP requests are supported here")
      case InvalidRequest => BadRequest
      case HttpRequest => try {
        Ok(html.department.createForm(departmentForm))
      } catch {
        case e: Exception => InternalServerError(html.operationResult("failed"))
      }
    }
  }}

  //form - ok
  def editDialog(id: Long) = Action {
    implicit request => requestType match {
      case JsonRequest | XmlRequest => BadRequest
      case HttpRequest => Department.findById(id).map {
        department =>
          Ok(html.department.editForm(id, departmentForm.fill(department)))
      }.getOrElse(NotFound)
      case InvalidRequest => BadRequest
    }
  }
}
