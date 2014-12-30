package controllers

import play.api.mvc._
import views.html
import models.{Department, Student}

object Application extends Controller {

  def index = Action {
      val v = Student.listWithDepartment
      Ok(html.index(v, Department.list()))
  }

}