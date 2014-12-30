package models

import anorm._
import anorm.SqlParser._

import play.api.db._
import play.api.Play.current

case class Student(
                    id: Pk[Long] = NotAssigned,
                    name: String,
                    department: Long
                    )

case class Department(
                       id: Pk[Long] = NotAssigned,
                       name: String,
                       imageSrc: String,
                       code: String
                       )

object Student {

  val simple = {
    get[Pk[Long]]("student.id") ~
      get[String]("student.name") ~
      get[Long]("student.department") map {
      case id ~ name ~ department => Student(id, name, department)
    }
  }

  val withDepartment = Student.simple ~ Department.simple map {
    case computer ~ department => (computer, department)
  }

  def findById(id: Long): Option[Student] = {
    DB.withConnection(implicit connection =>
      SQL("select * from student where id = {id}")
        .on('id -> id)
        .as(Student.simple.singleOpt)
    )
  }

  def list() = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            select * from student
            order by student.name
          """).as(Student.simple *)
    }
  }

  def listWithDepartment: Seq[(Student, Department)] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            select * from student
            left join department on student.department = department.id
          """).as(Student.withDepartment *)
    }
  }


  def update(id: Long, student: Student) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            update student
            set name = {name}, department = {department}
            where id = {id}
          """)
          .on(
            'id -> id,
            'name -> student.name,
            'department -> student.department
          ).executeUpdate()
    }
  }

  def insert(student: Student) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            insert into student (name, department)
            values ({name}, {department})
          """)
          .on(
            'name -> student.name,
            'department -> student.department
          ).executeUpdate() != 0
    }
  }

  def delete(id: Long) = {
    DB.withConnection {
      implicit connection =>
        SQL("delete from student where id = {id}").on('id -> id).executeUpdate() != 0
    }
  }

}

object Department {

  val simple = get[Pk[Long]]("department.id") ~
    get[String]("department.name") ~
    get[String]("department.imageSrc") ~
    get[String]("department.code") map {
    case id ~ name ~ imageSrc ~ code => Department(id, name, imageSrc, code)
  }

  def findById(id: Long): Option[Department] = {
    DB.withConnection(implicit connection =>
      SQL("select * from department where id = {id}")
        .on('id -> id)
        .as(Department.simple.singleOpt)
    )
  }

  def list() = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            select * from department
            order by name
          """).as(Department.simple *)
    }
  }

  def options() = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            select * from department
            order by name
          """)
          .as(Department.simple *)
          .map(c => c.id.toString -> c.name)
    }
  }

  def update(id: Long, department: Department) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            update department
            set name = {name}, imageSrc = {imageSrc}, code = {code}
            where id = {id}
          """)
          .on(
            'id -> id,
            'name -> department.name,
            'imageSrc -> department.imageSrc,
            'code -> department.code
          ).executeUpdate() != 0
    }
  }

  def insert(department: Department) = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            insert into department (name, imageSrc, code)
            values ({name}, {imageSrc}, {code})
          """)
          .on(
            'name -> department.name,
            'imageSrc -> department.imageSrc,
            'code -> department.code
          ).executeUpdate() != 0
    }
  }

  def delete(id: Long) = {
    DB.withConnection {
      implicit connection =>
        try {
          SQL("delete from student where department = {id}").on('id -> id).executeUpdate()
          SQL("delete from department where id = {id}").on('id -> id).executeUpdate()
          true;
        }
        catch { case e:Throwable => false }
    }
  }

}