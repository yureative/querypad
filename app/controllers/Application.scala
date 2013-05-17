package controllers

import anorm._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models.DBQuery

object Application extends Controller {

  val queryForm: Form[DBQuery] = Form(
    mapping(
      "sql"  -> nonEmptyText,
      "name" -> text,
      "save" -> boolean
    )(DBQuery.apply)(DBQuery.unapply))

  def index(useHistory: Option[Int]) = Action {
    val qform = useHistory map { id =>
      DBQuery.listHistory(30).find(_.id.get == id) map { h =>
        queryForm.fill(h.query)
      } getOrElse queryForm
    } getOrElse queryForm

    Ok(views.html.index(qform, DBQuery.listHistory(30)))
  }

  def executeQuery = Action { implicit request =>
    queryForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(errors, DBQuery.listHistory(30))),
      query  => {
        val queryResult = DBQuery.execute(query)
        if (query.save) {
          DBQuery.addHistory(query)
        }
        Ok(views.html.index(queryForm.fill(query),
          DBQuery.listHistory(30),
          Some(queryResult)))
      }
    )
  }

  def removeQueryHistory(id: Int) = Action {
    DBQuery.removeHistory(id)
    Ok
  }

  def listQueryHistory(count: Int) = Action {
    NotImplemented
  }

  def javascriptRoutes = Action { implicit request =>
    Ok(Routes.javascriptRouter("jsRouter", Some("jQuery.ajax"))(
      routes.javascript.Application.removeQueryHistory)).as("text/javascript")
  }

}
