package controllers

import anorm._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import java.sql.SQLException
import org.joda.time.DateTime
import models.DBQuery
import java.util.Date
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import utils._

object Application extends Controller {
  val queryForm: Form[DBQuery] = Form(
    mapping(
      "sql"  -> nonEmptyText,
      "name" -> text,
      "save" -> boolean
    )(DBQuery.apply)(DBQuery.unapply))

  def index(useHistory: Option[Int]) = Action {
    val qform = useHistory map { id =>
      DBQuery.findHistory(id) map { h =>
        queryForm.fill(h.query)
      } getOrElse queryForm
    } getOrElse queryForm

    Ok(views.html.index(qform, DBQuery.listHistory(30)))
  }

  def createQueryResultCSV(columns: Seq[String], rows: Stream[Seq[Any]]): File = {
    val csv = File.createTempFile("querypad_", ".csv")
    try {
      using(new BufferedWriter(new FileWriter(csv))) { writer =>
        writer.write(columns mkString ",")
        writer.newLine()
        rows foreach { row =>
          writer.write(row mkString ",")
          writer.newLine()
        }
      }
      csv
    } catch {
      case e: Throwable => {
        deleteQuietly(csv)
        throw e
      }
    }
  }

  def executeQuery = Action(parse.urlFormEncoded) { implicit request =>
    queryForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(errors, DBQuery.listHistory(30))),
      query  => {
        val formattedSQL = query.sql.replace(
          "{{today}}", new DateTime().toString("yyyyMMdd"))

        try {
          if (request.body.contains("submit-csv")) {
            executeQueryAsCSV(query.copy(sql=formattedSQL)) { csv =>
              Ok.sendFile(csv, fileName =
                f => "queryresult.%d.csv".format(new Date().getTime / 1000))
            }
          } else {
            val queryResult = DBQuery.execute(query.copy(sql=formattedSQL))
            if (query.save) {
              DBQuery.addHistory(query)
            }
            Ok(views.html.index(queryForm.fill(query),
              DBQuery.listHistory(30),
              Some(queryResult)))
          }
        } catch {
          case e: SQLException => {
            BadRequest(views.html.index(
              queryForm.fill(query).withGlobalError(e.getMessage), DBQuery.listHistory(30)))
          }
        }
      }
    )
  }
  
  def executeQueryAsCSV[T](query: DBQuery)(csvHandler: File => T): T =
    DBQuery.executeWithHandler(query) { (columns, rows) =>
      val csv = createQueryResultCSV(columns, rows)
      try {
        csvHandler(csv)
      } finally {
        deleteQuietly(csv)
      }
    }
  
  def executeHistory(id: Int) = Action { implicit request =>
    request.headers.get("Accept") map {
      _.toLowerCase match {
        case "text/csv" => DBQuery.findHistory(id) map { history =>
          executeQueryAsCSV(history.query) { csv =>
            Ok.sendFile(csv, fileName =
              f => s"${history.name}.%d.csv".format(new Date().getTime / 1000))
          }
        } getOrElse NotFound("history not found.")
        case _ => BadRequest("Unsupported accept format.")
      }
    } getOrElse BadRequest("Accept header required.")
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
