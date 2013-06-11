package controllers

import anorm._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import java.sql.SQLException
import org.joda.time.DateTime
import models.DBQuery
import models.DBQueryHistory
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
  
  val updateHistoryForm: Form[DBQueryHistory] = Form(
    mapping(
      "name" -> nonEmptyText(maxLength=30),
      "sql"  -> nonEmptyText
    ) { (name, sql) =>
      DBQueryHistory(NotAssigned, name, DBQuery(sql), new DateTime)
    } { history =>
      Some(history.name, history.query.sql)
    }
  )

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
    val csvAccept = Accepting("text/csv") 
    request match {
      case csvAccept() => 
        DBQuery.findHistory(id) map { history =>
          executeQueryAsCSV(history.query) { csv =>
            Ok.sendFile(csv, fileName =
              f => s"${history.name}.%d.csv".format(new Date().getTime / 1000))
          }
        } getOrElse NotFound("history not found.")
      case _ => BadRequest("unsupported accepts.")
    }
  }
  
  def showQueryHistory(id: Int) = Action { request =>
    import Json.toJson
    request match {
      case Accepts.Json() =>
        DBQuery.findHistory(id) map { history =>
          Ok(toJson(Map(
            "id"   -> toJson(history.id.get),
            "name" -> toJson(history.name),
	        "sql"  -> toJson(history.query.sql))))
	    } getOrElse NotFound("history not found.")
	  case _ => BadRequest(s"unsupported accepts.")
    } 
  }
  
  def updateQueryHistory(id: Int) = Action { implicit request =>
    Logger.debug("hoge")
    updateHistoryForm.bindFromRequest.fold(
      errors => {
        Logger.warn(errors.toString)
        BadRequest("Invalid value.")
      },
      history => {       
        DBQuery.updateHistory(history.copy(id=Id(id)))
        Ok
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
      routes.javascript.Application.showQueryHistory,
      routes.javascript.Application.updateQueryHistory,
      routes.javascript.Application.removeQueryHistory)).as("text/javascript")
  }

}
