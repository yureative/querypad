package models

import anorm._
import play.api.Logger
import play.api.db._
import play.api.Play.current
import org.joda.time.DateTime
import java.util.Date

case class DBQuery(sql: String, name: String = "", save: Boolean = false)
case class DBQueryResult(columns: List[String], rows: List[List[Any]])
case class DBQueryHistory(id: Pk[Int], name: String, query: DBQuery, updatedAt: DateTime)

object DBQuery {
  val QueryHistoryTable = "query_history"

  def execute(query: DBQuery): DBQueryResult = {
    DB.withConnection("ext") { implicit conn =>
      val rows = SQL(query.sql)()
      val columns = rows.head.metaData.ms map { mdItem =>
        mdItem.column.alias getOrElse mdItem.column.qualified
      }
      DBQueryResult(columns, rows.map(_.asList).toList)
    }
  }

  def addHistory(query: DBQuery) =
    DB.withConnection { implicit conn =>
      SQL(s"""INSERT INTO $QueryHistoryTable VALUES(
        nextval('query_history_seq'), {name}, {sql}, current_timestamp)""")
        .on("name" -> query.name, "sql" -> query.sql).executeInsert()
    }

  def listHistory(limit: Int): List[DBQueryHistory] = {
    DB.withConnection { implicit conn =>
      SQL(s"""SELECT * FROM $QueryHistoryTable
        ORDER BY updated_at DESC LIMIT {limit}""")
          .on("limit" -> limit)() map { row =>
            DBQueryHistory(row[Pk[Int]]("id"), row[String]("name"),
              DBQuery(row[String]("sql")), new DateTime(row[Date]("updated_at")))
          } toList
    }
  }

  def removeHistory(id: Int) {
    DB.withConnection { implicit conn =>
      SQL(s"""DELETE FROM $QueryHistoryTable WHERE id = {id}""")
        .on("id" -> id).execute()
    }
  }
}
