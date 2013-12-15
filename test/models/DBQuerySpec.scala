import org.joda.time.DateTime

import anorm.Id
import models.DBQuery
import models.DBQueryHistory
import play.api.db.DB
import play.api.test.FakeApplication
import play.api.test.PlaySpecification
import play.api.test.WithApplication

object DBQuerySpec extends PlaySpecification {

  "DBQuery#findHistory" should {
    "return None if history not found" in new WithMemoryDB {
      DBQuery.findHistory(0) === None
    }
    
    "return DBQueryHistory if history found" in new WithMemoryDB {
      DB.withConnection { conn =>
        conn.createStatement.execute(
          "INSERT INTO query_history VALUES (1, 'foo', 'SELECT 1;', '1970-01-01')")
      }
      
      DBQuery.findHistory(1) === Some(
        DBQueryHistory(Id(1), "foo", DBQuery("SELECT 1;"), DateTime.parse("1970-01-01")))
    }
  }

  abstract class WithMemoryDB extends WithApplication(
    FakeApplication(additionalConfiguration = inMemoryDatabase()))
}