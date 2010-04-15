package org.purview.webui.db

import java.sql.DriverManager
import net.liftweb.common.Logger
import net.liftweb.util.Props
import org.squeryl.Session
import org.squeryl.SessionFactory
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.adapters.H2Adapter
import org.squeryl.adapters.MySQLAdapter
import org.squeryl.adapters.PostgreSqlAdapter
import org.squeryl.internals.DatabaseAdapter

/**
 * A Controller that is responsible for managing database connections
 */
object Controller extends Logger {
  /** Initializes a default controller by reading the webapp's props file */
  def init() = {
    //Load database settings
    val dbDriverName = Props.get("db.driver", "org.h2.Driver") //Use H2 by default
    val dbConnectionURL = Props.get("db.url", "jdbc:h2:purview") //Use temporary DB by default
    val dbUser = Props.get("db.user").toOption
    val dbPassword = Props.get("db.password").toOption

    createSessionFactory(dbDriverName, dbConnectionURL, dbUser, dbPassword)
    using(SessionFactory.newSession) {
      try Database.create catch {case _ => warn("Database does already exist")}
    }
  }

  /**
   * Creates a session factory with the specified data. This will make Squeryl
   * use this data per default when spawning database sessions.
   * @param driver JDBC connection driver
   * @param url JDBC connection URL (see your database backend's documentation for syntax details)
   * @param user An optional username to use when connecting
   * @param password An optional password to use when connecting
   * @see connection
   */
  def createSessionFactory(driver: String, url: String, user: Option[String], password: Option[String]) = {
    //Check that the driver exists
    Class.forName(driver)

    //Create session factory that spawns sessions automatically
    SessionFactory.concreteFactory = Some(() => {
      Session.create(connection(url, user, password), adapterFor(driver))
    })
  }

  /**
   * Creates a JDBC connection with the specified connection data
   * @param url JDBC connection URL (see your database backend's documentation for syntax details)
   * @param user An optional username to use when connecting
   * @param password An optional password to use when connecting
   * @returns The constructed connection
   */
  def connection(url: String, user: Option[String], password: Option[String]) = (user, password) match {
    case (Some(user), Some(password)) => DriverManager.getConnection(url, user, password)
    case _ => DriverManager.getConnection(url)
  }

  /** Locates a Squeryl adapter for the given driver name */
  val adapterFor: Map[String, DatabaseAdapter] = Map(
    "org.h2.Driver" -> new H2Adapter,
    "org.postgresql.Driver" -> new PostgreSqlAdapter,
    "com.mysql.jdbc.Driver" -> new MySQLAdapter
  )
}
