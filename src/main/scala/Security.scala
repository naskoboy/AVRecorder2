package nasko.avrecorder

import com.github.t3hnar.bcrypt._
import org.mindrot.jbcrypt.BCrypt
import spray.routing.authentication.{UserPass, BasicAuth}
import spray.routing.directives.AuthMagnet
import scala.concurrent.{ExecutionContext, Future}

// https://www.bcrypt-generator.com/
// http://www.tecnoguru.com/blog/2014/07/07/implementing-http-basic-authentication-with-spray/
// http://blog.knoldus.com/2015/01/14/spray-authenticate-directive-a-decoupled-way-to-authenticate-your-api/

case class ApiUser(login: String, hashedPassword: Option[String] = None) {
  def withPassword(password: String) = copy (hashedPassword = Some(password.bcrypt(generateSalt)))
  def passwordMatches(password: String): Boolean = hashedPassword.exists(hp => BCrypt.checkpw(password, hp))
}

class AuthInfo(val user: ApiUser, permissions: Set[String]) {
  def hasPermission(permission: String) = {
    permissions.exists(_ == permission)
  }
}

object Repository {
  object apiUsers {
    def apply(login: String) = {
      try {
        val credentials: String = utils.config.getString(s"users.$login.credentials")
        Some(ApiUser(login, Some(credentials)))
      } catch { case e: com.typesafe.config.ConfigException => None }

    }
  }
}

trait Authenticator {
  def basicUserAuthenticator(implicit ec: ExecutionContext): AuthMagnet[AuthInfo] = {
    def validateUser(userPass: Option[UserPass]): Option[AuthInfo] = {
      import collection.JavaConversions._
      for {
        p <- userPass
        user <- Repository.apiUsers(p.user)
        if user.passwordMatches(p.pass)
      } yield new AuthInfo(user, utils.config.getStringList(s"users.${user.login}.permissions").toSet)
    }

    def authenticator(userPass: Option[UserPass]): Future[Option[AuthInfo]] = Future { validateUser(userPass) }

    BasicAuth(authenticator _, realm = "Private API")
  }
}
