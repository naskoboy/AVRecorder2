package nasko.avrecorder

import org.mindrot.jbcrypt.BCrypt
import spray.routing.authentication.{UserPass, BasicAuth}
import spray.routing.directives.AuthMagnet
import scala.concurrent.{ExecutionContext, Future}

// https://www.bcrypt-generator.com/
// http://www.tecnoguru.com/blog/2014/07/07/implementing-http-basic-authentication-with-spray/
// http://blog.knoldus.com/2015/01/14/spray-authenticate-directive-a-decoupled-way-to-authenticate-your-api/

class AuthInfo(val user: String, val permissions: Set[String]) {
  def hasPermission(permission: String) = permissions.exists(_ == permission)
}

trait Authenticator {
  def basicUserAuthenticator(implicit ec: ExecutionContext): AuthMagnet[AuthInfo] = {
    def validateUser(userPass: Option[UserPass]): Option[AuthInfo] = {
      import collection.JavaConversions._
      userPass.flatMap{ up =>
        val hashedPassword = utils.config.getString(s"users.${up.user}.credentials")
        if (BCrypt.checkpw(up.pass, hashedPassword)) Some(new AuthInfo(up.user, utils.config.getStringList(s"users.${up.user}.permissions").toSet))
        else None
      }
    }

    val authenticator = (userPass: Option[UserPass]) => Future { validateUser(userPass) }

    BasicAuth(authenticator, realm = "Private API")
  }
}
