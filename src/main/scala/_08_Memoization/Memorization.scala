package _08_Memoization

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.DurationInt
import scala.util.Try

object Memorization extends IOApp {
  case class Client(name: String, emailAddress: String)

  case class Email(body: String, recipients: List[String])

  trait EmailTemplates {
    def buildEmailForClient(templateId: String, client: Client): Email
  }

  // Long running computation
  def loadEmailTemplates(): IO[EmailTemplates] = {
    IO.sleep(5.seconds) *>
      IO.println("Loading email templates...") *>
      IO.pure(new EmailTemplates {
        override def buildEmailForClient(templateId: String, client: Client): Email = {
          if (templateId == "negative-balance") Email(s"Dear ${client.name}: your account has a negative balance", List(client.emailAddress))
          else
            Email(s"Dear ${client.name}: there is a problem with your account", List(client.emailAddress))
        }
      })
  }


  def processClient(client: Client): IO[Unit] = {
    //IO.raiseError(NegativeBalance)
    IO.println(s"Processing ${client.name}")
  }

  def sendMail(email: Email): IO[Unit] = IO.println(s"Sending email: ${email.body}")

  trait Error extends Throwable

  object NegativeBalance extends Error

  object AccountExpired extends Error

  //w/out memorization
  //  def processClients(clients: List[Client]): IO[Unit] = {
  //    val leftClients = clients
  //      .parTraverse(cl => Try(processClient(cl)).sequence.map(_.toEither).map(res => (cl, res)))
  //      .map(lst => lst.filter(_._2.isLeft))
  //
  //    for {
  //      lc <- leftClients
  //      _ <- lc.parTraverse { case (cl, _) => loadEmailTemplates()
  //        .flatMap(templates => sendMail(templates.buildEmailForClient("", cl))) }
  //    } yield Unit
  //  }

  //w/ memorization
  def processClients(clients: List[Client]): IO[Unit] = {
    loadEmailTemplates().memoize.flatMap { emailTemplatesIO =>
      clients.parTraverse { client =>
        processClient(client).handleErrorWith {
          case NegativeBalance => emailTemplatesIO.flatMap { emailTemplates =>
            val email = emailTemplates.buildEmailForClient("negative-balance", client)
            sendMail(email)
          }
          case AccountExpired => emailTemplatesIO.flatMap { emailTemplates =>
            val email = emailTemplates.buildEmailForClient("account-expired", client)
            sendMail(email)
          }
          case _ => emailTemplatesIO.flatMap { emailTemplates =>
            val email = emailTemplates.buildEmailForClient("generic-error", client)
            sendMail(email)
          }

        }
      }.void
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val clients = List(Client("a", "a@mail.ee"), Client("b", "b@mail.ee"))
    processClients(clients).as(ExitCode.Success)
  }
}
