/**
  * Created by fredy on 9/16/17.
  */
import java.net.URLEncoder

import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.methods.{GetChatAdministrators, ParseMode, RestrictChatMember, UnbanChatMember}
import info.mukel.telegrambot4s.models.{MessageEntityType, User}

import scala.collection.mutable
import scala.concurrent.Future
import scala.io.Source

/**
  * Let me Google that for you!
  */
object KulgramBot extends TelegramBot with Polling with Commands {
  lazy val token = scala.util.Properties
    .envOrNone("BOT_TOKEN")
    .getOrElse(Source.fromFile("bot.token").getLines().mkString)

  var adminList = Seq[User]()
  var host: User = new User(-1, false, "Null")
  var askQ = mutable.Queue[String]()
  var bannedList = Seq[User]()

  onCommand('help) { implicit msg =>
    reply(
      s"""List Kulgram Bot Command
         |
         |/help - list commands
         |
         |/lmgtfy - lmgtfy
         |
         |/makeKulgram - new kulgram
         |
         |/setHost - set kulgram host
         |
         |/getHost - get kulgram host
       """.stripMargin,
      parseMode = Option(ParseMode.Markdown)
    )
  }

  onCommand('makeKulgram) { implicit msg =>
    withArgs { args =>
      adminList = Seq[User]()
      host = User(-1, false, "Null")
      val chatAdmin = request(GetChatAdministrators(msg.source))
      val res = chatAdmin.flatMap(chatMembers => Future(chatMembers.map(_.user)))
/*      for {
        c <- chatAdmin
      } yield {
        c.map(_.user)
      }*/
      res.foreach(u => u.foreach(a => adminList = adminList.:+(a)))
      reply("Started")
    }
  }

  onCommand('setHost) { implicit msg =>
    withArgs { args =>
      val username = args.head.replace("@", "")
      val user = adminList.find(u => u.username.get == username)
      if (user.isDefined) {
        reply("Host set to " + username)
        host = user.get
      } else {
        reply("Host must be admin")
      }
    }
  }

  onCommand('getHost) { implicit msg =>
    if (host.id == -1) {
      reply("No host")
    } else {
      reply("Host is @" + host.username.get)
    }
  }

  onCommand('showadmin) { implicit msg =>
    if (adminList.nonEmpty) {
      adminList.foreach(u => reply("@" + u.username.get))
    } else {
      reply("No admin yet")
    }
  }

  onCommand('ask) { implicit msg =>
    withArgs { args =>
      if (askQ.count(_ == msg.from.get.username.get) == 0) {
        askQ += msg.from.get.username.get
        reply(msg.from.get.username.get + " added to KUEUE")
      }
    }
  }

  onMessage { implicit msg =>
    msg.leftChatMember match {
      case None => None
      case Some(u) => reply("Bye, " + u.firstName)
    }
    msg.newChatMembers match {
      case None => None
      case Some(arr) => {
        arr.foreach(println)
      }
    }
  }

  onCommand('ban) { implicit msg =>
    msg.entities match {
      case None => None
      case Some(e) =>
        e.foreach(m =>
          if (m.`type` == MessageEntityType.Mention) {
            m.user match {
              case None => None
              case Some(u) =>
                println(u)
                bannedList = bannedList.:+(u)
                request(RestrictChatMember(msg.source, u.id))
                reply(u.firstName + " is banned")
            }
          }
        )
    }
  }

  onCommand('unban) { implicit msg =>
    withArgs { args =>
      val username = args.head.replace("@", "")
      val user = bannedList.find(u => u.username.get == username)
      if (user.isDefined) {
        request(UnbanChatMember(msg.source, user.get.id))
        reply(username + " is unbanned")
      } else {
        reply("#trial Someone to be unbanned must be admin")
      }
    }
  }

  onCommand('next) { implicit msg =>
    if (host.id == -1) {
      reply("No host set")
    } else {
      if (msg.from.get.username.get == host.username.get) {
        if (askQ.nonEmpty) {
          val next = askQ.dequeue()
          reply("Your question : " + next)
        } else {
          reply("No quest here")
        }
      }
    }
  }

  onCommand('listAsk) { implicit msg =>
    if (askQ.nonEmpty) {
      reply(askQ.mkString("\n"))
    }
  }

  onCommand("/lmgtfy") { implicit msg =>
    withArgs { args =>
      reply(
        "http://lmgtfy.com/?q=" + URLEncoder.encode(args.mkString(" "), "UTF-8"),
        disableNotification = Option(true)
      )
    }
  }

  /*onCommand('pic) { implicit msg =>
    SendPhoto(msg.source, InputFile(Paths.get("/home/fredy/Download/sven.jpg")))
  }*/
}

object Main{
  def main(args: Array[String]): Unit = {
    println("starting");
    KulgramBot.run()
    println("runnning");
  }
  def abeng1: Unit = {
    def generate(v: Int): Int = {
      def decom(x: Int, res: Int): Int = {
        if (x == 0) res
        else decom(x / 10, res + (x % 10))
      }
      v + decom(v, 0)
    }
    val l = 1 to 5000
    val r = l.map(generate)
    val res = l.filterNot(r.toSet)
    println(res.sum)
  }
  def abeng2: Unit = {
    def grid8 = Array(
      0, 1, 0, 1, 0, 1, 0, 1,
      0, 0, 0, 0, 0, 1, 0, 0,
      1, 0, 1, 0, 0, 1, 0, 1,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 1, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 1, 0, 1,
      0, 1, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 1, 0, 1, 0).toList
    def grid4 = Array(
      1, 1, 1, 0,
      0, 0, 0, 0,
      0, 1, 0, 0,
      1, 1, 1, 0
    ).toList
    def findWay(grid: List[Int], N: Int): Tuple2[Int, Int] = {
      val list0 = grid.zipWithIndex.filter(_._1 == 0).map(_._2)
      def iterRow(r: Int, c: Int, d: Int, n: Int, res: List[Int]): List[Int] = {
        if (r >= 0 && r < n && c >= 0 && c < n) {
          if (grid(r * N + c) == 0) {
            iterRow(r + d, c, d, n, res.:+(r * N + c))
          } else {
            res
          }
        } else {
          res
        }
      }
      def iterCol(r: Int, c: Int, d: Int, n: Int, res: List[Int]): List[Int] = {
        if (r >= 0 && r < n && c >= 0 && c < n) {
          if (grid(r * N + c) == 0) {
            iterCol(r, c + d, d, n, res.:+(r * N + c))
          } else {
            res
          }
        } else {
          res
        }
      }
      def getRow(index:Int, n: Int) = {
        iterRow(index / n, index % n, 1, n, List[Int]()) ::: iterRow(index / n, index % n, -1, n, List[Int]())
      }
      def getCol(index:Int, n: Int) = {
        iterCol(index / n, index % n, 1, n, List[Int]()) ::: iterCol(index / n, index % n, -1, n, List[Int]())
      }
      def putNext(indexEmpty: List[Int], curN: Int): Tuple2[Int, Int]= {
        if (indexEmpty.isEmpty) {
          return (curN, 1)
        }
        val now = indexEmpty.head
        val del = getRow(now, N) ::: getCol(now, N)
        //del.toSet.foreach(println)
        val res2 = putNext(indexEmpty.tail, curN)
        val res = putNext(indexEmpty.filterNot(del.toSet), curN + 1)
        if (res._1 > res2._1) {
          res
        } else if (res._1 < res2._1) {
          res2
        } else {
          (res._1, res._2 + res2._2)
        }
      }
      putNext(list0, 0)
    }
    println(findWay(grid8, 8))
  }
}
