import bot.JDALauncher
import net.dv8tion.jda.core.JDA
import play.api.Configuration
import play.api.inject.Injector
import play.api.inject.guice.GuiceApplicationBuilder

import scala.concurrent.Future

object CLI {
  val injector: Injector = {
    new GuiceApplicationBuilder()
      .loadConfig { env =>
        Configuration(
          Configuration.load(env).underlying
//          .withoutPath("path")
//          .withoutPath("otherpath")
        )
      }
//      .configure(
//        Map(
//          "block" -> Map(
//            "sub" -> Map(
//              "bool" -> false,
//              "str" -> "0s"
//            )
//          )
//        )
//      )
      .overrides(
//        bind[Class].to(instance),
//        bind[Foo].to(fooImpl)
      )
      .injector()
  }

  val jdaLauncher: JDALauncher = injector.instanceOf[JDALauncher]
  val botReady: Future[JDA] = jdaLauncher.bot.readyPromise.future

  import scala.concurrent.ExecutionContext.Implicits.global

  def main(args: Array[String]): Unit = {
    botReady.foreach { jda =>
      // TODO: console commands.
    }
  }
}
