import play.api.Configuration
import play.api.inject.Injector
import play.api.inject.guice.GuiceApplicationBuilder

import bot.JDALauncher

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

  def main(args: Array[String]): Unit = {
    // TODO: console commands.
  }
}
