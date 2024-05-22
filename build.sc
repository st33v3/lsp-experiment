import mill._, scalalib._
import $ivy.`com.lihaoyi::mill-contrib-jmh:`
import contrib.jmh.JmhModule

object foo extends RootModule with ScalaModule {
  def scalaVersion = "3.4.1"
  def scalacOptions = Seq("-Yexplicit-nulls", "-Ysafe-init", "-deprecation")
  def ivyDeps = Agg(
    ivy"dev.zio::zio-json:0.6.2",
  )
  
  object test extends ScalaTests with TestModule.Utest {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.8.3")
  }

  object bench extends ScalaModule with JmhModule {
  def jmhCoreVersion = "1.35"
  def scalaVersion = "3.4.1"
  override def moduleDeps = Seq(foo)
} 

}

