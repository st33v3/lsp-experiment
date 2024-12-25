import mill._, scalalib._
import $ivy.`com.lihaoyi::mill-contrib-jmh:`
import contrib.jmh.JmhModule
import coursier.ivy.IvyRepository

object root extends RootModule with ScalaModule {
  def scalaVersion = "3.6.2"
  def scalacOptions = Seq("-Yexplicit-nulls", "-deprecation")
  
  //def repositoriesTask = T.task { Seq(MavenRepository("https://artifactory.etn")) ++ super.repositoriesTask() }
  
  override def repositoriesTask = T.task { 
    //Seq(IvyRepository.fromPattern(Pattern.default))
    val repo = IvyRepository.parse("http://artifactory.etn/twinstone/[organization]/[module]/[revision]/[artifact](-[revision])(-[classifier]).[ext]").fold(l => throw new Exception(l), identity)
    super.repositoriesTask() ++ Seq(repo)
  }
  
  def ivyDeps = Agg(
    ivy"dev.zio::zio-json:0.6.2",
    ivy"org.zweistein:wbeans-tfs:3.0.0",
    ivy"org.zweistein:wbeans-tfs:3.0.0",
    ivy"org.graalvm.polyglot:polyglot:24.1.0",
    ivy"org.graalvm.polyglot:js-community:24.1.0",
    ivy"org.parboiled::parboiled:2.5.1",
  )
  
  object test extends ScalaTests with TestModule.Utest {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.8.3")
  }

  object bench extends ScalaModule with JmhModule {
    def jmhCoreVersion = "1.35"
    def scalaVersion = root.scalaVersion
    override def moduleDeps = Seq(root)
    override def repositoriesTask = root.repositoriesTask
  } 

}

