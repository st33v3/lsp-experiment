package bench

import scala.collection.mutable
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.annotations.Fork
import scala.util.control.NoStackTrace
import org.zweistein.tfs.TfsBootstrap
import org.zweistein.tfs.TemplateBroker
import org.zweistein.tfs.EvaluationEnvironment
import org.zweistein.tfs.Template
import org.zweistein.tfs.parser.ParserExtensions
import org.zweistein.tfs.funs.StandardFunExtension
import org.zweistein.tfs.std.AbsynthParser
import org.zweistein.tfs.brokers.SettableDefaultTemplate
import java.io.ByteArrayInputStream
import java.util.Locale
import java.io.StringWriter
import java.util.TimeZone
import org.slf4j.LoggerFactory
import java.util.{List => JList}
import java.util.{Map => JMap}
import java.util.ArrayList
import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import scala.annotation.retains
import org.graalvm.polyglot.*
import org.graalvm.polyglot.proxy.*

class Person(private val name: String, private val age: Int):
  def getName(): String = name
  def getAge(): Int = age

@Fork(1)
class BenchTfs:

  @Benchmark
  def simpleTfs(b: Blackhole): Unit =
    b.consume(ProcessTfs.process("hello", Map.empty))

  @Benchmark
  def tableObjTfs(b: Blackhole): String =
    val ret = ProcessTfs.process("table", BenchTfsData.dataObj)
    b.consume(ret)
    ret

  @Benchmark
  def tableMapTfs(b: Blackhole): String =
    val ret = ProcessTfs.process("table", BenchTfsData.dataMap)
    b.consume(ret)
    ret

  @Benchmark
  def tableObjJs(b: Blackhole): String =
    val ret = ProcessJs.processObj(BenchTfsData.dataObj)
    b.consume(ret)
    ret

  @Benchmark
  def tableMapJs(b: Blackhole): String =
    val ret = ProcessJs.processMap(BenchTfsData.dataMap)
    b.consume(ret)
    ret

  @Benchmark
  def simpleNative(b: Blackhole): String =
    val w = new StringWriter()
    w.write("Hello world")
    val ret = w.toString()
    b.consume(ret)
    ret

  @Benchmark
  def tableObjNative(b: Blackhole): String =
    val w = new StringWriter()
    for 
      p <- BenchTfsData.dataObj.asScala
    do
      w.write("<tr>")
      w.write(s"<td>${p.getName()}</td><td>${p.getAge()}${if p.getAge() < 50 then " (child)" else ""}</td>")
      w.write("</tr>\n")
    val ret = w.toString()
    b.consume(ret)
    ret

  @Benchmark
  def tableObjNativeNoConcat(b: Blackhole): String =
    val w = new StringWriter()
    for 
      p <- BenchTfsData.dataObj.asScala
    do
      w.write("<tr>")
      w.write("<td>")
      w.write(p.getName())
      w.write("</td><td>")
      w.write(String.valueOf(p.getAge()))
      if p.getAge() < 50 then w.write(" (child)")
      w.write("</td></tr>\n")
    val ret = w.toString()
    b.consume(ret)
    ret

  @Benchmark
  def tableMapNative(b: Blackhole): String =
    val w = new StringWriter()
    for 
      m <- BenchTfsData.dataMap.asScala
    do
      w.write("<tr>")
      w.write(s"<td>${m.get("name")}</td><td>${m.get("age")}${if m.get("age").asInstanceOf[Integer] < 50 then " (child)" else ""}</td>")
      w.write("</tr>\n")
    val ret = w.toString()
    b.consume(ret)
    ret

  @Benchmark
  def tableMapNativeNoConcat(b: Blackhole): String =
    val w = new StringWriter()
    for 
      m <- BenchTfsData.dataMap.asScala
    do
      w.write("<tr>")
      w.write("<td>")
      w.write(String.valueOf(m.get("name")))
      w.write("</td><td>")
      w.write(String.valueOf(m.get("age")))
      if m.get("age").asInstanceOf[Integer] < 50 then w.write(" (child)")
      w.write("</td></tr>\n")
    val ret = w.toString()
    b.consume(ret)
    ret

object BenchTfsData:
    def genData(n: Int): JList[Person] = 
      val ret = new ArrayList[Person]()
      (1 `to` n).foreach(i => ret.add(Person(s"Name$i", i)))
      ret

    def genDataMap(n: Int): JList[JMap[String, Object]] = 
      val ret = new ArrayList[JMap[String, Object]]()
      (1 `to` n).foreach: i => 
        val m = new java.util.HashMap[String, Object]()
        m.put("name", s"Name$i")
        m.put("age", Integer.valueOf(i))
        ret.add(m)
      ret

    val dataObj = genData(100)
    val dataMap = genDataMap(100)

object ProcessTfs:
  object broker extends TemplateBroker:
    private val age = System.currentTimeMillis()
    val exts = new ParserExtensions();
    exts.addExtension(new StandardFunExtension());
    val parser = new AbsynthParser("windows-1250", exts);
    val templates = mutable.Map[String, Template]()
    
    def parse(env: EvaluationEnvironment, id: String, text: String): Template = 
      if (templates.contains(id)) return templates(id)
      val tpl = new SettableDefaultTemplate();
      val data = text.getBytes("windows-1250");
      parser.parse(tpl, new ByteArrayInputStream(data));
      tpl.setAge(age)
      tpl.setId(id)
      tpl.setLocale(Locale.ROOT);
      tpl.setResourceBundle(null);
      tpl.setBroker(this);
      templates(id) = tpl
      tpl
      
    override def obtainTemplate(env: EvaluationEnvironment, path: String): Template = 
      path match
        case "hello" => parse(env, path, "Hello world")
        case "table" => parse(env, path, """
          |{part line}
          |      <td>{l.name}</td>
          |      <td>{l.age}{if l.age lt 50} (child){/if}</td>
          |{/part}
          |<table>
          |  {while data}
          |    <tr>{_self.line l=_}</tr>
          |  {/while}
          |</table>
        """.stripMargin)
        case _ => throw new Exception("Template not found")
      

    override def resourceAge(env: EvaluationEnvironment, tpl: Template): Long = age

    override def restoreTemplate(env: EvaluationEnvironment, tpl: Template): Unit = ()

  val boot = new TfsBootstrap(broker, LoggerFactory.getLogger(classOf[BenchTfs]), null, null, null)

  def process(id: String, data: Any): String = 
    val dest = new StringWriter()
    val env = boot.createEnvironment(dest, Locale.ROOT, TimeZone.getDefault())
    try {
			env.pushValue("data", data)
			env.evaluate(id)
		} finally {
			env.destroy()
		}
    dest.toString()

object ProcessJs:
  val srcObj = """
    |function processLineObj(line) {
    |  return `<tr><td>${line.getName()}</td><td>${line.getAge()}${line.getAge() < 50 ? " (child)" : ""}</td></tr>\n`;
    |}
    |
    |(function (data) {
    |  var dest = "";
    |  for (var l = 0; l < data.size(); l++) dest += processLineObj(data.get(l));
    |  return dest;
    |})
    |
  """.stripMargin

  val srcMap = """
    |function processLineMap(line) {
    |  return `<tr><td>${line.name}</td><td>${line.age}${line.age < 50 ? " (child)" : ""}</td></tr>\n`;
    |}
    |
    |(function (data) {
    |  var dest = "";
    |  for (var l = 0; l < data.size(); l++) dest += processLineMap(data.get(l));
    |  return dest;
    |})
    |
  """.stripMargin
  val (valueObj, valueMap) = {
    val ctx = Context.newBuilder().allowAllAccess(true).allowHostAccess(true).build()
    (ctx.eval("js", srcObj), ctx.eval("js", srcMap))
  }
  
  def processObj(data: AnyRef): String =
    valueObj.execute(data).asString()

  def processMap(data: AnyRef): String =
    valueMap.execute(data).asString()

object BenchTfs:

  @main
  def main() =
    val x = new BenchTfs()
    println(x.tableObjTfs(new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.")))
    //println(x.tableObjJs(new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.")))

