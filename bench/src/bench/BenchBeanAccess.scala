package bench

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.annotations.Fork
import scala.util.control.NoStackTrace
import java.lang.invoke.MethodHandles
import java.lang.invoke.MethodType
import org.zweistein.tfs.conformers.AsmBeanConformer
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Type
import scala.collection.JavaConverters.mapAsJavaMapConverter


@Fork(1)
@State(Scope.Benchmark)
class BenchBeanAccess:

  private val bean = new Bean()
  private val reflect = bean.getClass.getMethod("getStr")
  private val reflectFast = bean.getClass.getMethod("getStr")
  reflectFast.setAccessible(true)

  val lookup = MethodHandles.lookup();
  val mtype = MethodType.methodType(classOf[String])
  val handle = lookup.findVirtual(classOf[Bean], "getStr", mtype)
  val accessor = new AsmBeanConformer(null).getAccessor(classOf[Bean])
  val fieldAccessor = GenAsm.fieldAccessor(classOf[Bean], "getStr")
  val map = 
    val m = new java.util.HashMap[String, String]()
    m.put("str", "getStr")
    m.put("int", "getInt")
    m
  
  @Benchmark
  def directAccess(b: Blackhole): Unit =
      b.consume(bean.getStr())

  @Benchmark
  def reflectAccess(b: Blackhole): Unit =
    b.consume(reflect.invoke(bean))

  @Benchmark
  def fastReflectAccess(b: Blackhole): Unit =
    b.consume(reflectFast.invoke(bean))

  @Benchmark
  def handleExactAccess(b: Blackhole): Unit =
    b.consume(handle.invokeExact(bean): String)

  @Benchmark
  def handleAccess(b: Blackhole): Unit =
    b.consume(handle.invoke(bean))

  @Benchmark
  def asmAccess(b: Blackhole): Unit =
    b.consume(accessor.get(bean, "str", null))

  @Benchmark
  def asmField(b: Blackhole): Unit =
    b.consume(fieldAccessor.get(bean))

  @Benchmark
  def mapLookup(b: Blackhole): Unit =
    b.consume(map.get("str"))

trait FieldAccess:
  def get(bean: AnyRef): Any

object GenAsm:
  def fieldAccessor(cls: Class[?], name: String): FieldAccess = 
    import org.objectweb.asm.Opcodes.*
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    val csn = cls.getName().replace('.', '_').replace('$', '_')
    val cn = "org/zweistein/tfs/conformers/BA_" + csn; 
    cw.visit(V1_5,
      ACC_SUPER + ACC_PUBLIC, 
      cn,
      null,
      "java/lang/Object",
      Array(classOf[FieldAccess].getName().replace('.', '/')))
    
    var mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    mv.visitCode()
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V")
    mv.visitInsn(RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()

    mv = cw.visitMethod(ACC_PUBLIC, "get", "(Ljava/lang/Object;)Ljava/lang/Object;", null, Array())
    mv.visitCode();
    val bcn = Type.getInternalName(cls)
    mv.visitVarInsn(ALOAD, 1);
    mv.visitTypeInsn(CHECKCAST, bcn)
    val method = classOf[Bean].getMethod(name)
    mv.visitMethodInsn(INVOKEVIRTUAL, bcn, method.getName(), Type.getMethodDescriptor(method))
    mv.visitInsn(ARETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
		
    cw.visitEnd()
    class MyLoader extends ClassLoader(classOf[GenAsm.type].getClassLoader()): 
      def define(bytes: Array[Byte]) = defineClass("org.zweistein.tfs.conformers.BA_" + csn, bytes, 0, bytes.length)
    val cl = new MyLoader()
    val ac = cl.define(cw.toByteArray()).asSubclass(classOf[FieldAccess])
    ac.newInstance()
