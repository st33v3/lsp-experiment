package bench

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole
import incpeg.LineBuffer
import incpeg.BufferSource
import incpeg.RowCol
import incpeg.Traversal

val str = "Wiki je skutečné hypertextové medium s nelineárními navigačními strukturami. Každá stránka obvykle obsahuje mnoho odkazů na jiné stránky. Ve větších wiki často existují hierarchické navigační stránky, ale nemusejí se používat. Odkazy jsou vytvářeny užitím specifické syntaxe, takzvaných link pattern (formát odkazu).\n" +
  "Původně používala většina wiki systémů jako link pattern metodu CamelCase, která vytváří odkazy tak, že výraz se napíše s velkými počátečními písmeny jednotlivých slov a vynechají se mezery mezi nimi. Slovo CamelCase je samo o sobě příkladem CamelCase. I když CamelCase vytváří odkazy velmi snadno, vede také k vytváření odkazů ve tvaru odlišném od standardního pravopisu. Wiki založené na CamelCase jsou rychle rozeznatelné podle velkého množství odkazů se jmény jako TableOfContents (TabulkaObsahu) a BeginnerQuestions (OtázkyZačátečníků).\n" +
  "CamelCase má mnoho kritiků a návrháři wiki hledali jiné řešení. První, kdo zavedl tzv. free links (volné odkazy) ve formátu _(volný odkaz), byl systém Cliki. Různé wiki systémy používají jako link pattern hranaté závorky, složené závorky, podtržítka, lomítka nebo jiné znaky. Odkazy mezi odlišnými wiki komunitami jsou možné použitím speciálních link pattern zvaných InterWiki.\n" +
  "Nové stránky se obvykle ve wiki vytvářejí jednoduše přidáním odpovídajícího odkazu na tematicky příbuznou stránku. Pokud odkazovaná stránka neexistuje, je odkaz obvykle zvýrazněn jako nefunkční. Přechod na takový odkaz otevře editační okno, které umožní uživateli napsat text nové stránky. Tento mechanismus zajišťuje, že tzv. „sirotčí“ stránky (stránky, na níž není odjinud odkazováno) vznikají jen zcela výjimečně, a zachovává tak vysokou úroveň hypertextové provázanosti stránek.\n" +
  "Wiki systémy jsou obecně založeny na principu, že je lepší usnadňovat opravy chyb než bránit jejich vzniku. Proto wiki, i když jsou hodně otevřené, zároveň poskytují různé prostředky pro kontrolu platnosti posledních změn obsahu. Nejvýraznější v téměř každém wiki systému je tzv. stránka posledních změn (recent changes page), která zobrazuje seznam určitého počtu posledních změn, nebo všechny změny za určité časové období. Některé wiki umožňují seznam filtrovat tak, aby malé změny nebo změny prováděné skripty (boty) byly vynechány.\n" +
  "Z tohoto záznamu změn jsou ve většině wiki dostupné další dvě funkce: historie oprav, která ukazuje předchozí verze stránky, a vlastnost „rozdíly“ (diff), která zvýrazňuje změny mezi dvěma verzemi. Historie změn umožňuje otevřít v editoru předchozí verzi, uložit ji a tím obnovit původní obsah. Funkce diff (rozdíl) může být použita při rozhodování, zda je změna nezbytná. Pravidelný uživatel wiki si může prohlédnout rozdíly editace uvedené na stránce posledních změn a pokud změny nejsou přijatelné, obnovit předchozí verzi z historie. Tento proces je více nebo méně jednoduchý v závislosti na použitém wiki software.\n" +
  "Pro případ, že by nepřijatelné změny na stránce posledních změn ušly pozornosti, poskytují některé wiki systémy další prostředky kontroly obsahu. Tavi Scotta Moonena zavedl sledované změny, formu interních záložek (bookmarků), která generuje seznam posledních změn pouze v určité skupině stránek. Odkazy na stránky, které mají velikost pod požadovanou hodnotou, mohou být ve wiki zvýrazněny. Tím jsou malé stránky (tzv. pahýly, stubs) rozeznatelné na stránkách, které na ně odkazují.\n" +
  "V extrémních případech mnoho wiki systémů umožňuje stránky některým privilegovaným uživatelům (správcům) některé stránky uzamknout, takže je ostatní uživatelé nemohou editovat. To je však obecně považováno za porušení základní filosofie wiki a proto se to používá jen ve velice omezené míře."

val lb = LineBuffer.fromString(str)
val bs = BufferSource(lb)

trait Sequencer:
  def eof(cs: CharSequence): Boolean
  def next(cs: CharSequence): Char

class LongSequencer extends Sequencer:
  private var index = 0L
  def eof(cs: CharSequence): Boolean = index >= cs.length
  def next(cs: CharSequence): Char = 
    val c = cs.charAt(index.toInt)
    index += 1
    c

class IntSequencer extends Sequencer:
  private var index = 0
  def eof(cs: CharSequence): Boolean = index >= cs.length
  def next(cs: CharSequence): Char = 
    val c = cs.charAt(index)
    index += 1
    c

class BenchTraversal:

  @Benchmark
  def simpleString(b: Blackhole): Unit =
    for (j <- 0 until str.length()) do
      b.consume(str.charAt(j))

  @Benchmark
  def lineBuffer(b: Blackhole): Unit = 
    for (j <- 0 until lb.count) do
      for (k <- 0 until lb.line(j).length()) do
        b.consume(lb.line(j).charAt(k))

  @Benchmark
  def lineBufferTraversal(b: Blackhole): Unit = 
    val tr = bs.createTraversal(RowCol(0, 0))
    while (tr.current != Traversal.EOF) do
      b.consume(tr.current)
      tr.consume()

  @Benchmark
  def longSequencer(b: Blackhole): Unit = 
    val seq: Sequencer = new LongSequencer
    while !seq.eof(str) do
      b.consume(seq.next(str))

  @Benchmark
  def intSequencer(b: Blackhole): Unit = 
    val seq: Sequencer = new IntSequencer
    while !seq.eof(str) do
      b.consume(seq.next(str))
