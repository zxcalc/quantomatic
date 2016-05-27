package quanto.util

import java.io.{FileNotFoundException, File}

import quanto.layout.ForceLayout
import quanto.util.json.JsonParseException
import quanto.data.{EqClass, Synth, Theory}
import quanto.util.json.Json


object LayoutSynth extends App {
  val theoryFile = "red_green"
  val theory : Theory = try {
    Theory.fromJson(Json.parse(
      new Json.Input(Theory.getClass.getResourceAsStream(theoryFile + ".qtheory"))))
  } catch {
    case e : JsonParseException =>
      println("Can't open theory file (theories: ghz_w, red_green, or string_ve).")
      sys.exit(1)
  }

  val synthFile = "/Users/alek/qderive-sample/synth/B4_4_3_GHZ.qsynth"
  val synth : Synth = try {
    Synth.fromJson(Json.parse(new File(synthFile)), theory)
  } catch {
    case e : JsonParseException =>
      println("Cannot parse synth file.")
      sys.exit(1)
    case e : FileNotFoundException =>
      println("Cannot locate synth file.")
      sys.exit(1)
  }

  println("laying out synthesis using theory: " + theoryFile + " and file: " + synthFile)

  // note if you parallelise this, each thread should have its *own* ForceLayout object
  val lo = new ForceLayout
  lo.maxIterations = 200

  val total = synth.classes.size
  println("found " + total + " equivalence classes")
  var i = 1

  val synth1 = Synth(synth.classes.map { cls =>
    val num = cls.congs.size + cls.redexes.size + 1
    println("laying out class " + i + "/" + total + "  (" + num + " graphs)"); i+=1
    EqClass(
      rep = lo.layout(cls.rep),
      congs = cls.congs.mapValues(g => lo.layout(g)),
      redexes = cls.redexes.mapValues(g => lo.layout(g)),
      data = cls.data
    )
  })

  val synthFile1 = synthFile.replaceFirst("\\.qsynth$", "") + "-lo.qsynth"
  Synth.toJson(synth1, theory).writeTo(new File(synthFile1))
  println("saved in file: " + synthFile1)

}
