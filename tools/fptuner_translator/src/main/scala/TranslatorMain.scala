
import scala.util.parsing.combinator._
import scala.io.Source
import java.io._

object TranslatorMain extends ExpressionParser  {

  // val result7 = parse(assign, "FPTT_1 _var_expr_1 = (FPTT_1)-3.0 * (FPTT_1)_var_expr_0;")
  // println(result7)

  val fptunerErrors = Map(
   "bspline0" -> List("9e-08", "4.5e-08", "9e-09", "9e-10", "2e-16", "1e-16", "2e-17", "2e-18", "4e-32"),
   "bspline1" -> List("4.5e-07", "2.25e-07", "4.5e-08", "4.5e-09", "8e-16", "4e-16", "8e-17", "8e-18", "2.5e-31"),
   "bspline2" -> List("4e-07", "2e-07", "4e-08", "4e-09", "7.5e-16", "3.75e-16", "7.5e-17", "7.5e-18", "2.5e-31"),
   "doppler" -> List("0.00025", "0.000125", "2.5e-05", "2.5e-06", "4.5e-13", "2.25e-13", "4.5e-14", "4.5e-15", "1.5e-28"),
   "himmilbeau" -> List("0.0015", "0.00075", "0.00015", "1.5e-05", "2.5e-12", "1.25e-12", "2.5e-13", "2.5e-14", "7.5e-28"),
   "invpendulum_out" -> List("2e-05", "1e-05", "2e-06", "2e-07", "4e-14", "2e-14", "4e-15", "4e-16", "1.5e-29"),
   "kepler0" -> List("6e-05", "3e-05", "6e-06", "6e-07", "1.5e-13", "7.5e-14", "1.5e-14", "1.5e-15", "3.5e-29"),
   "kepler1" -> List("0.0003", "0.00015", "3e-05", "3e-06", "5e-13", "2.5e-13", "5e-14", "5e-15", "2e-28"),
   "kepler2" -> List("0.002", "0.001", "0.0002", "2e-05", "2.5e-12", "1.25e-12", "2.5e-13", "2.5e-14", "7.5e-28"),
   "kepler2_double" -> List("0.005", "0.0025", "0.0005", "5e-05", "5.5e-12", "2.75e-12", "5.5e-13", "5.5e-14", "6e-30"),
   "rigidBody1" -> List("0.0002", "0.0001", "2e-05", "2e-06", "3.5e-13", "1.75e-13", "3.5e-14", "3.5e-15", "1.5e-28"),
   "rigidBody2" -> List("0.02", "0.01", "0.002", "0.0002", "4e-11", "2e-11", "4e-12", "4e-13", "1.5e-26"),
   "rigidBody2_triple" -> List("0.1", "0.05", "0.01", "0.001", "1.8e-10", "9e-11", "1.8e-11", "1.8e-12", "2.55e-28"),
   "sine" -> List("6.5e-07", "3.25e-07", "6.5e-08", "6.5e-09", "1.5e-15", "7.5e-16", "1.5e-16", "1.5e-17", "3.5e-31"),
   "sine_triple" -> List("2.5e-06", "1.25e-06", "2.5e-07", "2.5e-08", "4e-15", "2e-15", "4e-16", "4e-17", "4.5e-33"),
   "sqrt" -> List("0.0002", "0.0001", "2e-05", "2e-06", "3.5e-13", "1.75e-13", "3.5e-14", "3.5e-15", "1.5e-28"),
   "train4_state8" -> List("5.5e-06", "2.75e-06", "5.5e-07", "5.5e-08", "1e-14", "5e-15", "1e-15", "1e-16", "3.5e-30"),
   "train4_state9" -> List("5e-06", "2.5e-06", "5e-07", "5e-08", "9e-15", "4.5e-15", "9e-16", "9e-17", "3e-30"),
   "train4_state9_double" -> List("1.7e-05", "8.5e-06", "1.7e-06", "1.7e-07", "3.5e-14", "1.75e-14", "3.5e-15", "3.5e-16", "4.5e-32"),
   "turbine1" -> List("5.5e-05", "2.75e-05", "5.5e-06", "5.5e-07", "1e-13", "5e-14", "1e-14", "1e-15", "3.5e-29"),
   "turbine2" -> List("7.5e-05", "3.75e-05", "7.5e-06", "7.5e-07", "1.5e-13", "7.5e-14", "1.5e-14", "1.5e-15", "5e-29"),
   "turbine3" -> List("4e-05", "2e-05", "4e-06", "4e-07", "7.5e-14", "3.75e-14", "7.5e-15", "7.5e-16", "2.5e-29")
  )

  val daisySuffix = List("_32", "_32_05", "_32_01", "_32_001", "_64", "_64_05", "_64_01", "_64_001", "_dbldbl")

  val fptunerToDaisyMap: Map[String, Map[String, String]] = fptunerErrors.map({
    case (benchmark, errors) =>
      (benchmark -> errors.zip(daisySuffix).toMap)
    })

  println(fptunerToDaisyMap)

  object FloatTpe extends Enumeration {
    type FloatTpe = Value

    val FL_32 = Value("Float")
    val FL_64 = Value("Double")
    val FL_128 = Value("DblDouble")
  }
  import FloatTpe._

  def filesInDirectory(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def getBenchmarkNameAndError(filename: String): (String, String) = {
    // FPTuner file names are orig-filename . error . cpp, e.g. files/bspline-0.4.5e-08.cpp
    //strip .cpp suffix and files/ prefix
    val strippedFilename = filename.replace(".cpp", "").replace("files/", "")

    // bspline-0.4.5e-08
    // the name is the part before the the first dot
    val indexOfDot = strippedFilename.indexOf(".")
    val (benchmarkName, error) = strippedFilename.splitAt(indexOfDot)
    (benchmarkName, error.tail)  //has leading dot
  }
  def main(args: Array[String]) = {

    // val result7 = parse(assign, "FPTT_0 _var_expr_0 = 0.53735720058553;")
    // println(result7)

    var results: Map[String, List[String]] = Map()

    for(file <- filesInDirectory("files/")) {
      val filename = file.getPath()
      println("filename: " + filename)
      val daisyProgram = translate(filename)

      val (benchmarkName, error) = getBenchmarkNameAndError(filename)

      val newRes = if (results.contains(benchmarkName)) results(benchmarkName) :+ daisyProgram
               else List(daisyProgram)

      results = results + (benchmarkName -> newRes)

    }

    for((name, fncs) <- results) {

      val out = new PrintWriter(new File("output/" + name.capitalize + ".scala"))
      out.write("import scala.annotation.strictfp\n\n")
      out.write("//FPTuner generated\n")
      out.write("@strictfp\n")
      out.write(s"object ${name.capitalize} {\n\n")

      for (fnc <- fncs) {
        out.write(fnc)
        out.write("\n\n")
      }

      out.write("}")
      out.close

    }



  }

  def translate(filename: String): String = {
    //val filename = args.head

    // types assigned to type variables FPTT_?
    var typeMap: Map[Int, FloatTpe] = Map.empty

    // assignments representing input variables
    var inputVars: List[Assignment] = List.empty
    // assignments reprenting the function body
    var body: List[Assignment] = List.empty

    // map from variables defined in the program to types
    var varMap: Map[Int, FloatTpe] = Map.empty

    // parse and process the relevant FPTuner program parts
    for (line <- Source.fromFile(filename).getLines()) {
      print(line)
      if (line.startsWith("#define FPTT")) {
        println(" ... processing type def")
        line.replace("#define FPTT_", "").split(" ") match {
          case Array(i, "float") => typeMap = typeMap + (i.toInt -> FL_32)
          case Array(i, "double") => typeMap = typeMap + (i.toInt -> FL_64)
          case Array(i, "__float128") => typeMap = typeMap + (i.toInt -> FL_128)
        }

      } else if (line.trim.startsWith("FPTT")) {

        parse(assign, line) match {
          // input vars
          case Success(ass @ Assignment(TpeVar(tpeIndx), Var(indx), Const(_)), _) =>
            varMap = varMap + (indx -> typeMap(tpeIndx))
            inputVars = inputVars :+ ass

          case Success(ass @ Assignment(TpeVar(tpeIndx), Var(indx), _), _) =>
            varMap = varMap + (indx -> typeMap(tpeIndx))
            body = body :+ ass
        }
        println(" ... processing asignment")

      } else {
        println(" ... skipping")
      }
    }

    //println(typeMap); println(); println(body.mkString("\n"))

    // print Scala code
    val buffer = new StringBuilder()

    val arguments = inputVars.map({
      // the constant is irrelevant
      case Assignment(TpeVar(i), Var(indx), Const(_)) =>
        s"x_$indx: ${typeMap(i)}"
      }).mkString(", ")

    val returnTpe = typeMap(body.last.lhsTpe.index)

    val (benchmarkName, error) = getBenchmarkNameAndError(filename)

    val newFncName = benchmarkName + fptunerToDaisyMap(benchmarkName)(error)

    buffer ++= s"def $newFncName($arguments): $returnTpe = {\n"

    for(assign <- body.init) {
      assign match {
        case Assignment(TpeVar(i), Var(indx), rs) =>
          buffer ++= s"  val x_$indx: ${typeMap(i)} = "  + printScala(rs, varMap, typeMap) + "\n"
      }
    }
    body.last match {
      case Assignment(TpeVar(i), Var(indx), rs) =>
          buffer ++= "  " + printScala(rs, varMap, typeMap) + "\n"
    }
    buffer ++= "}\n"

    println("\n\nTranslated program:")
    println(buffer)
    buffer.toString
  }

  def printScala(e: Expr, varMap: Map[Int, FloatTpe], typeMap: Map[Int, FloatTpe]): String = e match {
    case Plus(lhs, rhs) =>
      printScala(lhs, varMap, typeMap) + " + " + printScala(rhs, varMap, typeMap)
    case Minus(lhs, rhs) =>
      printScala(lhs, varMap, typeMap) + " - " + printScala(rhs, varMap, typeMap)
    case Times(lhs, rhs) =>
      printScala(lhs, varMap, typeMap) + " * " + printScala(rhs, varMap, typeMap)
    case Div(lhs, rhs) =>
      printScala(lhs, varMap, typeMap) + " / " + printScala(rhs, varMap, typeMap)
    case UMinus(t) =>
      "- " + printScala(t, varMap, typeMap)
    case Cast(TpeVar(i), Var(index)) =>
      if (varMap(index) == typeMap(i)) { // no cast necessary, types match
        "x_" + index
      } else {
        typeMap(i) match {
          case FL_32 => s"x_$index.toFloat"
          case FL_64 => s"x_$index.toDouble"
          case FL_128 => s"DblDouble(x_$index)"
        }
      }

    case Cast(TpeVar(i), Const(str)) =>
      val constTpe: FloatTpe = if (str.endsWith("f")) FL_32  //float
                              else FL_64
      if (constTpe == typeMap(i)) {
        str
      } else {
        typeMap(i) match {
          case FL_32 => s"str.toFloat"
          case FL_64 => s"str.toDouble"
          case FL_128 => s"DblDouble($str)"
        }
      }


  }
}

trait ExpressionParser extends RegexParsers with JavaTokenParsers {

  def tpe: Parser[TpeVar] = """FPTT_\d+""".r ^^ {
    x: String => TpeVar(x.replace("FPTT_", "").toInt)
  }

  def variable: Parser[Var] = """_var_expr_\d+""".r ^^ {
    x: String => Var(x.replace("_var_expr_", "").toInt)
  }

  def const: Parser[Const] = floatingPointNumber ^^ { x: String => Const(x.toString) }
  def cast: Parser[Cast] = "(" ~ tpe ~ ")" ~ expr ^^ { case "(" ~ t ~ ")" ~ v => Cast(t,v) }
  def plus: Parser[Plus] = cast ~ "+" ~ cast ^^ { case c1 ~ "+" ~ c2 => Plus(c1, c2) }
  def minus: Parser[Minus] = cast ~ "-" ~ cast ^^ { case c1 ~ "-" ~ c2 => Minus(c1, c2)}
  def times: Parser[Times] = cast ~ "*" ~ cast ^^ { case c1 ~ "*" ~ c2 => Times(c1, c2)}
  def div: Parser[Div] = cast ~ "/" ~ cast ^^ { case c1 ~ "/" ~ c2 => Div(c1, c2)}
  def uminus: Parser[UMinus] = "-" ~ cast ^^ {case "-" ~ t => UMinus(t)}
  def expr: Parser[Expr] = variable | const | plus | minus | times | div | uminus
  def assign: Parser[Assignment] = tpe ~ variable ~ "=" ~ expr ~ ";" ^^ {
    case t ~ v ~ "=" ~ e ~ ";" => Assignment(t, v, e)
  }

}

case class Block(assignments: List[Assignment])
case class Assignment(lhsTpe: TpeVar, lhs: Var, rs: Expr)

abstract class Expr
case class Plus(lhs: Expr, rhs: Expr) extends Expr
case class Minus(lhs: Expr, rhs: Expr) extends Expr
case class Times(lhs: Expr, rhs: Expr) extends Expr
case class Div(lhs: Expr, rhs: Expr) extends Expr
case class UMinus(t: Expr) extends Expr

case class Cast(tpe: TpeVar, e: Expr) extends Expr

// _var_expr_{index}
case class Var(index: Int) extends Expr
case class Const(d: String) extends Expr

case class Defn(tpe: TpeVar, lhs: Expr, c: Const) extends Expr

// FPTT_{index}
case class TpeVar(index: Int)
