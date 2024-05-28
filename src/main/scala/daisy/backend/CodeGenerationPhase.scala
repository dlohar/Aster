// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package backend

import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer

import utils.CodePrinter
import lang.Trees._
import tools.FinitePrecision
import FinitePrecision._
import tools.FinitePrecision._
import lang.Types._
import lang.TreeOps._

import lang.Extractors.ArithOperator
import tools.{Interval, Rational}
import lang.Identifiers.Identifier
import lang.TreeOps.{getLastExpression}


object CodeGenerationPhase extends DaisyPhase {
  override val name = "Code Generation"
  override val shortName = "codegen"
  override val description = "Generates (executable) code."
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption(
      "lang",
      Set("C", "Scala", "FPCore"),
      "Scala",
      "Language for which to generate code"),
    FlagOption(
      "genMain",
      "Whether to generate a main method to run the code."),
    FlagOption(
      "apfixed",
      "Print C code for Xilinx with ap_fixed data type"),
    FlagOption(
      "codegenNNOriginal",
      "(temporary flag) to generate code for the original program with loops"
    )
  )

  implicit val debugSection = DebugSectionBackend

  var reporter: Reporter = null

  def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val uniformPrecisions = ctx.uniformPrecisions

    reporter = ctx.reporter

    val mixedTuning = ctx.hasFlag("mixed-tuning")
    val fixedPrecision = ctx.fixedPoint
    val apfixedFormat = ctx.hasFlag("apfixed")
    val choosePrecision = ctx.option[String]("choosePrecision")

    val newDefs = transformConsideredFunctions(ctx, prg){ fnc =>


      // if the function returns tuples, we need to re-tuple them
      val _body = fnc.returnType match {
        case TupleType(_) => reTuple(fnc.body.get, ctx.resultTupleIds(fnc.id))
        case _ => fnc.body.get
      }

      // different scenarios:

      // CASE: mixed-tuning for fixed-point arithmetic, necessarily with apfixed,
      // CASE: uniform fixed with apfixed flag
      if ((mixedTuning && fixedPrecision) || (fixedPrecision && apfixedFormat)) {
        // number of bits given in precision flag
        var b: Int = 0
        if (choosePrecision != null) {
          b = (uniformPrecisions(fnc.id): @unchecked) match { case FixedPrecision(a) => a}
        } else {
          b = (ctx.option[Precision]("precision"): @unchecked) match { case FixedPrecision(a) => a}
        }

        val newBody = toAPFixedCode(_body, b, ctx.intermediateRanges(fnc.id),
          ctx.intermediateAbsErrors(fnc.id))

        val fncParams = fnc.params.map({
          case ValDef(id) =>
            val actualRange = ctx.intermediateRanges(fnc.id)(Variable(id),
              emptyPath) +/- ctx.intermediateAbsErrors(fnc.id)(Variable(id), emptyPath)

            val parTotal = id.getType match {
              case FinitePrecisionType(FixedPrecision(a)) => a
              case RealType => (uniformPrecisions(fnc.id): @unchecked) match { case FixedPrecision(b) => b}
            }

            val intBits = FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRange))
            ValDef(id.changeType(APFixedType(parTotal, intBits)))
        })

        val retType = fnc.returnType match {
          case FinitePrecisionType(FixedPrecision(a)) =>
            val actualRange = ctx.resultRealRanges(fnc.id) +/- ctx.resultAbsoluteErrors(fnc.id)
            val intBits = FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRange))
            APFixedType(a, intBits)

          case TupleType(resTypes) =>
            getLastExpression(_body) match {
              case Tuple(args) =>
                TupleType(args.zip(resTypes).map({

                  // check that the result is actually a Variable
                  case (Variable(resId), retType) =>
                    val totalBits = retType match {
                      case FinitePrecisionType(FixedPrecision(a)) => a // types already changed
                      case RealType => (uniformPrecisions(fnc.id): @unchecked) match { case FixedPrecision(b) => b}
                    }

                    val actualRange = ctx.intermediateRanges(fnc.id)(Variable(resId), emptyPath) +/-
                      ctx.intermediateAbsErrors(fnc.id)(Variable(resId), emptyPath)
                    val intBits = FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRange))
                    APFixedType(totalBits, intBits)
                }))
            }
        }
        val precision = (ctx.option[Precision]("precision"): @unchecked) match {
            case FixedPrecision(a) => a
        }
        val costFunction = computeCost(fncParams, newBody, retType, ctx.originalFunctions(fnc.id), precision)
        reporter.info("Cost Function:" + costFunction)

        if (ctx.hasFlag("codegenNNOriginal")) {
          assignQNNOriginalPrecision(fncParams, newBody, retType, ctx.originalFunctions(fnc.id), precision)

        } else {
          fnc.copy(
            params = fncParams,
            body = Some(newBody),
            returnType = retType)
        }


      // CASE: mixed-tuning with floats
      } else if (mixedTuning) {
        // types are changed before, since the semantics of type assignments is special
        fnc

      } else if (fixedPrecision) {
        assert(!ctx.option[Option[String]]("mixed-precision").isDefined,
          "Mixed-precision codegen is not supported for fixed-points.")

        val b = (uniformPrecisions(fnc.id): @unchecked) match { case FixedPrecision(b) => b}
        val newBody = toFixedPointCode(_body, FixedPrecision(b),
          ctx.intermediateRanges(fnc.id), ctx.intermediateAbsErrors(fnc.id))
        val valDefType = b match {
           // cast precisions between 8, 16 and 32
          case x if x <= 8 => Int16Type
          case x if 8 < x && x <= 16 => Int32Type
          case x if 16 < x && x <= 32 => Int64Type
        }
        val newReturnType = fnc.returnType match {
          case TupleType(args) => TupleType(args.map(x => valDefType))
          case _ => valDefType
        }

        fnc.copy(params = fnc.params.map(vd => ValDef(vd.id.changeType(valDefType))),
          body = Some(newBody),
          returnType = newReturnType)

      // floats
      } else {
        val defaultPrec = uniformPrecisions.getOrElse(fnc.id, Float64)
        val typeMap = ctx.specInputPrecisions(fnc.id).withDefaultValue(defaultPrec)
        // // if we have floating-point code, we need to just change the types
        val newBody = assignFloatType(_body, typeMap, defaultPrec)._1
        val newReturnType = fnc.returnType match {
          case TupleType(args) => TupleType(args.map(x => FinitePrecisionType(defaultPrec)))
          case _ => FinitePrecisionType(defaultPrec)
        }

        fnc.copy(
          returnType = newReturnType,
          params = fnc.params.map(vd => ValDef(vd.id.changeType(FinitePrecisionType(typeMap(vd.id))))),
          body = Some(newBody)
        )

      }
    }

    val newProgram = Program(prg.id, newDefs)

    val lang = if ((mixedTuning && fixedPrecision) || ctx.hasFlag("apfixed")) "apfixed"
      else ctx.option[String]("lang")

    writeFile(newProgram, lang, ctx)

    (ctx, newProgram)
  }

  def fixedBits(prec: Precision): Int = (prec: @unchecked) match {
    case FixedPrecision(a) => a
  }

  private def reTuple(e: Expr, resIds: Seq[Identifier]): Expr = e match {
    // found the first inserted result ID
    case Let(id, _, _) if (resIds.contains(id)) =>
      // replace with just the tuple
      Tuple(reconstructTuple(e))

    case Let(id, value, body) => Let(id, value, reTuple(body, resIds))

    case _ => e

  }

  private def reconstructTuple(e: Expr): Seq[Expr] = e match {
    // last one, i.e. base case
    case Let(_, Variable(id), Variable(_)) =>
      Seq(Variable(id))

    case Let(_, Variable(id), body) =>
      reconstructTuple(body) :+ Variable(id)
  }

  private def writeFile(prg: Program, lang: String, ctx: Context): Unit = {
    import java.io.FileWriter
    import java.io.BufferedWriter

    val filename = lang match {
      case "apfixed" => System.getProperty("user.dir")+"/output/" + prg.id + ".cpp"
      case _ =>
      System.getProperty("user.dir")+"/output/" + prg.id + CodePrinter.suffix(lang)
    }
    ctx.codegenOutput.append(prg.id)
    val fstream = new FileWriter(filename)
    val out = new BufferedWriter(fstream)
    CodePrinter(prg, ctx, lang, out)
  }

  private def assignFloatType(e: Expr, tpeMap: Map[Identifier, Precision],
    defaultPrecision: Precision): (Expr, Precision) = e match {

    case Variable(id) =>
      (Variable(id.changeType(FinitePrecisionType(tpeMap(id)))), tpeMap(id))

    case x @ RealLiteral(r) => if(x.stringValue == null){
      (FinitePrecisionLiteral(r, defaultPrecision, r.toString()), defaultPrecision)
    } else {
      (FinitePrecisionLiteral(r, defaultPrecision,x.stringValue), defaultPrecision)
    }
    case ArithOperator(es_old, recons) =>
      val (es, ps) = es_old.unzip(assignFloatType(_, tpeMap, defaultPrecision))

      val prec = getUpperBound(ps: _*)
      (recons(es), prec)

    case Let(id, value, body) =>
      val (eValue, valuePrec) = assignFloatType(value, tpeMap, defaultPrecision)
      val (eBody, bodyPrec) = assignFloatType(body, tpeMap, defaultPrecision)

      val idPrec = tpeMap(id)

      if (idPrec >= valuePrec) {
        (Let(id.changeType(FinitePrecisionType(tpeMap(id))), eValue, eBody), bodyPrec)
      } else {
        val newValue = Cast(eValue, FinitePrecisionType(idPrec))
        (Let(id.changeType(FinitePrecisionType(tpeMap(id))), newValue, eBody), bodyPrec)
      }

    case IfExpr(cond, thenn, elze) =>
      val (eCond, _) = assignFloatType(cond, tpeMap, defaultPrecision)
      val (eThen, thenPrec) = assignFloatType(thenn, tpeMap, defaultPrecision)
      val (eElse, elsePrec) = assignFloatType(elze, tpeMap, defaultPrecision)

      val prec = getUpperBound(Seq(thenPrec, elsePrec): _*)

      (IfExpr(eCond, eThen, eElse), prec)

    case Tuple(args) =>
      (Tuple(args.map(assignFloatType(_, tpeMap, defaultPrecision)).unzip._1),
        defaultPrecision)
        // tuples can only occur at the very end, hence the returned precision
        // doesn't matter

    case GreaterThan(l, r) =>
      val (eLeft, leftPrec) = assignFloatType(l, tpeMap, defaultPrecision)
      val (eRight, rightPrec) = assignFloatType(r, tpeMap, defaultPrecision)

      val prec = getUpperBound(Seq(leftPrec, rightPrec): _*)
      (GreaterThan(eLeft, eRight), prec)

    case GreaterEquals(l, r) =>
      val (eLeft, leftPrec) = assignFloatType(l, tpeMap, defaultPrecision)
      val (eRight, rightPrec) = assignFloatType(r, tpeMap, defaultPrecision)

      val prec = getUpperBound(Seq(leftPrec, rightPrec): _*)
      (GreaterEquals(eLeft, eRight), prec)

    case LessThan(l, r) =>
      val (eLeft, leftPrec) = assignFloatType(l, tpeMap, defaultPrecision)
      val (eRight, rightPrec) = assignFloatType(r, tpeMap, defaultPrecision)

      val prec = getUpperBound(Seq(leftPrec, rightPrec): _*)
      (LessThan(eLeft, eRight), prec)

    case LessEquals(l, r) =>
      val (eLeft, leftPrec) = assignFloatType(l, tpeMap, defaultPrecision)
      val (eRight, rightPrec) = assignFloatType(r, tpeMap, defaultPrecision)

      val prec = getUpperBound(Seq(leftPrec, rightPrec): _*)
      (LessEquals(eLeft, eRight), prec)
  }

  /*
   * Expects code to be already in SSA form.
   * @param format the (uniform) fixed-point precision to use
   */
  def toFixedPointCode(expr: Expr, format: FixedPrecision, intermRanges: Map[(Expr, PathCond), Interval],
    intermAbsErrors: Map[(Expr, PathCond), Rational]): Expr = {
    val newType = format match {
      case FixedPrecision(x) if x <= 8 => Int16Type
      case FixedPrecision(x) if 8 < x && x <= 16 => Int32Type
      case FixedPrecision(x) if 16 < x && x <= 32 => Int64Type
    }

    @inline
    def getFractionalBits(e: Expr, path: PathCond): Int = {
      // the overall interval is the real-valued range +/- absolute errors
      val interval = intermRanges(e, path) +/- intermAbsErrors(e, path)
      format.fractionalBits(interval)
    }


    def _toFPCode(e: Expr, path: PathCond): Expr = (e: @unchecked) match {
      case x @ Variable(id) => Variable(id.changeType(newType))

      case RealLiteral(r) => // TODO: translate constant
        val f = format.fractionalBits(r)
        format match {
          case FixedPrecision(x) if x <= 8 => Int16Literal((r * Rational.fromDouble(math.pow(2, f))).roundToInt)
          case FixedPrecision(x) if 8 < x && x <= 16 => Int32Literal((r * Rational.fromDouble(math.pow(2, f))).roundToInt)
          case FixedPrecision(x) if 16 < x && x <= 32 => Int64Literal((r * Rational.fromDouble(math.pow(2, f))).roundToLong)
        }

      case UMinus(t) => UMinus(_toFPCode(t, path))

      case Sqrt(t) =>
        throw new Exception("Sqrt is not supported for fixed-points!")

      case x @ Plus(lhs, rhs) =>
        val fLhs = getFractionalBits(lhs, path)
        val fRhs = getFractionalBits(rhs, path)

        // determine how much to shift left or right
        val fAligned = math.max(fLhs, fRhs)
        val newLhs =
          if (fLhs < fAligned) {
            LeftShift(_toFPCode(lhs, path), (fAligned - fLhs))
          } else {
            _toFPCode(lhs, path)
          }
        val newRhs =
          if (fRhs < fAligned) {
            LeftShift(_toFPCode(rhs, path), (fAligned - fRhs))
          } else {
            _toFPCode(rhs, path)
          }

        // fractional bits result
        val fRes = getFractionalBits(x, path)
        // shift result
        if (fAligned == fRes) {
          Plus(newLhs, newRhs)
        } else if (fRes < fAligned) {
          RightShift(Plus(newLhs, newRhs), (fAligned - fRes))
        } else { // (fAligned < fRes) {
          // TODO: this sounds funny. does this ever happen?
          //reporter.warning("funny shifting condition is happening")
          LeftShift(Plus(newLhs, newRhs), (fRes - fAligned))

        }

      case x @ Minus(lhs, rhs) =>
        // fractional bits from lhs
        val fLhs = getFractionalBits(lhs, path)
        val fRhs = getFractionalBits(rhs, path)

        // determine how much to shift left or right
        val fAligned = math.max(fLhs, fRhs)
        val newLhs =
          if (fLhs < fAligned) {
            LeftShift(_toFPCode(lhs, path), (fAligned - fLhs))
          } else {
            _toFPCode(lhs, path)
          }
        val newRhs =
          if (fRhs < fAligned) {
            LeftShift(_toFPCode(rhs, path), (fAligned - fRhs))
          } else {
            _toFPCode(rhs, path)
          }

        // fractional bits result
        val fRes = getFractionalBits(x, path)
        // shift result
        if (fAligned == fRes) {
          Minus(newLhs, newRhs)
        } else if (fRes < fAligned) {
          RightShift(Minus(newLhs, newRhs), (fAligned - fRes))
        } else { // (fAligned < fRes) {
          // TODO: this sounds funny. does this ever happen?
          //reporter.warning("funny shifting condition is happening")
          LeftShift(Minus(newLhs, newRhs), (fRes - fAligned))
        }

      case x @ Times(lhs, rhs) =>

        val mult = Times(_toFPCode(lhs, path), _toFPCode(rhs, path))
        val fMult = getFractionalBits(lhs, path) + getFractionalBits(rhs, path)

        // fractional bits result
        val fRes = getFractionalBits(x, path)
        // shift result
        if (fMult == fRes) {
          mult
        } else if (fRes < fMult) {
          RightShift(mult, (fMult - fRes))
        } else { // (fAligned < fRes) {
          // TODO: this sounds funny. does this ever happen?
          //reporter.warning("funny shifting condition is happening")
          LeftShift(mult, (fRes - fMult))
        }

      case x @ Division(lhs, rhs) =>
        val fLhs = getFractionalBits(lhs, path)
        val fRhs = getFractionalBits(rhs, path)

        val fRes = getFractionalBits(x, path)
        val shift = fRes + fRhs - fLhs
        Division(LeftShift(_toFPCode(lhs, path), shift), _toFPCode(rhs, path))

      case Let(id, value, body) =>
        Let(id.changeType(newType), _toFPCode(value, path), _toFPCode(body, path))

      case x @ IfExpr(cond, thenn, elze) =>
        IfExpr(_toFPCode(cond, path), _toFPCode(thenn, path :+ cond),
          _toFPCode(elze, path :+ lang.TreeOps.negate(cond)))

      case Tuple(args) => Tuple(args.map(_toFPCode(_, path)))

      case x @ GreaterThan(l, r) => GreaterThan(_toFPCode(l, path), _toFPCode(r, path))
      case x @ GreaterEquals(l, r) => GreaterEquals(_toFPCode(l, path), _toFPCode(r, path))
      case x @ LessThan(l, r) => LessThan(_toFPCode(l, path), _toFPCode(r, path))
      case x @ LessEquals(l, r) => LessEquals(_toFPCode(l, path), _toFPCode(r, path))
    }

    _toFPCode(expr, emptyPath)
  }

  /*
   * Generates code for the Vivado HLS hardware synthesis tool with the ap_fixed data type.
   * Expects code to be already in SSA form.
   */
  def toAPFixedCode(expr: Expr, totalBits: Int, intermRanges: Map[(Expr, PathCond), Interval],
    intermAbsErrors: Map[(Expr, PathCond), Rational]): Expr = {

    @inline
    def getIntegerBits(e: Expr, path: PathCond): Int = {
      // the overall interval is the real-valued range +/- absolute errors
      var actualRange = intermRanges(e, path)
      actualRange = actualRange +/- intermAbsErrors(e, path)
      FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRange))
    }

    def _toFPCode(e: Expr, path: PathCond): Expr = (e: @unchecked) match {
      case x @ Variable(id) => x //Variable(id.changeType(newType))

      case x @ RealLiteral(r) => x  // constants are handled automatically?

      case x @ FinitePrecisionLiteral(r,prec,s)  => x

      case x @ Cast(ex,typ) => //ex
        val bits = typ match {
            case FinitePrecisionType(FixedPrecision(a)) => a
            case _ => totalBits
        }
        ex match {
          case Variable(_) => Cast(ex, APFixedType(bits,getIntegerBits(x, path)))
          case UMinus(Variable(_)) => Cast(ex, APFixedType(bits,getIntegerBits(x, path)))
          case _ => Cast(_toFPCode(ex, path), APFixedType(bits,getIntegerBits(ex, path)))
        }

      case x @ ArithOperator(Seq(t: Expr), recons) => //x
        recons(Seq(_toFPCode(t, path)))

      case x @ ArithOperator(Seq(lhs: Expr, rhs: Expr), recons) => //x
        val rhsFP = _toFPCode(rhs, path)
        val lhsFP = _toFPCode(lhs, path)
        recons(Seq(lhsFP, rhsFP))

      case Let(id, value, body) =>
        val bits = id.getType match {
            case FinitePrecisionType(FixedPrecision(a)) => a
            case _ => totalBits
        }
        val idType = APFixedType(bits, getIntegerBits(value, path))
        Let(id.changeType(idType), _toFPCode(value, path),
          _toFPCode(body, path))

      case x @ IfExpr(cond, thenn, elze) =>
        IfExpr(_toFPCode(cond, path), _toFPCode(thenn, path :+ cond),
               _toFPCode(elze, path :+ lang.TreeOps.negate(cond)))

      case x @ GreaterThan(l, r) => GreaterThan(_toFPCode(l, path), _toFPCode(r, path))
      case x @ GreaterEquals(l, r) => GreaterEquals(_toFPCode(l, path), _toFPCode(r, path))
      case x @ LessThan(l, r) => LessThan(_toFPCode(l, path), _toFPCode(r, path))
      case x @ LessEquals(l, r) => LessEquals(_toFPCode(l, path), _toFPCode(r, path))

      case x @ Tuple(_) => x
    }
    _toFPCode(expr, emptyPath)
  }

  /*
   * Expects code to be already in SSA form.
   * Expects the types to be attached to the tree and expects the program
   * to be in SSA form.
   * TODO: check that this is sound, I think the range should be finite-precision
   * and not real-valued as it is most likely here...
   */
  def toMixedFixedPointCode(expr: Expr, path: PathCond, rangeMap: Map[(Expr, PathCond), Interval]): Expr = (expr: @unchecked) match {

    case x @ Variable(id) => id.getType match {
      case FinitePrecisionType(FixedPrecision(x)) if 8 < x && x <= 16 => Variable(id.changeType(Int32Type))
      case FinitePrecisionType(FixedPrecision(x)) if 16 < x && x <= 32 => Variable(id.changeType(Int64Type))
    }

    // case Let(id, x @ RealLiteral(r), body) =>
    //   val tpe = id.getType.asInstanceOf[Fixed]
    //   val f = tpe.fractionalBits(r)
    //   tpe match {
    //     case FixedPrecision(16) =>
    //       val tmp = Int32Literal((r * Rational.fromDouble(math.pow(2, f))).roundToInt)
    //       Let(id.changeType(Int32Type), tmp, toMixedFixedPointCode(body))

    //     case FixedPrecision(32) =>
    //       val tmp = Int64Literal((r * Rational.fromDouble(math.pow(2, f))).roundToLong)
    //       Let(id.changeType(Int64Type), tmp, toMixedFixedPointCode(body))
    //   }

    case FinitePrecisionLiteral(r, prec @ FixedPrecision(_), strVal) =>
      val f = prec.fractionalBits(r)
      prec match {
        case FixedPrecision(x) if 8 < x && x <= 16 => Int32Literal((r * Rational.fromDouble(math.pow(2, f))).roundToInt)
        case FixedPrecision(x) if 16 < x && x <= 32 => Int64Literal((r * Rational.fromDouble(math.pow(2, f))).roundToLong)
      }

    // TODO: we may shift too much, i.e. (x >> 32) << 3, which can be optmixed/
    // cleaned up after the fact.
    // necessarily a down cast from 32 bit
    case Cast(e, FinitePrecisionType(FixedPrecision(16))) =>
      Cast(RightShift(toMixedFixedPointCode(e, path, rangeMap), 16), Int32Type)

    // necessarily an up cast from 16 bit, and not redundant (if all went well in mixed-opt)
    case Cast(e, FinitePrecisionType(FixedPrecision(32))) =>
      Cast(LeftShift(toMixedFixedPointCode(e, path, rangeMap), 16), Int64Type)

    // case Let(id, x @ UMinus(y @ Variable(t)), body) =>
    //   if (id.getType == t.getType) {
    //     UMinus(toMixedFixedPointCode(t))
    //   } else {
    //     assert(id.getType < t.getType)
    //     //downcast
    //     RightShift(UMinus(toMixedFixedPointCode(t)), 16)
    //   }

    case UMinus(t) => UMinus(toMixedFixedPointCode(t, path, rangeMap))

    case Sqrt(t) =>
      throw new Exception("Sqrt is not supported for fixed-points!")


    case x @ Plus(lhs, rhs) =>
      // TODO: is there some better way?!
      val lhsPrec = lhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      val rhsPrec = rhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      // fractional bits from lhs
      val fLhs = lhsPrec.fractionalBits(rangeMap(lhs, path))
      val fRhs = rhsPrec.fractionalBits(rangeMap(rhs, path))

      // determine how much to shift left or right
      val fAligned = math.max(fLhs, fRhs)
      val newLhs =
        if (fLhs < fAligned) LeftShift(toMixedFixedPointCode(lhs, path, rangeMap), (fAligned - fLhs))
        else toMixedFixedPointCode(lhs, path, rangeMap)
      val newRhs =
        if (fRhs < fAligned) LeftShift(toMixedFixedPointCode(rhs, path, rangeMap), (fAligned - fRhs))
        else toMixedFixedPointCode(rhs, path, rangeMap)

      // fractional bits result
      val fRes = getUpperBound(lhsPrec, rhsPrec).asInstanceOf[FixedPrecision].fractionalBits(rangeMap(x, path))
      // shift result
      if (fAligned == fRes) {
        Plus(newLhs, newRhs)
      } else if(fRes < fAligned) {
        RightShift(Plus(newLhs, newRhs), (fAligned - fRes))
      } else { //(fAligned < fRes) {
        // TODO: this sounds funny. does this ever happen?
        reporter.warning("funny shifting condition is happening")
        LeftShift(Plus(newLhs, newRhs), (fRes - fAligned))

      }

    case x @ Minus(lhs, rhs) =>
      // TODO: is there some better way?!
      val lhsPrec = lhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      val rhsPrec = rhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      // fractional bits from lhs
      val fLhs = lhsPrec.fractionalBits(rangeMap(lhs, path))
      val fRhs = rhsPrec.fractionalBits(rangeMap(rhs, path))

      // determine how much to shift left or right
      val fAligned = math.max(fLhs, fRhs)
      val newLhs =
        if (fLhs < fAligned) LeftShift(toMixedFixedPointCode(lhs, path, rangeMap), (fAligned - fLhs))
        else toMixedFixedPointCode(lhs, path, rangeMap)
      val newRhs =
        if (fRhs < fAligned) LeftShift(toMixedFixedPointCode(rhs, path, rangeMap), (fAligned - fRhs))
        else toMixedFixedPointCode(rhs, path, rangeMap)

      // fractional bits result
      val fRes = getUpperBound(lhsPrec, rhsPrec).asInstanceOf[FixedPrecision].fractionalBits(rangeMap(x, path))
      // shift result
      if (fAligned == fRes) {
        Minus(newLhs, newRhs)
      } else if(fRes < fAligned) {
        RightShift(Minus(newLhs, newRhs), (fAligned - fRes))
      } else { //(fAligned < fRes) {
        // TODO: this sounds funny. does this ever happen?
        reporter.warning("funny shifting condition is happening")
        LeftShift(Minus(newLhs, newRhs), (fRes - fAligned))
      }

    case x @ Times(lhs, rhs) =>
      // TODO: is there some better way?!
      val lhsPrec = lhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      val rhsPrec = rhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]

      val mult = Times(toMixedFixedPointCode(lhs, path, rangeMap), toMixedFixedPointCode(rhs, path, rangeMap))
      val fMult = lhsPrec.fractionalBits(rangeMap(lhs, path)) +
        rhsPrec.fractionalBits(rangeMap(rhs, path))

      // fractional bits result
      val fRes = getUpperBound(lhsPrec, rhsPrec).asInstanceOf[FixedPrecision].fractionalBits(rangeMap(x, path))
      // shift result
      if (fMult == fRes) {
        mult
      } else if(fRes < fMult) {
        RightShift(mult, (fMult - fRes))
      } else { //(fAligned < fRes) {
        // TODO: this sounds funny. does this ever happen?
        reporter.warning("funny shifting condition is happening")
        LeftShift(mult, (fRes - fMult))
      }

    case x @ Division(lhs, rhs) =>
      // TODO: is there some better way?!
      val lhsPrec = lhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      val rhsPrec = rhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]

      val fLhs = lhsPrec.fractionalBits(rangeMap(lhs, path))
      val fRhs = rhsPrec.fractionalBits(rangeMap(rhs, path))

      val fRes = getUpperBound(lhsPrec, rhsPrec).asInstanceOf[FixedPrecision].fractionalBits(rangeMap(x, path))
      val shift = fRes + fRhs - fLhs
      Division(LeftShift(toMixedFixedPointCode(lhs, path, rangeMap), shift), toMixedFixedPointCode(rhs, path, rangeMap))

    case Let(id, value, body) =>
      val newId = id.changeType(id.getType match {
        case FinitePrecisionType(FixedPrecision(x)) if 8 < x && x <= 16 => Int32Type
        case FinitePrecisionType(FixedPrecision(x)) if 16 < x && x <= 32 => Int64Type
      })
      Let(newId, toMixedFixedPointCode(value, path, rangeMap), toMixedFixedPointCode(body, path, rangeMap))
  }

  // assign fixed-point precision types to original neural network functions
  private def assignQNNOriginalPrecision(fncParams: Seq[ValDef], newBody: Expr,
    retType: TypeTree, originalFnc: FunDef, prec: Int): FunDef = {

    def unifyAPFixed(aps: Set[TypeTree]): APFixedType = {
      // the final/overall precision is determined as <totalbits, intBits>
      // - totalbits = max-fractional-bits + max-integer-bits (to satisfy rounding error)
      // - intBits = max-integer-bits (to satisfy no overflow)

      val (maxIntBits: Int, maxFpBits: Int) = aps.foldLeft((-1, -1)){
        case ((currMaxInt, currMaxFp), APFixedType(totalBits, intBits)) =>
          ( math.max(currMaxInt, intBits), math.max(currMaxFp, totalBits - intBits) )
      }
      // assumes that overall bitlength is limited to 64 bits
      APFixedType(math.min(prec, maxIntBits + maxFpBits), maxIntBits)
    }

    def _toFPCode(e: Expr, typeMap: Map[Identifier, APFixedType]): Expr = (e: @unchecked) match {
      case x @ Variable(id) => x

      // TODO: revisit with mixed-precision
      case x @ Cast(ex,typ) => ???

      case Let(id, matrx @ MatrixLiteral(value),  body) =>
        Let(id.changeType(typeMap(id)), matrx, _toFPCode(body, typeMap))

      case Let(id, vectr @ VectorLiteral(value), body) =>
        Let(id.changeType(typeMap(id)), vectr, _toFPCode(body, typeMap))

      case Let(id, relu @ Relu(VectorPlus(
        LinearMap(m @ Variable(mId), x @ Variable(xId)),
        b @ Variable(bId))), body) =>

        val idType = typeMap(id)

        // we really want to annotate the operations, but will try to annotate
        // the IDs here as a proxy, even though the precision assigned here
        // may be different from the precision assigned to the variables before
        Let(id.changeType(idType), Relu(VectorPlus(
          LinearMap(Variable(mId.changeType(idType)), Variable(xId.changeType(idType))),
            Variable(bId.changeType(idType)))), _toFPCode(body, typeMap))

      case Let(id, linear @ Linear(VectorPlus(
        LinearMap(m @ Variable(mId), x @ Variable(xId)),
        b @ Variable(bId))), body) =>

        val idType = typeMap(id)

        // we really want to annotate the operations, but will try to annotate
        // the IDs here as a proxy, even though the precision assigned here
        // may be different from the precision assigned to the variables before
        Let(id.changeType(idType), Linear(VectorPlus(
          LinearMap(Variable(mId.changeType(idType)), Variable(xId.changeType(idType))),
            Variable(bId.changeType(idType)))), _toFPCode(body, typeMap))
    }

    val originalBody = originalFnc.body.get

    val newVars: Set[Identifier] = allLetVariablesOf(newBody)

    val originalIDs: Set[Identifier] = allLetVariablesOf(originalBody)

    val typeMap = originalIDs.map(origID => {
      // we identify matching IDs using the variable names
      // since we have control over the naming, this should be fine
      val relevantVars: Set[Identifier] = newVars.filter(i => i.toString.startsWith(origID.toString + "_"))
      val newPrecs = relevantVars.map(_.getType)
      val unifiedPrecision = unifyAPFixed(newPrecs)
      (origID -> unifiedPrecision)
    }).toMap

    val apFixedBody = _toFPCode(originalBody, typeMap)

    val apFixedReturnType = retType match {
      case TupleType(args) => TupleType(Seq.fill(args.size)(unifyAPFixed(args.toSet)))
    }
    originalFnc.copy(
      params = fncParams,
      body = Some(apFixedBody),
      returnType = apFixedReturnType
    )
  }

  // Returns the set of all variables occuring in let expressions.
  // We cannot use the allVariablesOf function in TreeOps, because that does
  // retain the needed type information (some occurrences have the right type, others don't)
  private def allLetVariablesOf(expr: Expr): Set[Identifier] = {
    lang.TreeOps.fold[Set[Identifier]] {
      case (e, subs) =>
        val subvs = subs.flatten.toSet
        e match {
          case Let(i, _, _) => subvs + i
          case _ => subvs
        }
    }(expr)
  }
  def computeCost(fncParams: Seq[ValDef], newBody: Expr,
    retType: TypeTree, originalFnc: FunDef, prec: Int): Rational = {

    def unifyAPFixed(aps: Set[TypeTree]): APFixedType = {
      // the final/overall precision is determined as <totalbits, intBits>
      // - totalbits = max-fractional-bits + max-integer-bits (to satisfy rounding error)
      // - intBits = max-integer-bits (to satisfy no overflow)

      val (maxIntBits: Int, maxFpBits: Int) = aps.foldLeft((-1, -1)){
        case ((currMaxInt, currMaxFp), APFixedType(totalBits, intBits)) =>
          ( math.max(currMaxInt, intBits), math.max(currMaxFp, totalBits - intBits) )
      }
      // assumes that overall bitlength is limited to 64 bits
      APFixedType(math.min(prec, maxIntBits + maxFpBits), maxIntBits)
    }

    val originalBody = originalFnc.body.get

    val newVars: Set[Identifier] = allLetVariablesOf(newBody)

    val originalIDs: Set[Identifier] = allLetVariablesOf(originalBody)

    val typeMap = originalIDs.map(origID => {
      // we identify matching IDs using the variable names
      // since we have control over the naming, this should be fine
      val relevantVars: Set[Identifier] = newVars.filter(i => i.toString.startsWith(origID.toString + "_"))
      val newPrecs = relevantVars.map(_.getType)
      val unifiedPrecision = unifyAPFixed(newPrecs)
      (origID -> unifiedPrecision)
    }).toMap

    val noOfInputs = fncParams.size
    var noOfNeurons = new ListBuffer[Int]
    var layer = 0

      // Gives the number of bits of Fixed Precision Type
    def intBits(apfixedFormat: APFixedType): Int = (apfixedFormat: @unchecked) match {
      case APFixedType(a, b) => b
    }
    def totalBits(apfixedFormat: APFixedType): Int = (apfixedFormat: @unchecked) match {
      case APFixedType(a, b) => a
    }

    def eval(e: Expr): Rational = e match {
      case Let(id, matrx @ MatrixLiteral(value),  body) =>
        eval(body)
      case Let(id, vectr @ VectorLiteral(value), body) =>
        noOfNeurons += value.size
        eval(body)

      case Let(id, relu @ Relu(VectorPlus(
        LinearMap(m @ Variable(mId), x @ Variable(xId)),
        b @ Variable(bId))), body) =>
        var dotCost = Rational.zero
        val wordLengthW = totalBits(typeMap(mId))
        val dot = totalBits(typeMap(id)) - intBits(typeMap(id))
        if (xId.toString.startsWith("x")) {
          dotCost = noOfNeurons(layer) * noOfInputs * wordLengthW * dot
        } else {
          dotCost = noOfNeurons(layer) * noOfNeurons(layer-1) * wordLengthW * dot
        }
        layer += 1
        val bias = totalBits(typeMap(id)) - intBits(typeMap(id))
        val biasCost = Rational.max(bias, dot)
        val actCost = biasCost
        dotCost + biasCost + actCost + eval(body)

      case Let(id, linear @ Linear(VectorPlus(
        LinearMap(m @ Variable(mId), x @ Variable(xId)),
        b @ Variable(bId))), body) =>
        var dotCost = Rational.zero
        val wordLengthW = totalBits(typeMap(mId))
        val dot = totalBits(typeMap(id)) - intBits(typeMap(id))
        if (xId.toString.startsWith("x")) {
          dotCost = noOfNeurons(layer) * noOfInputs * wordLengthW * dot
        } else {
          dotCost = noOfNeurons(layer) * noOfNeurons(layer-1) * wordLengthW * dot
        }
        layer += 1
        val bias = totalBits(typeMap(id)) - intBits(typeMap(id))
        val biasCost = Rational.max(bias, dot)
        val actCost = biasCost
        dotCost + biasCost + actCost + eval(body)
      case _ => Rational.zero
    }
    eval(originalBody)
  }

}







