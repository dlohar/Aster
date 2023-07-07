// Copyright 2020 MPI-SWS, Saarbruecken, Germany
package daisy
package transform

import scala.collection.immutable.Seq
import lang.Trees._
import tools.Rational
import lang.Identifiers._
import lang.Types._

object NNUnrollPhase extends DaisyPhase {
  override val name = "NNUnroll"
  override val shortName = "nn-unroll"
  override val description = "Unrolls a neural network so that Daisy can analyze it"

  var reporter: Reporter = null

  private var varStore: Map[String, Identifier] = Map()

  // dimensions of the declared matrices and vectors
  private var dimensions: Map[Identifier, List[Int]] = Map()


  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter

    val newDefs = transformConsideredFunctions(ctx, prg){ fnc => {
      val (lwrBnds, uprBnds) = extractPrecondition(fnc)
      //println(s"$lwrBnds - $uprBnds")

      // register the variables corresponding to the input
      val param: Identifier = fnc.params(0).id
      for(i <- 0 until lwrBnds.length) {
        getVar(s"${param}_${i}", RealType)
      }
      dimensions = dimensions + (param -> List(lwrBnds.length))

      // step 1: put code in three-address-form
      val tacBody = tac(fnc.body.get)

      // step 2: unroll
      val unrolledBody = unroll(tacBody)

      // step 3: reconstruct the function
      val params: Seq[ValDef] = (0 until lwrBnds.length).map(
        i => ValDef(getVar(s"${param}_${i}", RealType)))

      val boundConditions: Seq[Expr] = params.zip(lwrBnds.zip(uprBnds)).flatMap({
        case (p, (lBound, uBound)) =>
          Seq(LessEquals(RealLiteral(lBound), Variable(p.id)), LessEquals(Variable(p.id), RealLiteral(uBound)))
      })
      val precondition = Some(And(boundConditions))
      val body = Some(unrolledBody)
      val postcondition = fnc.postcondition

      val returnType = lang.TreeOps.getLastExpression(unrolledBody) match {
        case Tuple(args) => TupleType(args.map(x => RealType))
        case Variable(_) => RealType
      }

      FunDef(fnc.id, returnType, params, precondition, body, postcondition)
    }}

    //println("Unrolled program:")
    //println(newDefs)

    assert(ctx.originalFunctions.isEmpty) // do not overwrite data
    (ctx.copy(originalFunctions = prg.defs.map(f => (f.id, f)).toMap),
      Program(prg.id, newDefs))
  }

  // return an existing or a new variable by the given name
  private def getVar(s: String, tpe: TypeTree): Identifier = {
    varStore.get(s) match {
      case Some(id) => id
      case None =>
        val newId = FreshIdentifier(s, tpe)
        varStore = varStore + (s -> newId)
        newId
    }
  }

  // this necessarily has to have been already created, so will crash if incorrectly used
  private def getRealVar(s: String): Variable = {
    Variable(varStore(s))
  }

  private def tac(expr: Expr): Expr = expr match {
    case Let(id, m @ MatrixLiteral(value), next) =>
      dimensions = dimensions + (id -> List(value.length, value(0).length))
      Let(id, m, tac(next))

    case Let(id, v @ VectorLiteral(value), next) =>
      dimensions = dimensions + (id -> List(value.length))
      Let(id, v, tac(next))

    // TODO/FIXME: hardcoded for specific input formats (for now)
    case Let(id, Relu(VectorPlus(LinearMap(m @ Variable(mId), x @ Variable(xId)), b @ Variable(bId))), next) =>
      val id_dot = getVar(s"${id}_dot", VectorType)
      val id_bias = getVar(s"${id}_bias", VectorType)

      dimensions = dimensions + (id_dot -> List(dimensions(mId)(0)))
      dimensions = dimensions + (id_bias -> dimensions(bId))
      dimensions = dimensions + (id -> dimensions(bId))
      // TODO: dimension check should really be here, not in unroll

      Let(id_dot, LinearMap(m, x),
        Let(id_bias, VectorPlus(Variable(id_dot), b),
          Let(id, Relu(Variable(id_bias)), tac(next))))

    // TODO/FIXME: hardcoded for specific input formats (for now)
    case Let(id, Linear(VectorPlus(LinearMap(m @ Variable(mId), x @ Variable(xId)), b @ Variable(bId))), next) =>
      val id_dot = getVar(s"${id}_dot", VectorType)
      val id_bias = getVar(s"${id}_bias", VectorType)

      dimensions = dimensions + (id_dot -> List(dimensions(mId)(0)))
      dimensions = dimensions + (id_bias -> dimensions(bId))
      dimensions = dimensions + (id -> dimensions(bId))
      // TODO: dimension check should really be here, not in unroll

      Let(id_dot, LinearMap(m, x),
        Let(id_bias, VectorPlus(Variable(id_dot), b),
          Let(id, Linear(Variable(id_bias)), tac(next))))


    case Let(id, Tanh(VectorPlus(LinearMap(m @ Variable(mId), x @ Variable(xId)), b @ Variable(bId))), next) =>
      val id_dot = getVar(s"${id}_dot", VectorType)
      val id_bias = getVar(s"${id}_bias", VectorType)

      dimensions = dimensions + (id_dot -> List(dimensions(mId)(0)))
      dimensions = dimensions + (id_bias -> dimensions(bId))
      dimensions = dimensions + (id -> dimensions(bId))
      // TODO: dimension check should really be here, not in unroll

      Let(id_dot, LinearMap(m, x),
        Let(id_bias, VectorPlus(Variable(id_dot), b),
          Let(id, Tanh(Variable(id_bias)), tac(next))))

    case Relu(VectorPlus(LinearMap(m, x @ Variable(xId)), b @ Variable(bId))) =>
      val id_dot = getVar("_last_dot", VectorType)
      val id_bias = getVar("_last_bias", VectorType)

      dimensions = dimensions + (id_dot -> dimensions(xId))
      dimensions = dimensions + (id_bias -> dimensions(bId))
      // TODO: dimension check should really be here, not in unroll

      Let(id_dot, LinearMap(m, x),
        Let(id_bias, VectorPlus(Variable(id_dot), b),
          Relu(Variable(id_bias))))
    case Linear(VectorPlus(LinearMap(m, x @ Variable(xId)), b @ Variable(bId))) =>
      val id_dot = getVar("_last_dot", VectorType)
      val id_bias = getVar("_last_bias", VectorType)

      dimensions = dimensions + (id_dot -> dimensions(xId))
      dimensions = dimensions + (id_bias -> dimensions(bId))
      // TODO: dimension check should really be here, not in unroll

      Let(id_dot, LinearMap(m, x),
        Let(id_bias, VectorPlus(Variable(id_dot), b),
          Linear(Variable(id_bias))))


    case Tanh(VectorPlus(LinearMap(m, x @ Variable(xId)), b @ Variable(bId))) =>
      val id_dot = getVar("_last_dot", VectorType)
      val id_bias = getVar("_last_bias", VectorType)

      dimensions = dimensions + (id_dot -> dimensions(xId))
      dimensions = dimensions + (id_bias -> dimensions(bId))
      // TODO: dimension check should really be here, not in unroll

      Let(id_dot, LinearMap(m, x),
        Let(id_bias, VectorPlus(Variable(id_dot), b),
          Tanh(Variable(id_bias))))

    case _ => expr
  }

  def unroll(expr: Expr): Expr = expr match {
    case Let(id, MatrixLiteral(value), next) =>
      unrollMatrix(value, id.toString, () => unroll(next), 0, 0)

    case Let(id, VectorLiteral(value), next) =>
      unrollVector(value, id.toString, () => unroll(next), 0)

    case Let(id, LinearMap(Variable(m), Variable(v)), next) =>
      val dimM = dimensions(m)
      val dimV = dimensions(v)
      assert(dimM(1) == dimV(0), s"the dimensions of $m and $v do not match")
      unrollLinearMap(id.toString, m.toString, v.toString, dimM(0), dimV(0), () => unroll(next), 0)

    case Let(id, VectorPlus(Variable(v), Variable(b)), next) =>
      val dimV = dimensions(v)(0)
      val dimB = dimensions(b)(0)
      assert(dimV == dimB, s"The dimensions of $v and $b do not match.")
      unrollVectorAddition(id.toString, v.toString, b.toString, dimV, () => unroll(next), 0)

    case Let(id, Relu(Variable(b)), next) =>
      unrollRelu(id.toString, b.toString, dimensions(b)(0), () => unroll(next), 0)
    case Let(id, Linear(Variable(b)), next) =>
      unrollLinear(id.toString, b.toString, dimensions(b)(0), () => unroll(next), 0)

    case Let(id, Tanh(Variable(b)), next) =>
      unrollTanh(id.toString, b.toString, dimensions(b)(0), () => unroll(next), 0)

    case Variable(x) if (x.getType == VectorType) =>
      val dimX = dimensions(x)(0)
      Tuple((0 until dimX).map(i => getRealVar(s"${x}_${i}")))
      // mixed-precision tuning does not support tuple types
      //Tuple((0 until dimX).map(i => getRealVar(s"${x}_${i}")))
      //Max((0 until dimX).map(i => getRealVar(s"${x}_${i}")))
  }



  def unrollLinearMap(id: String, mName: String, vName: String, maxI: Int, j: Int, next: () => Expr, i: Int): Expr = {
    if (i == maxI) {
      next()
    } else {
      val firstTerm: Expr = Times(getRealVar(s"${mName}_${i}_0"), getRealVar(s"${vName}_0"))

      val dotProduct: Expr = (1 until j).map(k =>
        Times(getRealVar(s"${mName}_${i}_${k}"), getRealVar(s"${vName}_${k}"))).fold(firstTerm)({
          case (acc: Expr, term: Expr) => Plus(acc, term)
        })

      Let(getVar(s"${id}_${i}", RealType), dotProduct, unrollLinearMap(id, mName, vName, maxI, j, next, i + 1))
    }
  }

  def unrollVectorAddition(id: String, vName: String, bName: String, maxI: Int, next: () => Expr, i: Int): Expr = {
    if (i == maxI) {
      next()
    } else {
      Let(getVar(s"${id}_${i}", RealType), Plus(getRealVar(s"${vName}_${i}"), getRealVar(s"${bName}_${i}")),
        unrollVectorAddition(id, vName, bName, maxI, next, i + 1))
    }
  }

  def unrollRelu(id: String, vName: String, maxI: Int, next: () => Expr, i: Int): Expr = {
    if (i == maxI) {
      next()
    } else {
      Let(getVar(s"${id}_${i}", RealType), Relu(getRealVar(s"${vName}_${i}")),
        unrollRelu(id, vName, maxI, next, i + 1))
    }
  }

   def unrollLinear(id: String, vName: String, maxI: Int, next: () => Expr, i: Int): Expr = {
    if (i == maxI) {
      next()
    } else {
      Let(getVar(s"${id}_${i}", RealType), Linear(getRealVar(s"${vName}_${i}")),
        unrollLinear(id, vName, maxI, next, i + 1))
    }
  }

  def unrollTanh(id: String, vName: String, maxI: Int, next: () => Expr, i: Int): Expr = {
    if (i == maxI) {
      next()
    } else {
      Let(getVar(s"${id}_${i}", RealType), Tanh(getRealVar(s"${vName}_${i}")),
        unrollTanh(id, vName, maxI, next, i + 1))
    }
  }

  def unrollMatrix(m: List[List[Rational]], name: String, next: () => Expr, i: Int, j: Int): Expr = {
    // base case; we are done
    if (i == m.length - 1 && j == m(i).length - 1) {

      Let(getVar(s"${name}_${i}_${j}", RealType), RealLiteral(m(i)(j), m(i)(j).toString), next())

    } else {
      if (j == m(i).length - 1) { // we are at the end of a row
        Let(getVar(s"${name}_${i}_${j}", RealType), RealLiteral(m(i)(j), m(i)(j).toString),
          unrollMatrix(m, name, next, i + 1, 0))
      } else {
        Let(getVar(s"${name}_${i}_${j}", RealType), RealLiteral(m(i)(j), m(i)(j).toString),
          unrollMatrix(m, name, next, i, j + 1))
      }
    }
  }

  def unrollVector(v: List[Rational], name: String, next: () => Expr, i: Int): Expr = {
    // base case; we are done
    if (i == v.length - 1) {
      Let(getVar(s"${name}_${i}", RealType), RealLiteral(v(i), v(i).toString), next())
    } else {
      Let(getVar(s"${name}_${i}", RealType), RealLiteral(v(i), v(i).toString), unrollVector(v, name, next, i + 1))
    }
  }

  def extractPrecondition(fnc: FunDef): (List[Rational], List[Rational]) = {
    fnc.precondition match {
      case Some(And(Seq(LowerBounds(tLow, bndsLow), UpperBounds(tHigh, bndsHigh)))) =>
        (bndsLow, bndsHigh)

      case Some(And(Seq(UpperBounds(tHigh, bndsHigh), LowerBounds(tLow, bndsLow)))) =>
        (bndsLow, bndsHigh)

      case _ => reporter.fatalError("You forgot the precondition (or it is in the wrong format) for " + fnc.id)
    }
  }
}
