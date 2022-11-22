package com.softwaremill.mmplayground

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

case class Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, -R]() extends EndpointInputsOps[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R] {
  def mapIn[II](f: INPUT => II)(g: II => INPUT): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R] = this
}

trait EndpointInputsOps[A, I, E, O, -R] extends EndpointInputsMacros[A, I, E, O, R] {
  type EndpointType[_A, _I, _E, _O, -_R]
}

trait EndpointInputsMacros[A, I, E, O, -R] { this: EndpointInputsOps[A, I, E, O, R] =>
  def mapInTo[CASE_CLASS]: EndpointType[A, CASE_CLASS, E, O, R] =
  macro MapToMacro.generateMapInTo[EndpointType[A, CASE_CLASS, E, O, R], I, CASE_CLASS]

  def n[CASE_CLASS]: CASE_CLASS = macro MapToMacro.nice[CASE_CLASS]
}

object MapToMacro {

  def nice[CASE_CLASS: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    println("In 'nice'")

    val t: Type = weakTypeOf[CASE_CLASS]

//    val classSymbol: ClassSymbol = t.typeSymbol.asClass
//    mprint("classSymbol", classSymbol)
//    val className: TermName = classSymbol.asType.name.toTermName
//    mprint("className", className)
//
//    // ===
//
//    val fullyQualifiedName = t.typeConstructor.toString
//    mprint("fullyQualifiedName", fullyQualifiedName)
//    mprint("fullyQualifiedName parsed", c.parse(fullyQualifiedName))
//
//    // ===
//
//    val constructorSymbol = t.member(termNames.CONSTRUCTOR)
//    mprint("constructorSymbol", constructorSymbol)
//
    // ===

    val constructorMethod = Select(New(Ident(t.dealias.typeSymbol)), termNames.CONSTRUCTOR)
    mprint("constructorMethod", constructorMethod)

    // ===
//
//    val asSeenFrom = classSymbol.asClass.selfType.asSeenFrom(c.enclosingClass.tpe, c.enclosingClass.symbol)
//    mprint("asSeenFrom", asSeenFrom)
//
//    // ====
//
//    val whatCausedExpansion = c.macroApplication
//    mprint("whatCausedExpansion", whatCausedExpansion)
//
//    // ===
//
////    val classToExpand = c.enclosingClass
////    mprint("classToExpand", classToExpand)
//
//    // ==
//
//    val allSymbols = Seq.unfold(t.typeSymbol)( s => if (c != c.mirror.RootClass && s.isClass) Some((s, s.owner)) else None)
//    val allSymbolsReadable = allSymbols.foldRight("")((s, result) => s"$result${s.name}." )
//    mprint("allSymbolsReadable", allSymbolsReadable)

    // ==

    val caseClassUtil = new CaseClassUtil[c.type, CASE_CLASS](c, "mapTo mapping")

    q"$constructorMethod(Some(42))"
  }

  private def mprint(cmd: String, res: Any): Unit = {
    println(s"[NICE] $cmd => $res")
  }

  def generateMapInTo[RESULT, T: c.WeakTypeTag, CASE_CLASS: c.WeakTypeTag](c: blackbox.Context): c.Expr[RESULT] =
    c.Expr[RESULT](generateDelegateMap[T, CASE_CLASS](c)("mapIn"))

  private def generateDelegateMap[T: c.WeakTypeTag, CASE_CLASS: c.WeakTypeTag](
                                                                                c: blackbox.Context
                                                                              )(delegateTo: String): c.Tree = {
    import c.universe._
    val to = MapToMacro.tupleToCaseClass[T, CASE_CLASS](c)
    val from = MapToMacro.tupleFromCaseClass[T, CASE_CLASS](c)

    q"${c.prefix}.${TermName(delegateTo)}($to)($from)"
  }

  private def tupleToCaseClass[TUPLE: c.WeakTypeTag, CASE_CLASS: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._

    val caseClassUtil = new CaseClassUtil[c.type, CASE_CLASS](c, "mapTo mapping")
    val tupleType = weakTypeOf[TUPLE]
    val tupleTypeArgs = tupleType.dealias.typeArgs

    if (caseClassUtil.fields.size == 1) {
      verifySingleFieldCaseClass(c)(caseClassUtil, tupleType)
      q"(t: ${tupleType.dealias}) => ${caseClassUtil.className}(t)"
    } else {
      verifyCaseClassMatchesTuple(c)(caseClassUtil, tupleType, tupleTypeArgs)
      val ctorArgs = (1 to tupleTypeArgs.length).map(idx => q"t.${TermName(s"_$idx")}")
      q"(t: ${tupleType.dealias}) => ${caseClassUtil.className}(..$ctorArgs)"
    }
  }

  private def tupleFromCaseClass[TUPLE: c.WeakTypeTag, CASE_CLASS: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._

    val caseClassUtil = new CaseClassUtil[c.type, CASE_CLASS](c, "mapTo mapping")
    val tupleType = weakTypeOf[TUPLE]

    if (caseClassUtil.fields.size == 1) {
      verifySingleFieldCaseClass(c)(caseClassUtil, tupleType)

    } else {
      verifyCaseClassMatchesTuple(c)(caseClassUtil, tupleType, tupleType.dealias.typeArgs)
    }

    val tupleArgs = caseClassUtil.fields.map(field => q"t.${TermName(s"${field.name}")}")
    val classType = caseClassUtil.classSymbol.asType
    q"(t: $classType) => (..$tupleArgs)"
  }

  private def verifySingleFieldCaseClass[CASE_CLASS](
                                                      c: blackbox.Context
                                                    )(caseClassUtil: CaseClassUtil[c.type, CASE_CLASS], tupleType: c.Type): Unit = {
    val field = caseClassUtil.fields.head
    if (!(field.info.resultType =:= tupleType)) {
      c.abort(
        c.enclosingPosition,
        s"The type doesn't match the type of the case class field: $tupleType, $field"
      )
    }
  }

  private def verifyCaseClassMatchesTuple[CASE_CLASS](c: blackbox.Context)(
    caseClassUtil: CaseClassUtil[c.type, CASE_CLASS],
    tupleType: c.Type,
    tupleTypeArgs: List[c.Type]
  ): Unit = {
    val tupleSymbol = tupleType.typeSymbol
    if (!tupleSymbol.fullName.startsWith("scala.Tuple")) {
      c.abort(c.enclosingPosition, s"Expected source type to be a tuple, but got: $tupleType")
    }

    if (caseClassUtil.fields.size != tupleTypeArgs.size) {
      c.abort(
        c.enclosingPosition,
        s"The arity of the source type doesn't match the arity of the target type: $tupleType, ${caseClassUtil.t}"
      )
    }

    caseClassUtil.fields.zip(tupleTypeArgs).foreach { case (caseClassField, tupleArg) =>
      if (!(caseClassField.info.resultType =:= tupleArg)) {
        c.abort(
          c.enclosingPosition,
          s"The type of the tuple field doesn't match the type of the case class field ($caseClassField): $tupleArg, ${caseClassField.info.resultType}"
        )
      }
    }
  }
}