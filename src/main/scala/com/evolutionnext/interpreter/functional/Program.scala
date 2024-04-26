package com.evolutionnext.interpreter.functional

sealed trait Expression

case class Constant(number: Int) extends Expression

case class Multiply(left: Expression, right: Expression) extends Expression

case class Subtract(left: Expression, right: Expression) extends Expression

case class Sum(left: Expression, right: Expression) extends Expression

object Runner {
    private def program: Expression = Multiply(Constant(40), Sum(Constant(40), Constant(60)))

    private def evaluate(expression: Expression): Int = expression match {
        case Constant(c) => c
        case Sum(left, right) => evaluate(left) + evaluate(right)
        case Subtract(left, right) => evaluate(left) - evaluate(right)
        case Multiply(left, right) => evaluate(left) * evaluate(right)
    }

    @main
    def evaluateFunctionProgram(): Unit = {
        val result: Int = evaluate(program)
        println(result)
    }
}

