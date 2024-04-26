package com.evolutionnext.interpreter.typed

sealed trait Expression[A]

case class Constant[A](number: A) extends Expression[A]

case class Multiply[A](left: Expression[A], right: Expression[A]) extends Expression[A]

case class Divide[A](left: Expression[A], right: Expression[A]) extends Expression[A]

case class Subtract[A](left: Expression[A], right: Expression[A]) extends Expression[A]

case class Sum[A](left: Expression[A], right: Expression[A]) extends Expression[A]

case class Error(message: String)

object Runner {
    private val program: Expression[Int] = Multiply(Constant(40), Sum(Constant(40), Constant(60)))
    private val errorProgram: Expression[Int] = Multiply(Constant(40), Divide(Constant(3), Constant(0)))
    private val floatProgram: Expression[Float] = Multiply(Constant(40.0), Divide(Constant(30.0), Constant(12.0)))
    private val doubleProgram: Expression[Double] = Multiply(Constant(40.0), Divide(Constant(30.0), Constant(12.0)))

    private def evaluate[A](expression: Expression[A])(using F: Fractional[A]): Either[Error, A] = expression match {
        case Constant(c) => Right(c)
        case Sum(left, right) =>
            for {
                x <- evaluate(left)
                y <- evaluate(right)
            } yield F.plus(x, y)
        case Subtract(left, right) =>
            for {
                x <- evaluate(left)
                y <- evaluate(right)
            } yield F.minus(x, y)
        case Multiply(left, right) =>
            for {
                x <- evaluate(left)
                y <- evaluate(right)
            } yield F.times(x, y)
        case Divide(left, right) =>
            for {
                x <- evaluate(left)
                y <- evaluate(right)
            } yield F.div(x, y)
    }

    @main
    def evaluateFunctionProgram(): Unit = {
        //val result = evaluate(program)
        //val errorResult = evaluate(errorProgram)
        val floatResult = evaluate(floatProgram)
        val doubleResult = evaluate(doubleProgram)
        println(floatResult)
        println(doubleResult)
    }
}




