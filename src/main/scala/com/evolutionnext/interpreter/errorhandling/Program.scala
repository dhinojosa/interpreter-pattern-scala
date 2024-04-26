package com.evolutionnext.interpreter.errorhandling

sealed trait Expression

case class Constant(number: Int) extends Expression

case class Multiply(left: Expression, right: Expression) extends Expression

case class Divide(left: Expression, right: Expression) extends Expression

case class Subtract(left: Expression, right: Expression) extends Expression

case class Sum(left: Expression, right: Expression) extends Expression


case class Error(message: String)

object Runner {
    private val program: Expression = Multiply(Constant(40), Sum(Constant(40), Constant(60)))
    private val errorProgram: Expression = Multiply(Constant(40), Divide(Constant(3), Constant(0)))

    private def evaluate(expression: Expression): Either[Error, Int] = expression match {
        case Constant(c) => Right(c)
        case Sum(left, right) => for {
            x <- evaluate(left)
            y <- evaluate(right)
        } yield x + y
        case Subtract(left, right) => for {
            x <- evaluate(left)
            y <- evaluate(right)
        } yield x - y
        case Multiply(left, right) => for {
            x <- evaluate(left)
            y <- evaluate(right)
        } yield x * y
        case Divide(left, right) =>
            val errorOrInt = evaluate(right)
            if (errorOrInt.isLeft) then
                errorOrInt.left.map(error => Error(error.message + "Division by Zero"))
            else if (errorOrInt == Right(0))
                Left(Error("Division by zero"))
            else {
                for {
                    x <- evaluate(left)
                    y <- errorOrInt
                } yield x / y
            }
    }

    @main
    def evaluateFunctionProgram(): Unit = {
        val result = evaluate(program)
        println(result)
        val errorResult = evaluate(errorProgram)
        println(errorResult)
    }
}

