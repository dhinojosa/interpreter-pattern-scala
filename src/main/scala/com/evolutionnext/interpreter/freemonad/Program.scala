package com.evolutionnext.interpreter.freemonad

import cats.free.Free
import cats.{~>, Id}

// Step 1: Define the Algebra as a sealed trait
sealed trait Expression[T]
case class Sum(a: Int, b: Int) extends Expression[Int]
case class Subtract(a: Int, b: Int) extends Expression[Int]
case class Multiply(a: Int, b: Int) extends Expression[Int]
case class Negate(a: Int) extends Expression[Int]

// Step 2: Create the Free Monad using the Algebra
type ExpressionF[A] = Free[Expression, A]

// Step 3: Define Smart Constructors for the operations
def sum(a: Int, b: Int): ExpressionF[Int] = Free.liftF(Sum(a, b))
def subtract(a: Int, b: Int): ExpressionF[Int] = Free.liftF(Subtract(a, b))
def multiply(a: Int, b: Int): ExpressionF[Int] = Free.liftF(Multiply(a, b))
def negate(a: Int): ExpressionF[Int] = Free.liftF(Negate(a))

// Step 4: Create a sample computation
val complexExpression: ExpressionF[Int] = for {
    sumResult <- sum(1, 2)
    negResult <- negate(sumResult)
    finalResult <- multiply(negResult, 5)
} yield finalResult

// Step 5: Define an Interpreter
object ExpressionInterpreter extends (Expression ~> Id) {
    def apply[A](fa: Expression[A]): Id[A] = fa match {
        case Sum(a, b) => a + b
        case Subtract(a, b) => a - b
        case Multiply(a, b) => a * b
        case Negate(a) => -a
    }
}

@main
def runFreeMonad(): Unit = {
    // Run the computation using the interpreter
    val result: Int = complexExpression.foldMap(ExpressionInterpreter)
    println(result) // This should output the result of (1+2) * -5 which is -15
}
