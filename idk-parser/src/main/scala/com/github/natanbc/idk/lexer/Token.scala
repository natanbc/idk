package com.github.natanbc.idk.lexer

case class Token(position: Position, tpe: TokenType, value: String) {
    def this(position: Position, tpe: TokenType) = this(position, tpe, tpe.value.getOrElse(throw new AssertionError("missing value")))

    override def toString: String = s"Token(pos = $position, type = $tpe, value = '$value')"
}

object Token {
    def apply(position: Position, tpe: TokenType) = new Token(position, tpe)
}

case class Position(lineNumber: Int, column: Int) {
    override def toString: String = s"($lineNumber:$column)"
}
