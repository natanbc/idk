package com.github.natanbc.idk.lexer

class SyntaxException(s: String) extends RuntimeException(s) {
    def this(c: Char, c2: Char, position: Position) = this(s"""$position: Expected "$c", got "$c2"""")

    def this(s: String, position: Position) = this(s"""$position: $s""")
}
