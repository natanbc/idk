package com.github.natanbc.idk.lexer

case class TokenType private(name: String, value: Option[String] = None) {
    def this(name: String, value: String) = this(name, Some(value))

    override def toString: String = name
}

//t_name if name is a keyword
object TokenType {
    final val int = TokenType("int")
    final val hex_int = TokenType("hex_int")
    final val binary_int = TokenType("binary_int")
    final val float = TokenType("float")
    final val string = TokenType("string")
    final val identifier = TokenType("identifier")
    final val decorator = TokenType("decorator")
    final val let = TokenType("let", "let")
    final val global = TokenType("global", "global")
    final val t_true = TokenType("true", "true")
    final val t_false = TokenType("false", "false")
    final val nil = TokenType("nil", "nil")

    final val plus = TokenType("plus", "+")
    final val minus = TokenType("minus", "-")
    final val asterisk = TokenType("asterisk", "*")
    final val percent = TokenType("percent", "%")
    final val slash = TokenType("slash", "/")
    final val caret = TokenType("caret", "^")
    final val left_paren = TokenType("left_paren", "(")
    final val right_paren = TokenType("right_paren", ")")
    final val left_brace = TokenType("left_brace", "{")
    final val right_brace = TokenType("right_brace", "}")
    final val left_bracket = TokenType("left_bracket", "[")
    final val right_bracket = TokenType("right_bracket", "]")
    final val dot = TokenType("dot", ".")
    final val comma = TokenType("comma", ",")
    final val assign = TokenType("assign", "=")
    final val function = TokenType("function", "function")
    final val eq = TokenType("eq", "==")
    final val neq = TokenType("neq", "!=")
    final val negation = TokenType("negation", "!")
    final val t_if = TokenType("if", "if")
    final val t_else = TokenType("else", "else")
    final val t_while = TokenType("while", "while")
    final val t_return = TokenType("return", "return")
    final val greater = TokenType(">", ">")
    final val greaterEq = TokenType(">=", ">=")
    final val smaller = TokenType("<", "<")
    final val smallerEq = TokenType("<=", "<=")
    final val and = TokenType("and", "&&")
    final val or = TokenType("or", "||")
    final val comment = TokenType("comment", "")
    final val eof = TokenType("eof", "")
    final val varargs = TokenType("varargs", "...")

    def apply(name: String, value: String) = new TokenType(name, Some(value))
}
