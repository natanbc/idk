package com.github.natanbc.idk.parser

import com.github.natanbc.idk.ast.Node
import com.github.natanbc.idk.lexer.Token

abstract class InfixParselet {
    def parse(parser: Parser, left: Node, token: Token): Node

    def getPrecedence: Int
}
