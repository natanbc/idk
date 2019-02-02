package com.github.natanbc.idk.parser

import com.github.natanbc.idk.ast.Node
import com.github.natanbc.idk.lexer.Token

abstract class PrefixParselet {
    def parse(parser: Parser, token: Token): Node
}
