package com.github.natanbc.idk.parser

import java.io.StringReader

import com.github.natanbc.idk.ast.Node
import com.github.natanbc.idk.lexer.{Lexer, Token, TokenType}

import scala.collection.mutable

class Parser(s: String) {
    private val l = new Lexer(new StringReader(s), 10)

    private val prefixParselets = new mutable.HashMap[TokenType, PrefixParselet]()
    private val infixParselets = new mutable.HashMap[TokenType, InfixParselet]()

    @inline def parseExpression(): Node = parseExpression(0)

    //returns null on eof
    def parseExpression(precedence: Int): Node = {
        var t = l.readToken()
        if(t.tpe == TokenType.eof) return null
        val prefix = prefixParselets.getOrElse(t.tpe, null)
        if(prefix == null) throw new ParseException(s"No prefix parselet for ${t.tpe}")

        var left = prefix.parse(this, t)

        while(precedence < getPrecedence) {
            t = l.readToken()
            val infix = infixParselets(t.tpe)
            left = infix.parse(this, left, t)
        }

        left
    }

    @inline private def getPrecedence: Int = {
        val t = l.peekToken()
        val parser = infixParselets.getOrElse(t.tpe, null)
        if(parser != null) return parser.getPrecedence
        0
    }

    def matches(tpe: TokenType, throwOnEof: Boolean = true): Boolean = {
        val a = l.peekToken().tpe
        if(throwOnEof && a == TokenType.eof) throw new ParseException("unexpected eof")
        val v = a == tpe
        if(v) l.readToken()
        v
    }

    def peek(): Token = l.peekToken()

    def consume(tpe: TokenType): Token = {
        val t = l.peekToken()
        //use reference equality
        if(t.tpe ne tpe) {
            throw new ParseException(s"Expected token of type $tpe, got $t")
        }

        l.readToken()
    }

    def register(tpe: TokenType, parselet: PrefixParselet): Unit = {
        prefixParselets(tpe) = parselet
    }

    def register(tpe: TokenType, parselet: InfixParselet): Unit = {
        infixParselets(tpe) = parselet
    }
}
