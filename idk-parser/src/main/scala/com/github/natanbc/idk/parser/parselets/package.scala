package com.github.natanbc.idk.parser

import com.github.natanbc.idk.ast._
import com.github.natanbc.idk.lexer.{SyntaxException, Token, TokenType}

import scala.collection.mutable.ListBuffer

package object parselets {
    private val infixParselets = Seq[(TokenType, InfixParselet)](
        TokenType.plus -> BinaryOperatorParselet(Precedence.SUM, isRight = false, Add),
        TokenType.minus -> BinaryOperatorParselet(Precedence.SUM, isRight = false, Sub),
        TokenType.asterisk -> BinaryOperatorParselet(Precedence.PRODUCT, isRight = false, Mul),
        TokenType.slash -> BinaryOperatorParselet(Precedence.PRODUCT, isRight = false, Div),
        TokenType.percent -> BinaryOperatorParselet(Precedence.PRODUCT, isRight = false, Mod),
        TokenType.caret -> BinaryOperatorParselet(Precedence.EXPONENT, isRight = false, Pow),
        TokenType.eq -> BinaryOperatorParselet(Precedence.CONDITIONAL, isRight = false, Eq),
        TokenType.neq -> BinaryOperatorParselet(Precedence.CONDITIONAL, isRight = false, Neq),
        TokenType.greater -> BinaryOperatorParselet(Precedence.CONDITIONAL, isRight = false, Greater),
        TokenType.greaterEq -> BinaryOperatorParselet(Precedence.CONDITIONAL, isRight = false, GreaterEq),
        TokenType.smaller -> BinaryOperatorParselet(Precedence.CONDITIONAL, isRight = false, Smaller),
        TokenType.smallerEq -> BinaryOperatorParselet(Precedence.CONDITIONAL, isRight = false, SmallerEq),
        TokenType.and -> BinaryOperatorParselet(Precedence.CONJUNCTION, isRight = false, And),
        TokenType.or -> BinaryOperatorParselet(Precedence.DISJUNCTION, isRight = false, Or),
        TokenType.left_paren -> CallParselet,
        TokenType.assign -> AssignParselet,
        TokenType.dot -> KnownMemberParselet,
        TokenType.left_bracket -> DynamicMemberParselet,
    )

    private val prefixParselets = Seq[(TokenType, PrefixParselet)](
        (TokenType.int, (_,t)=>IntConstant(t.value.toLong)),
        (TokenType.hex_int, (_,t)=>IntConstant(java.lang.Long.parseLong(t.value, 16))),
        (TokenType.binary_int, (_,t)=>IntConstant(java.lang.Long.parseLong(t.value, 2))),
        (TokenType.float, (_,t)=>FloatConstant(t.value.toDouble)),
        (TokenType.string, (_,t)=>StringConstant(t.value)),
        (TokenType.t_true, (_,_)=>BooleanConstant(true)),
        (TokenType.t_false, (_,_)=>BooleanConstant(false)),
        (TokenType.nil, (_,_)=>NilConstant),
        (TokenType.left_paren, (parser,_)=>{
            val n = parser.parseExpression()
            parser.consume(TokenType.right_paren)
            n
        }),
        (TokenType.left_brace, (parser,_)=>{
            val l = new collection.mutable.ListBuffer[(Node, Node)]()
            if(!parser.matches(TokenType.right_brace)) {
                do {
                    if(parser.matches(TokenType.left_bracket)) {
                        val expr = parser.parseExpression()
                        parser.consume(TokenType.right_bracket)
                        parser.consume(TokenType.assign)
                        val value = parser.parseExpression()
                        l += expr->value
                    } else {
                        val k = parser.consume(TokenType.identifier).value
                        val next = parser.peek().tpe
                        if(next == TokenType.comma || next == TokenType.right_brace) {
                            l += StringConstant(k)->Identifier(k)
                        } else {
                            parser.consume(TokenType.assign)
                            val value = parser.parseExpression()
                            l += StringConstant(k)->value
                        }
                    }
                } while(parser.matches(TokenType.comma))
                parser.consume(TokenType.right_brace)
            }
            ObjectLiteral(l.toList)
        }),
        (TokenType.left_bracket, (parser,_)=>{
            val l = new collection.mutable.ListBuffer[Node]()
            if(!parser.matches(TokenType.right_bracket)) {
                do {
                    l += parser.parseExpression()
                } while(parser.matches(TokenType.comma))
                parser.consume(TokenType.right_bracket)
            }
            ArrayLiteral(l.toList)
        }),
        (TokenType.identifier, (_,t)=>Identifier(t.value)),
        (TokenType.t_return, (p,_)=>Return(p.parseExpression())),
        TokenType.minus -> PrefixOperatorParselet(Precedence.PREFIX, Neg),
        TokenType.plus -> PrefixOperatorParselet(Precedence.PREFIX, identity),
        TokenType.negation -> PrefixOperatorParselet(Precedence.PREFIX, Negate),
        TokenType.t_if -> IfParselet,
        TokenType.t_while -> WhileParselet,
        TokenType.function -> FunctionParselet,
        (TokenType.let, (p,_)=>Let(p.consume(TokenType.identifier).value)),
        (TokenType.global, (p,_)=>Global(p.consume(TokenType.identifier).value)),
        (TokenType.varargs, (p,_)=>Unpack(p.parseExpression())),
    )

    def register(p: Parser): Unit = {
        infixParselets.foreach(e=>p.register(e._1, e._2))
        prefixParselets.foreach(e=>p.register(e._1, e._2))
    }

    private case class PrefixOperatorParselet(precedence: Int, nodeBuilder: Node=>Node) extends PrefixParselet {
        override def parse(parser: Parser, token: Token): Node = nodeBuilder(parser.parseExpression(precedence))
    }

    private case class BinaryOperatorParselet(precedence: Int, isRight: Boolean, nodeBuilder: (Node, Node)=>Node) extends InfixParselet {
        override def getPrecedence: Int = precedence

        override def parse(parser: Parser, left: Node, token: Token): Node = {
            nodeBuilder(left, parser.parseExpression(precedence - (if(isRight) 1 else 0)))
        }
    }

    private object CallParselet extends InfixParselet {
        override def parse(parser: Parser, left: Node, token: Token): Node = {
            val l = new collection.mutable.ListBuffer[Node]()

            if(!parser.matches(TokenType.right_paren)) {
                do {
                    l += parser.parseExpression()
                } while(parser.matches(TokenType.comma))
                parser.consume(TokenType.right_paren)
            }

            val list = l.toList

            if(list.nonEmpty && list.dropRight(1).exists(_.isInstanceOf[Unpack])) {
                throw new SyntaxException("Unpack must be only on the last argument of a function call")
            }

            Call(left, list)
        }

        override def getPrecedence: Int = Precedence.CALL
    }

    private object AssignParselet extends InfixParselet {
        override def parse(parser: Parser, left: Node, token: Token): Node = {
            val value = parser.parseExpression(Precedence.ASSIGNMENT)
            left match {
                case i: Identifier => Assign(i, value)
                case m: Member => Assign(m, value)
                case g: Global => Assign(g, value)
                case l: Let => Assign(l, value)
                case Assign(a, b) => Assign(a, Assign(b, value))
                case _ => throw new SyntaxException("left hand side of an assignment must be an identifier or member")
            }
        }

        override def getPrecedence: Int = Precedence.ASSIGNMENT
    }

    private object IfParselet extends PrefixParselet {
        override def parse(parser: Parser, token: Token): Node = {
            val cond = parser.parseExpression()

            val ifTrue = {
                val leftBrace = parser.matches(TokenType.left_brace)
                if(leftBrace && parser.matches(TokenType.right_brace)) {
                    Body(List())
                } else {
                    val b = new collection.mutable.ListBuffer[Node]()
                    do {
                        b += parser.parseExpression()
                    } while(leftBrace && !parser.matches(TokenType.right_brace))
                    Body(b.toList)
                }
            }
            if(parser.matches(TokenType.t_else, throwOnEof = false)) {
                If(cond, ifTrue, {
                    val leftBrace = parser.matches(TokenType.left_brace)
                    if(leftBrace && parser.matches(TokenType.right_brace)) {
                        Body(List())
                    } else {
                        val b = new collection.mutable.ListBuffer[Node]()
                        do {
                            b += parser.parseExpression()
                        } while(!parser.matches(TokenType.right_brace) && leftBrace)
                        Body(b.toList)
                    }
                })
            } else {
                If(cond, ifTrue, Body(List()))
            }
        }
    }

    private object WhileParselet extends PrefixParselet {
        override def parse(parser: Parser, token: Token): Node = {
            val cond = parser.parseExpression()

            val body = {
                val leftBrace = parser.matches(TokenType.left_brace)
                if(leftBrace && parser.matches(TokenType.right_brace)) {
                    Body(List())
                } else {
                    val b = new collection.mutable.ListBuffer[Node]()
                    do {
                        b += parser.parseExpression()
                    } while(leftBrace && !parser.matches(TokenType.right_brace))
                    Body(b.toList)
                }
            }
            val elseBody = {
                val hasBlock = parser.matches(TokenType.t_else)
                val leftBrace = parser.matches(TokenType.left_brace)
                if(!hasBlock || (leftBrace && parser.matches(TokenType.right_brace))) {
                    Body(List())
                } else {
                    val b = new collection.mutable.ListBuffer[Node]()
                    do {
                        b += parser.parseExpression()
                    } while(leftBrace && !parser.matches(TokenType.right_brace))
                    Body(b.toList)
                }
            }
            While(cond, body, elseBody)
        }
    }

    private object FunctionParselet extends PrefixParselet {
        override def parse(parser: Parser, token: Token): Node = {
            val annotations = new ListBuffer[String]()
            if(parser.matches(TokenType.left_bracket)) {
                do {
                    val isIdentifier = parser.peek().tpe == TokenType.identifier
                    if(isIdentifier) {
                        annotations += parser.consume(TokenType.identifier).value
                    } else {
                        annotations += parser.consume(TokenType.decorator).value
                    }
                } while(parser.matches(TokenType.comma))
                parser.consume(TokenType.right_bracket)
            }
            val maybeName = parser.peek()
            val name = if(maybeName.tpe == TokenType.identifier) {
                parser.consume(TokenType.identifier).value
            } else {
                null
            }
            parser.consume(TokenType.left_paren)
            val args = new collection.mutable.ListBuffer[String]()
            var isVarargs = false
            if(!parser.matches(TokenType.right_paren)) {
                do {
                    if(isVarargs) {
                        throw new SyntaxException("No arguments may be declared after a varargs argument")
                    }
                    if(parser.matches(TokenType.varargs)) {
                        isVarargs = true
                    }
                    args += parser.consume(TokenType.identifier).value
                } while(parser.matches(TokenType.comma))
                parser.consume(TokenType.right_paren)
            }
            val leftBrace = parser.matches(TokenType.left_brace)
            val body = if(leftBrace) {
                if(parser.matches(TokenType.right_brace)) {
                    Body(List())
                } else {
                    val b = new collection.mutable.ListBuffer[Node]()
                    do {
                        b += parser.parseExpression()
                    } while(!parser.matches(TokenType.right_brace))
                    Body(b.toList)
                }
            } else {
                parser.parseExpression()
            }
            Function(name, args.toList, body, isVarargs, annotations.toList)
        }
    }

    private object KnownMemberParselet extends InfixParselet {
        override def parse(parser: Parser, left: Node, token: Token): Node = {
            Member(StringConstant(parser.consume(TokenType.identifier).value), left)
        }

        override def getPrecedence: Int = Precedence.MEMBER_ACCESS
    }

    private object DynamicMemberParselet extends InfixParselet {
        override def parse(parser: Parser, left: Node, token: Token): Node = {
            val n = Member(parser.parseExpression(), left)
            parser.consume(TokenType.right_bracket)
            n
        }

        override def getPrecedence: Int = Precedence.MEMBER_ACCESS
    }
}
