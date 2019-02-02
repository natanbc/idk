package com.github.natanbc.idk.lexer

import java.io.Reader

import scala.annotation.switch

class Lexer(r: Reader, maxHistorySize: Int) {
    private var lineNumber = 1
    private var columnNumber = 0
    private val history = new collection.mutable.ListBuffer[Entry]()
    private var previous = 0
    private var eof = false

    val tokenHistory = new collection.mutable.ListBuffer[Token]()

    def readToken(): Token = {
        if(tokenHistory.nonEmpty) return tokenHistory.remove(0)
        var c = advance()
        while(Character.isWhitespace(c) || c == '\t') c = advance()
        (c: @switch) match {
            case '(' => Token(getPosition, TokenType.left_paren)
            case ')' => Token(getPosition, TokenType.right_paren)

            case '{' => Token(getPosition, TokenType.left_brace)
            case '}' => Token(getPosition, TokenType.right_brace)

            case '[' => Token(getPosition, TokenType.left_bracket)
            case ']' => Token(getPosition, TokenType.right_bracket)

            case '+' => Token(getPosition, TokenType.plus)
            case '-' => Token(getPosition, TokenType.minus)
            case '*' => Token(getPosition, TokenType.asterisk)
            case '/' => Token(getPosition, TokenType.slash)
            case '%' => Token(getPosition, TokenType.percent)
            case '^' => Token(getPosition, TokenType.caret)

            case '.' =>
                if(check('.')) {
                    if(check('.')) {
                        Token(getPosition.copy(column = columnNumber - 2), TokenType.varargs)
                    } else {
                        throw new SyntaxException("unexpected .. (did you mean ...?)", getPosition)
                    }
                } else {
                    Token(getPosition, TokenType.dot)
                }
            case ',' => Token(getPosition, TokenType.comma)

            case '=' =>
                if(check('=')) {
                    Token(getPosition.copy(column = columnNumber - 1), TokenType.eq)
                } else {
                    Token(getPosition, TokenType.assign)
                }

            case '!' =>
                if(check('=')) {
                    Token(getPosition.copy(column = columnNumber - 1), TokenType.neq)
                } else {
                    Token(getPosition, TokenType.negation)
                }

            case '>' =>
                if(check('=')) {
                    Token(getPosition.copy(column = columnNumber - 1), TokenType.greaterEq)
                } else {
                    Token(getPosition, TokenType.greater)
                }

            case '<' =>
                if(check('=')) {
                    Token(getPosition.copy(column = columnNumber - 1), TokenType.smallerEq)
                } else {
                    Token(getPosition, TokenType.smaller)
                }

            case '&' =>
                if(check('&')) {
                    Token(getPosition.copy(column = columnNumber - 1), TokenType.and)
                } else {
                    throw new SyntaxException("Unexpected &", getPosition)
                }

            case '|' =>
                if(check('|')) {
                    Token(getPosition.copy(column = columnNumber - 1), TokenType.or)
                } else {
                    throw new SyntaxException("Unexpected |", getPosition)
                }

            case '"' => readString('"')
            case '\'' => readString('\'')
            case '`' => readString('`')

            case '@' =>
                val c = peek()
                if(c.isLetter) {
                    Token(getPosition, TokenType.decorator, readName('@').value)
                } else {
                    throw new SyntaxException("Unexpected " + c, getPosition)
                }

            case '0' =>
                (peek(): @switch) match {
                    case 'x'|'X' => advance(); readHexNumber()
                    case 'b'|'B' => advance(); readBinaryNumber()
                    case other if other.isDigit => advance(); readNumber(other)
                    case other if other == '.' => readNumber('0')
                    case _ => Token(getPosition, TokenType.int, "0")
                }

            case '\u0000' => Token(getPosition, TokenType.eof)
            case '\uFFFF' => Token(getPosition, TokenType.eof)

            case o =>
                if(Character.isDigit(o)) {
                    readNumber(o)
                } else if(Character.isLetter(o)) {
                    readName(o)
                } else {
                    throw new SyntaxException(s"unexpected character '$o'", getPosition)
                }
        }
    }

    def peekToken(): Token = {
        val t = readToken()
        tokenHistory.insert(0, t)
        t
    }

    private def readString(quote: Char): Token = {
        var c = '\u0000'
        val sb = new StringBuilder
        val pos = getPosition
        while(true) {
            if(previous == 0 && eof) throw new SyntaxException("Unfinished string", getPosition)
            c = this.advance()
            c match {
                case 0 =>
                case '\r' =>
                case '\n' =>
                    throw new SyntaxException("Unterminated string")
                case '\\' =>
                    c = this.advance()
                    c match {
                        case 'b' =>
                            sb.append('\b')
                        case 't' =>
                            sb.append('\t')
                        case 'n' =>
                            sb.append('\n')
                        case 'f' =>
                            sb.append('\f')
                        case 'r' =>
                            sb.append('\r')
                        case 'u' =>
                            val s = advance(4)
                            try
                                sb.append(Integer.parseInt(s, 16).toChar)
                            catch {
                                case _: NumberFormatException =>
                                    throw new SyntaxException(s"Illegal escape '$s'")
                            }
                        case '"' =>
                        case '\'' =>
                        case '\\' =>
                        case '/' =>
                            sb.append(c)
                        case _ =>
                            throw new SyntaxException(s"Illegal escape '$c'")
                    }
                case _ =>
                    if(c == quote) {
                        return Token(pos, TokenType.string, sb.toString())
                    }
                    sb.append(c)
            }
        }
        throw new AssertionError()
    }

    private def readName(c: Char): Token = {
        val sb = new StringBuilder().append(c)

        val pos = getPosition

        var chr = advance()
        while(Character.isLetterOrDigit(chr) || chr == '_') {
            sb.append(chr)
            chr = advance()
        }
        back()

        sb.toString() match {
            case "if" => Token(pos, TokenType.t_if)
            case "else" => Token(pos, TokenType.t_else)
            case "while" => Token(pos, TokenType.t_while)
            case "true" => Token(pos, TokenType.t_true)
            case "false" => Token(pos, TokenType.t_false)
            case "nil" => Token(pos, TokenType.nil)
            case "fn" => Token(pos, TokenType.function)
            case "return" => Token(pos, TokenType.t_return)
            case "let" => Token(pos, TokenType.let)
            case "global" => Token(pos, TokenType.global)
            case s => Token(pos, TokenType.identifier, s)
        }
    }

    private def readHexNumber(): Token = {
        var c = 0.toChar
        val sb = new StringBuilder()
        val pos = getPosition
        while(true) {
            c = advance()
            if(Character.isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) {
                sb.append(c)
            } else {
                back()
                val s = sb.dropWhile(_ == '0')
                //8 * 2 chars per byte
                if(s.length > 16) throw new SyntaxException(s"Number too large (hex numbers maximum length is 16): 0x$sb")
                return Token(pos, TokenType.hex_int, if(s.isEmpty) "0" else s.toString())
            }
        }
        throw new AssertionError()
    }

    private def readBinaryNumber(): Token = {
        var c = 0.toChar
        val sb = new StringBuilder()
        val pos = getPosition
        while(true) {
            c = advance()
            if(c == '0' || c == '1') {
                sb.append(c)
            } else {
                back()
                val s = sb.dropWhile(_ == '0')
                //64 bits
                if(s.length > 64) throw new SyntaxException(s"Number too large (binary numbers maximum length is 64): 0b$sb")
                return Token(pos, TokenType.binary_int, if(s.isEmpty) "0" else s.toString())
            }
        }
        throw new AssertionError()
    }

    private def readNumber(init: Char): Token = {
        var point = false
        var c = 0.toChar
        val sb = new StringBuilder()
        val pos = getPosition.copy(column = columnNumber - 1)
        sb.append(init)
        while(true) {
            c = advance()
            if(Character.isDigit(c)) sb.append(c)
            else c match {
                case '.' =>
                    if(point) {
                        back()
                        return Token(pos, TokenType.float, sb.toString())
                    }
                    if(!Character.isDigit(peek())) {
                        back()
                        return Token(pos, TokenType.int, sb.toString())
                    }
                    sb.append(c)
                    point = true
                case '_' =>
                case _ =>
                    back()
                    if(point) {
                        return Token(pos, TokenType.float, sb.toString())
                    } else {
                        return Token(pos, TokenType.int, sb.toString())
                    }

            }
        }
        throw new AssertionError()
    }

    @inline private def getPosition = Position(lineNumber, columnNumber)

    @inline private def check(c: Char): Boolean = {
        if(advance() == c) return true
        back()
        false
    }

    @inline private def peek(): Char = {
        val c = advance()
        back()
        c
    }

    @inline private def back(): Unit = previous += 1

    private def advance(): Char = {
        if(previous != 0) {
            previous -= 1
            val e = history.remove(0)
            lineNumber = e.position.lineNumber
            columnNumber = e.position.column
            return e.value
        }
        val c = {
            var i = r.read()
            if(i < 0) {
                eof = true
                i = 0
            }
            i
        }
        if(c == '\n') {
            lineNumber += 1
            columnNumber = 0
        } else {
            columnNumber += 1
        }
        if(history.lengthCompare(maxHistorySize) > 0) {
            history.remove(history.length - 1)
        }
        history.insert(0, Entry(Position(lineNumber, columnNumber), c.toChar))
        c.toChar
    }

    private def advance(n: Int): String = {
        if(n == 0) return ""
        val chars = new Array[Char](n)
        var pos = 0
        while(pos < n) {
            chars(pos) = advance()
            if(previous == 0 && eof) throw new SyntaxException("Substring bounds error", getPosition)
            pos += 1
        }
        new String(chars)
    }
}

private case class Entry(position: Position, value: Char)
