package lang.calc

import one.wabbit.parsing.CharInput
import one.wabbit.parsing.Spanned
import one.wabbit.parsing.TextAndPosSpan

typealias CalcSpan = TextAndPosSpan

object Calc {
    enum class Op {
        ADD, SUB, MUL, DIV, POW, MOD;

        companion object {
            fun fromChar(char: Char): Op? {
                return when (char) {
                    '+' -> ADD
                    '-' -> SUB
                    '*' -> MUL
                    '/' -> DIV
                    '^' -> POW
                    '%' -> MOD
                    else -> null
                }
            }
        }
    }

    sealed interface Statement {
        data class Eval(val expression: Spanned<CalcSpan, Expression>) : Statement

        // x = sin(y)
        // f(x) = sin(x) + 1
        data class Assign(
            val name: Spanned<CalcSpan, String>,
            val args: Spanned<CalcSpan, List<String>>?,
            val value: Spanned<CalcSpan, Expression>
        ) : Statement

        // x += sin(y)
        data class AssignOp(
            val name: Spanned<CalcSpan, String>,
            val op: Spanned<CalcSpan, Op>,
            val value: Spanned<CalcSpan, Expression>
        ) : Statement
    }

    sealed interface Expression {
        data class Number(val value: Spanned<CalcSpan, Double>) : Expression
        data class Call(
            val name: Spanned<CalcSpan, String>,
            val args: Spanned<CalcSpan, List<Expression>>?
        ) : Expression

        data class Binary(
            val left: Spanned<CalcSpan, Expression>,
            val op: Spanned<CalcSpan, Op>,
            val right: Spanned<CalcSpan, Expression>
        ) : Expression
    }

    object Parser {
        private val NAME_RE = Regex("[a-zA-Z_][a-zA-Z0-9_]*")

        fun parse(str: String): Statement {
            val input = CharInput.withTextAndPosSpans(str)
            if ('=' in str) {
                val index = str.indexOf('=')
                require(index > 0) { "Invalid assignment" }
                val op = Op.fromChar(str[index - 1])

                if (op == null) {
                    // Attempt to parse as function assignment.
                    skipWS(input)
                    val name = readIdentifier(input)
                    skipWS(input)
                    val args = if (input.current == '(') {
                        input.advance()
                        readArgs(input)
                    } else {
                        null
                    }
                    skipWS(input)
                    require(input.current == '=') { "Invalid assignment" }
                    input.advance()
                    skipWS(input)
                    val value = readExpression(input)
                    skipWS(input)
                    require(input.current == CharInput.EOB) { "Unexpected character" }
                    return Statement.Assign(name, args, value)
                } else {
                    // Attempt to parse as Op assignment.
                    skipWS(input)
                    val name = readIdentifier(input)
                    skipWS(input)
                    val op = readOp(input)
                    require(input.current == '=') { "Invalid assignment" }
                    input.advance()
                    skipWS(input)
                    val value = readExpression(input)
                    skipWS(input)
                    require(input.current == CharInput.EOB) { "Unexpected character" }
                    return Statement.AssignOp(name, op, value)
                }
            } else {
                val expression = readExpression(input)
                return Statement.Eval(expression)
            }
        }

        fun skipWS(input: CharInput<CalcSpan>) {
            while (input.current.isWhitespace()) {
                input.advance()
            }
        }

        fun readOp(input: CharInput<CalcSpan>): Spanned<CalcSpan, Op> {
            skipWS(input)
            val start = input.mark()
            val op = Op.fromChar(input.current)
            require(op != null) { "Invalid operator" }
            input.advance()
            return Spanned(input.capture(start), op)
        }

        fun readArgs(input: CharInput<CalcSpan>): Spanned<CalcSpan, List<String>> {
            val start = input.mark()
            val args = mutableListOf<String>()
            while (true) {
                skipWS(input)
                if (input.current == ')') {
                    input.advance()
                    break
                }
                val arg = readIdentifier(input)
                args.add(arg.span.raw)
                skipWS(input)
                if (input.current == ',') {
                    input.advance()
                } else {
                    require(input.current == ')') { "Expected ',' or ')'" }
                }
            }
            return Spanned(input.capture(start), args)
        }

        fun readIdentifier(input: CharInput<CalcSpan>): Spanned<CalcSpan, String> {
            val start = input.mark()
            require(input.current.isLetter() || input.current == '_') { "Invalid identifier" }
            input.advance()
            while (input.current.isLetterOrDigit() || input.current == '_') {
                input.advance()
            }
            val span = input.capture(start)
            return Spanned(span, span.raw)
        }

        internal fun readIntegerOrReal(input: CharInput<CalcSpan>): Spanned<CalcSpan, Double>? {
            val start = input.mark()

            while (true) {
                val char = input.current
                when {
                    char.isDigit() -> {
                        input.advance()
                    }

                    char == '.' -> {
                        input.advance()
                        val r = readReal(input, start)?.span
                        require(r != null) { "Invalid real number" }
                        return Spanned(r, r.raw.toDouble())
                    }

                    else -> {
                        // INCLUDES char == CharInput.EOB
                        val r = input.capture(start)
                        return Spanned(r, r.raw.toDouble())
                    }
                }
            }
        }

        internal fun readReal(input: CharInput<CalcSpan>, startMark: CharInput.Mark): Spanned<CalcSpan, String>? {
            // Supports 1.0, 1.0e10, 1.0e-10, 1.0e+10

            while (true) {
                val char = input.current
                // println("readReal: ${input}")
                when {
                    char == CharInput.EOB || char == '>' || char.isWhitespace() || char == '/' -> {
                        val span = input.capture(startMark)
                        return Spanned(span, span.raw)
                    }

                    char.isDigit() -> {
                        input.advance()
                    }

                    char == 'e' || char == 'E' -> {
                        input.advance()
                        return readExponent(input, startMark)
                    }

                    else -> return null
                }
            }
        }

        internal fun readExponent(input: CharInput<CalcSpan>, startMark: CharInput.Mark): Spanned<CalcSpan, String>? {
            // Supports 1.0e10, 1.0e-10, 1.0e+10
            val char = input.current

            if (char == '+' || char == '-') {
                input.advance()
            }

            if (!input.current.isDigit())
                return null

            while (true) {
                when {
                    input.current.isDigit() -> {
                        input.advance()
                    }

                    else -> {
                        // INCLUDES input.current == CharInput.EOB
                        val span = input.capture(startMark)
                        return Spanned(span, span.raw)
                    }
                }
            }
        }

        fun readExpression(input: CharInput<CalcSpan>): Spanned<CalcSpan, Expression> {
            val start = input.mark()

            val stack = mutableListOf<Spanned<CalcSpan, Expression>>()
            val ops = mutableListOf<Spanned<CalcSpan, Op>>()

            while (true) {
                skipWS(input)
                if (input.current.isDigit()) {
                    val number = readIntegerOrReal(input)
                    require(number != null) { "Invalid number" }
                    val expr = Spanned(number.span, Expression.Number(number))
                    stack.add(expr)
                } else if (input.current.isLetter() || input.current == '_') {
                    val name = readIdentifier(input)
                    if (input.current == '(') {
                        input.advance()
                        val args = readExprArgs(input)
                        val expr = Spanned(input.capture(start), Expression.Call(name, args))
                        stack.add(expr)
                    } else {
                        val expr = Spanned(name.span, Expression.Call(name, null))
                        stack.add(expr)
                    }
                } else {
                    require(input.current == '(') { "Expected number or variable" }
                    input.advance()
                    val inner = readExpression(input)
                    skipWS(input)
                    require(input.current == ')') { "Expected ')'" }
                    input.advance()
                    stack.add(inner)
                }

                skipWS(input)
                if (input.current == CharInput.EOB || input.current == ')' || input.current == ',') {
                    break
                }

                val op = readOp(input)
                while (ops.isNotEmpty() && ops.last().value.ordinal >= op.value.ordinal) {
                    val right = stack.removeLast()
                    val left = stack.removeLast()
                    val op = ops.removeLast()
                    val expr = Spanned(input.capture(start), Expression.Binary(left, op, right))
                    stack.add(expr)
                }
                ops.add(op)
            }

            while (ops.isNotEmpty()) {
                val right = stack.removeLast()
                val left = stack.removeLast()
                val op = ops.removeLast()
                val expr = Spanned(input.capture(start), Expression.Binary(left, op, right))
                stack.add(expr)
            }

            require(stack.size == 1) { "Invalid expression" }

            return stack[0]
        }

        fun readExprArgs(input: CharInput<CalcSpan>): Spanned<CalcSpan, List<Expression>> {
            val start = input.mark()
            val args = mutableListOf<Expression>()
            while (true) {
                skipWS(input)
                if (input.current == ')') {
                    input.advance()
                    break
                }
                val arg = readExpression(input)
                args.add(arg.value)
                skipWS(input)
                if (input.current == ',') {
                    input.advance()
                } else {
                    require(input.current == ')') { "Expected ',' or ')'" }
                }
            }
            return Spanned(input.capture(start), args)
        }
    }
}
