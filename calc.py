import ply.yacc as yacc
import ply.lex as lex

reserved = {
    'echo': 'ECHO',
}

tokens = (
    # Literals
    'NAME', 'NUMBER',

    # Operators
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MOD',  # +, -, *, /, %
    'OR', 'AND', 'XOR', 'LSHIFT', 'RSHIFT',     # |, &, ^, <<, >>
    'LOR', 'LAND',                              # ||, &&
    'LT', 'LE', 'GT', 'GE', 'EQ', 'NE',         # <, <=, >, >=, ==, !=

    # Assignment
    'EQUALS', 'TIMESEQUAL', 'DIVEQUAL', 'MODEQUAL', 'PLUSEQUAL', 'MINUSEQUAL',  # =, *=, /=, %=, +=, -=
    'LSHIFTEQUAL', 'RSHIFTEQUAL', 'ANDEQUAL', 'XOREQUAL', 'OREQUAL',  # <<=, >>=, &=, ^=, |=

    # Delimiters
    'LPAREN', 'RPAREN', 'SEMICOLON',  # (, ), ;
) + tuple(reserved.values())

# Ignored characters
t_ignore = " \t"


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


# Operators
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_MOD = r'%'
t_OR = r'\|'
t_AND = r'&'
t_XOR = r'\^'
t_LSHIFT = r'<<'
t_RSHIFT = r'>>'
t_LOR = r'\|\|'
t_LAND = r'&&'
t_LT = r'<'
t_GT = r'>'
t_LE = r'<='
t_GE = r'>='
t_EQ = r'=='
t_NE = r'!='


# Assignment operators
t_EQUALS = r'='
t_TIMESEQUAL = r'\*='
t_DIVEQUAL = r'/='
t_MODEQUAL = r'%='
t_PLUSEQUAL = r'\+='
t_MINUSEQUAL = r'-='
t_LSHIFTEQUAL = r'<<='
t_RSHIFTEQUAL = r'>>='
t_ANDEQUAL = r'&='
t_OREQUAL = r'\|='
t_XOREQUAL = r'\^='


# Delimeters
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_SEMICOLON = r';'


def t_NAME(t):
    r'[A-Za-z_][\w_]*'
    t.type = reserved.get(t.value, "NAME")
    return t


def t_comment(t):
    r'\#.*'
    t.lexer.lineno += t.value.count('\n')


def t_NUMBER(t):
    r'\d+\.?\d*'
    t.value = float(t.value)
    return t


def t_error(t):
    print("Illegal character %s" % repr(t.value[0]))
    t.lexer.skip(1)

# Build the lexer
lex.lex()

# Precedence rules for the arithmetic operators
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS')
)

# dictionary of names (for storing variables), should only be used in eval
names = {}


def eval_expression(t):
    expression_len = len(t)

    if expression_len == 3:
        op, a, b = t[0], t[1], t[2]

        # Evaluation
        if op == '+':
            return eval(a) + eval(b)
        elif op == '-':
            return eval(a) - eval(b)
        elif op == '*':
            return eval(a) * eval(b)
        elif op == '/':
            return eval(a) / eval(b)
        elif op == '%':
            return eval(a) % eval(b)
        elif op == '|':
            return eval(a) | eval(b)
        elif op == '&':
            return eval(a) & eval(b)
        elif op == '^':
            return eval(a) ^ eval(b)
        elif op == '<<':
            return eval(a) << eval(b)
        elif op == '>>':
            return eval(a) >> eval(b)
        elif op == '&&':
            return eval(a) and eval(b)
        elif op == '||':
            return eval(a) or eval(b)
        elif op == '<':
            return eval(a) < eval(b)
        elif op == '<=':
            return eval(a) <= eval(b)
        elif op == '>':
            return eval(a) > eval(b)
        elif op == '>=':
            return eval(a) >= eval(b)
        elif op == '==':
            return eval(a) == eval(b)
        elif op == '!=':
            return eval(a) != eval(b)

        # Assignment
        elif op == '<<=':
            names[a] <<= eval(b)
        elif op == '>>=':
            names[a] >>= eval(b)
        elif op == '&=':
            names[a] &= eval(b)
        elif op == '^=':
            names[a] ^= eval(b)
        elif op == '|=':
            names[a] |= eval(b)
        elif op == '*=':
            names[a] *= eval(b)
        elif op == '/=':
            names[a] /= eval(b)
        elif op == '%=':
            names[a] %= eval(b)
        elif op == '+=':
            names[a] += eval(b)
        elif op == '-=':
            names[a] -= eval(b)
        elif op == '=':
            names[a] = eval(b)

        # Block
        elif op == 'block':
            eval(a)
            eval(b)

    elif expression_len == 2:
        a, b = t[0], t[1]

        if a == 'echo':
            print(eval(b))


def eval(t):
    if type(t) == tuple:
        return eval_expression(t)
    elif isinstance(t, str):
        if t in names: return names[t]
        elif t == 'TRUE': return True
        elif t == 'FALSE': return False

    return t


def p_block(p):
    """block : block statement
             | statement"""
    if len(p) == 3:
        p[0] = ('block', p[1], p[2])
    else:
        p[0] = p[1]

    eval(p[0])


def p_statement_expr(p):
    """statement : expression SEMICOLON"""
    p[0] = p[1]


def p_expression(p):
    """expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression MOD expression
                  | expression OR expression
                  | expression AND expression
                  | expression XOR expression
                  | expression LSHIFT expression
                  | expression RSHIFT expression
                  | expression LOR expression
                  | expression LAND expression
                  | expression LT expression
                  | expression LE expression
                  | expression GT expression
                  | expression GE expression
                  | expression EQ expression
                  | expression NE expression
                  """
    p[0] = (p[2], p[1], p[3])


def p_statement_assign(p):
    """statement : NAME EQUALS expression SEMICOLON
                 | NAME TIMESEQUAL expression SEMICOLON
                 | NAME DIVEQUAL expression SEMICOLON
                 | NAME MODEQUAL expression SEMICOLON
                 | NAME PLUSEQUAL expression SEMICOLON
                 | NAME MINUSEQUAL expression SEMICOLON
                 | NAME LSHIFTEQUAL expression SEMICOLON
                 | NAME RSHIFTEQUAL expression SEMICOLON
                 | NAME ANDEQUAL expression SEMICOLON
                 | NAME OREQUAL expression SEMICOLON
                 | NAME XOREQUAL expression SEMICOLON"""
    p[0] = (p[2], p[1], p[3])


def p_statement_echo(p):
    """statement : ECHO expression SEMICOLON"""
    p[0] = ('echo', p[2])


def p_expression_uminus(p):
    """expression : MINUS expression %prec UMINUS"""
    p[0] = -p[2]


def p_expression_group(p):
    """expression : LPAREN expression RPAREN"""
    p[0] = p[2]


def p_expression_number(p):
    """expression : NUMBER"""
    p[0] = p[1]


def p_expression_name(p):
    """expression : NAME"""
    p[0] = p[1]


def p_error(p):
    if hasattr(p, 'value'):
        print('Syntax error at %s' % p.value, str(p))
    else:
        print('Error :', p)

yacc.yacc()

with open('code.pypy') as file:
    for line in file:
        yacc.parse(line)