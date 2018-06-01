import ply.yacc as yacc
import ply.lex as lex

reserved = {
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'while': 'WHILE',
    'for': 'FOR',
    'echo': 'ECHO',
    'true': 'TRUE',
    'false': 'FALSE'
}

tokens = [
    'NAME', 'NUMBER',
    'LPAREN', 'RPAREN', 'SEMICOLON',
    'AND', 'OR', 'NOT',
    'EQUALITY', 'INEQUALITY', 'LESS', 'MORE', 'LESS_OR_EQUAL', 'MORE_OR_EQUAL',
    'MINUS', 'TIMES', 'EQUALS', 'DIVIDE', 'PLUS',
    'ID'
    ] + list(reserved.values())

# Tokens

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EQUALS = r'='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_NAME = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_SEMICOLON = r';'
t_EQUALITY = r'=='
t_INEQUALITY = r'\!='
t_LESS = r'<'
t_MORE = r'>'
t_LESS_OR_EQUAL = r'<='
t_MORE_OR_EQUAL = r'>='
t_TRUE = r'true'
t_FALSE = r'false'
t_AND = r'\&\&'
t_OR = r'\|\|'
t_NOT = r'\!'


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]'
    if t.value in reserved:
        t.type = reserved[t.value]
    return t


def t_NUMBER(t):
    r'\d+\.?\d*'
    t.value = float(t.value)
    return t

# Ignored characters
t_ignore = " \t"


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
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


def eval_keyword(keyword, data):
    if keyword == 'echo':
        print(str(eval(data)))


def eval_expression(op, a, b):
    print(op, a, b)
    if op == '+':
        return eval(a) + eval(b)
    elif op == '-':
        return eval(a) - eval(b)
    elif op == '*':
        return eval(a) * eval(b)
    elif op == '/':
        return eval(a) / eval(b)
    elif op == '=':
        names[a] = eval(b)
        return names[a]
    elif op == '==': return eval(a) == eval(b)
    elif op == '!=': return eval(a) != eval(b)
    elif op == '>': return eval(a) > eval(b)
    elif op == '<': return eval(a) < eval(b)
    elif op == '>=': return eval(a) >= eval(b)
    elif op == '<=': return eval(a) <= eval(b)
    elif op == '&&': return eval(a) and eval(b)
    elif op == '||': return eval(a) or eval(b)
    elif op == 'block':
        eval(a)
        eval(b)
        return


def eval(t):
    if type(t) == tuple:
        if len(t) == 3:
            return eval_expression(t[0], t[1], t[2])
        elif len(t) == 2:
            return eval_keyword(t[0], t[1])
    elif isinstance(t, str):
        if t in names: return names[t]
        elif t == 'true': return True
        elif t == 'false': return False

    return t


def p_block(p):
    """block : block statement
             | statement"""
    if len(p) == 3:
        p[0] = ('block', p[1], p[2])
    else:
        p[0] = eval(p[1])

    print('DEBUG', p[0], '=', p[0])


def p_statement_expr(p):
    """statement : expression SEMICOLON"""
    p[0] = p[1]


def p_expression(p):
    """expression : expression EQUALITY expression
                  | expression INEQUALITY expression
                  | expression LESS expression
                  | expression MORE expression
                  | expression LESS_OR_EQUAL expression
                  | expression MORE_OR_EQUAL expression
                  | expression AND expression
                  | expression OR expression
                  | expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression"""
    p[0] = (p[2], p[1], p[3])


def p_statement_assign(p):
    """statement : NAME EQUALS expression SEMICOLON"""
    p[0] = ('=', p[1], p[3])


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
    print("Syntax error at '%s'" % p.value)

yacc.yacc()

while True:
    try:
        s = input('calc > ')   # use input() on Python 3
    except EOFError:
        break
    yacc.parse(s)