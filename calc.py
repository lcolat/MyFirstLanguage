# -----------------------------------------------------------------------------
# calc.py
#
# A simple calculator with variables.
# -----------------------------------------------------------------------------

tokens = (
    'NAME','NUMBER',
    'PLUS','MINUS','TIMES','DIVIDE','EQUALS',
    'LPAREN','RPAREN','SEMICOLON'
    )

# Tokens


t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_EQUALS  = r'='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_NAME    = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_SEMICOLON = r';'

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
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
import ply.lex as lex
lex.lex()

# Precedence rules for the arithmetic operators
precedence = (
    ('left','PLUS','MINUS'),
    ('left','TIMES','DIVIDE'),
    ('right','UMINUS')
    )

# dictionary of names (for storing variables)
names = { }

def eval(t):
    if type(t) == tuple:
        op, a, b = t[0], t[1], t[2]
        if op == '+': return eval(a) + eval(b)
        elif op == '-': return eval(a) - eval(b)
        elif op == '*': return eval(a) * eval(b)
        elif op == '/': return eval(a) / eval(b)
        elif op == '=':
            names[a] = eval(b)
            return names[a]
    else:
        if t in names: return int(names[t])
        return t

def p_statement_expr(p):
    '''statement : statement expression SEMICOLON
                 | expression SEMICOLON'''
    if len(p) == 3:
        print(p[1])
        print(eval(p[1]))
    elif len(p) == 4:
        print(p[2])
        print(eval(p[2]))

def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression'''
    if p[2] == '+'  : p[0] = ('+',p[1],p[3])
    elif p[2] == '-': p[0] = ('-',p[1],p[3])
    elif p[2] == '*': p[0] = ('*',p[1],p[3])
    elif p[2] == '/': p[0] = ('/',p[1],p[3])

def p_statement_assign(p):
    'statement : NAME EQUALS expression SEMICOLON'
    p[0] = eval(('=',p[1],p[3]))

def p_expression_uminus(p):
    'expression : MINUS expression %prec UMINUS'
    p[0] = -p[2]

def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]

def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]

def p_expression_name(p):
    'expression : NAME'
    try:
        p[0] = p[1]
    except LookupError:
        print("Undefined name '%s'" % p[1])
        p[0] = 0

def p_error(p):
    print("Syntax error at '%s'" % p.value)

import ply.yacc as yacc
yacc.yacc()

while True:
    try:
        s = input('calc > ')   # use input() on Python 3
    except EOFError:
        break
    yacc.parse(s)