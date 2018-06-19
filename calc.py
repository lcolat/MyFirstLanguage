import ply.yacc as yacc
import ply.lex as lex

reserved = {
    'echo': 'ECHO',
    'while': 'WHILE',
    'for': 'FOR',
    'then': 'THEN',
    'if': 'IF',
    'elif': 'ELIF',
    'else': 'ELSE',
    'function': 'FUNCTION',
    'import': 'IMPORT',
}

operators = {
    '+': 'PLUS',
    '-': 'MINUS',
    '*': 'TIMES',
    '/': 'DIVIDE',
    '%': 'MOD',
    '|': 'OR',
    '&': 'AND',
    '^': 'XOR',
    '<<': 'LSHIFT',
    '>>': 'RSHIFT',
    '||': 'LOR',
    '&&': 'LAND',
    '<': 'LT',
    '<=': 'LE',
    '>': 'GT',
    '>=': 'GE',
    '==': 'EQ',
    '!=': 'NE',
}

assignment = {
    '=': 'EQUALS',
    '*=': 'TIMESEQUAL',
    '/=': 'DIVEQUAL',
    '%=': 'MODEQUAL',
    '+=': 'PLUSEQUAL',
    '-=': 'MINUSEQUAL',
    '<<=': 'LSHIFTEQUAL',
    '>>=': 'RSHIFTEQUAL',
    '&=': 'ANDEQUAL',
    '^=': 'XOREQUAL',
    '|=': 'OREQUAL',
}

delimiters = {
    '(': 'LPAREN',
    ')': 'RPAREN',
    ';': 'SEMICOLON',
    '#': 'SHARP',
    ',': 'COMMA',
    '[': 'LBRACKET',
    ']': 'RBRACKET',
}

others = (
    'NAME', 'NUMBER', 'STRING',
)

tokens = others +\
         tuple(operators.values()) +\
         tuple(assignment.values()) +\
         tuple(delimiters.values()) +\
         tuple(reserved.values())

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
t_SHARP = r'\#'
t_COMMA = r','
t_LBRACKET = r'\['
t_RBRACKET = r'\]'


def t_NAME(t):
    r'[A-Za-z_][\w_]*'
    t.type = reserved.get(t.value, "NAME")
    return t


def t_STRING(t):
    r"'([^\\']+|\\'|\\\\)*'"
    t.value = t.value[1:-1]
    return t


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
preprocessor = {}
functions = {}


def eval_op(t):
    op, a, b = t[0], eval(t[1]), eval(t[2])

    if op == '+':
        if isinstance(a, str):
            return a + str(b)
        elif isinstance(b, str):
            return str(a) + b

        return a + b
    elif op == '-':
        return a - b
    elif op == '*':
        return a * b
    elif op == '/':
        return a / b
    elif op == '%':
        return a % b
    elif op == '|':
        return a | b
    elif op == '&':
        return a & b
    elif op == '^':
        return a ^ b
    elif op == '<<':
        return a << b
    elif op == '>>':
        return a >> b
    elif op == '&&':
        return a and b
    elif op == '||':
        return a or b
    elif op == '<':
        return a < b
    elif op == '<=':
        return a <= b
    elif op == '>':
        return a > b
    elif op == '>=':
        return a >= b
    elif op == '==':
        return a == b
    elif op == '!=':
        return a != b


def eval_assignment(t):
    op, a, b = t[0], t[1], eval(t[2])

    if op == '<<=':
        names[a] <<= b
    elif op == '>>=':
        names[a] >>= b
    elif op == '&=':
        names[a] &= b
    elif op == '^=':
        names[a] ^= b
    elif op == '|=':
        names[a] |= b
    elif op == '*=':
        names[a] *= b
    elif op == '/=':
        names[a] /= b
    elif op == '%=':
        names[a] %= b
    elif op == '+=':
        names[a] += b
    elif op == '-=':
        names[a] -= b
    elif op == '=':
        names[a] = b


def eval_expression(t):
    ops = operators.keys()
    assigns = assignment.keys()

    for el in t:
        if isinstance(el, str):
            if el in ops:
                return eval_op(t)
            if el in assigns:
                return eval_assignment(t)

    op = t[0]

    if op == 'block':
        eval(t[1])
        eval(t[2])
        return

    elif op == 'while':
        while eval(t[1]):
            eval(t[2])

    elif op == 'for':  # FOR statement TO expression THEN block
        start = eval(t[1])

        for i in range(start, eval(t[2])):
            eval(t[3])

    elif op == 'if':
        if eval(t[1]):
            eval(t[2])
        elif eval(t[3]):
            pass
        else:
            eval(t[4])

    elif op == 'elif':
        if eval(t[1]):
            eval(t[2])
            return True
        elif len(t[3]) != 0:
            if eval(t[3]):
                return True

        return False

    elif op == 'else':
        eval(t[1])

    elif op == 'echo':
        if t[1] == '__names':
            print('__names', str(names))
        elif t[1] == '__preprocessor':
            print('__preprocessor', str(preprocessor))
        else:
            print(str(eval(t[1])))

    elif op == 'preprocessor':
        preprocessor[t[1]] = t[2]

    elif op == 'declare_function':
        functions[t[1]] = (t[2], t[3])

    elif op == 'exec_function':
        if t[1] in functions:
            func = functions[t[1]]
            func_params, func_block = func[0], func[1]

            names_to_reset = {}

            i = 0
            for param in func_params:
                if param in names:
                    names_to_reset[param] = names[param]

                try:
                    names[param] = eval(t[2][i])
                except:
                    names[param] = None
                finally:
                    i += 1

            res = eval(func_block)

            # Reset the context
            for param in names_to_reset:
                names[param] = names_to_reset[param]

            for param in func_params:
                del names[param]

            return res
        else:
            return None

    elif op == 'import':
        with open(t[1] + '.pypy') as imported_file:
            for imported_line in imported_file:
                yacc.parse(imported_line)


def eval(t):
    if type(t) == tuple:
        return eval_expression(t)
    elif isinstance(t, str):
        if t in names:
            return names[t]
        if t in preprocessor:
            return preprocessor[t]
        elif t == 'true':
            return True
        elif t == 'false':
            return False

    return t


def p_program(p):
    """program : program block
             | block"""

    if len(p) == 2:
        eval(p[1])


def p_block(p):
    """block : block statement

             | statement"""

    if len(p) == 3:
        p[0] = ('block', p[1], p[2])
    else:
        p[0] = p[1]


def p_empty(p):
    'empty :'
    pass


def p_preprocessor(p):
    """statement : SHARP NAME expression"""
    p[0] = ('preprocessor', p[2], p[3])


def p_statement_echo(p):
    """statement : ECHO expression SEMICOLON"""
    p[0] = ('echo', p[2])


def p_statement_while(p):
    """statement : WHILE expression THEN block"""
    p[0] = ('while', p[2], p[4])


def p_statement_for(p):
    """statement : FOR statement SEMICOLON expression THEN block"""
    p[0] = ('for', p[2], p[4], p[6])


def p_statement_if(p):
    """statement : IF expression THEN block elif_statement else_statement"""
    p[0] = ('if', p[2], p[4], p[5], p[6])


def p_statement_elif(p):
    """elif_statement : ELIF expression THEN block elif_statement
                      | empty"""
    if p[1] is not None:
        p[0] = ('elif', p[2], p[4], p[5])
    else:
        p[0] = []


def p_statement_else(p):
    """else_statement : ELSE block
                      | empty"""
    if p[1] is not None:
        p[0] = ('else', p[2])
    else:
        p[0] = []


def p_statement_import(p):
    """statement : IMPORT NAME SEMICOLON"""

    p[0] = ('import', p[2])


def p_parameters_declaration(p):
    """parameters_declaration : empty
                              | NAME COMMA parameters_declaration
                              | NAME"""

    if len(p) == 4:
        p[0] = [p[1]] + p[3]
    elif p[1] is not None:
        p[0] = [p[1]]
    else:
        p[0] = []


def p_parameters_execution(p):
    """parameters_execution : empty
                              | expression COMMA parameters_execution
                              | expression"""

    if len(p) == 4:
        p[0] = [p[1]] + p[3]
    elif p[1] is not None:
        p[0] = [p[1]]
    else:
        p[0] = []


def p_statement_function_declaration(p):
    """statement : FUNCTION NAME LBRACKET parameters_declaration RBRACKET block"""
    p[0] = ('declare_function', p[2], p[4], p[6])


def p_statement_function_execution(p):
    """statement : NAME LBRACKET parameters_execution RBRACKET SEMICOLON"""
    p[0] = ('exec_function', p[1], p[3])


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
                  | expression NE expression"""
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


def p_expression_uminus(p):
    """expression : MINUS expression %prec UMINUS"""
    p[0] = -p[2]


def p_expression_group(p):
    """expression : LPAREN expression RPAREN"""
    p[0] = p[2]


def p_expression_number(p):
    """expression : NUMBER"""
    p[0] = p[1]


def p_expression_string(p):
    """expression : STRING"""
    p[0] = p[1]


def p_expression_name(p):
    """expression : NAME"""
    p[0] = p[1]


def p_error(p):
    if p is not None:
        print("Erreur de syntaxe à la ligne %s" % p.lineno, p)


yacc.yacc()

with open('code.pypy') as file:
    for line in file:
        yacc.parse(line)
