import ply.yacc as yacc
import ply.lex as lex

# Contenido inspirado en la lección de PLY de https://www.dabeaz.com/ply/ply.html

# ----------------------------------ANALISIS LÉXICO---------------------------------------

# Diccionario de palabras reservadas de nuestro lenguaje
reservedWordDict = {
    'bool': 'BOOL',
    'true': 'TRUE',
    'false': 'FALSE',
    'and': 'AND',
    'or': 'OR',
    'if': 'IF',
    'elif': 'ELIF',
    'else': 'ELSE',
    'for': 'FOR',
    'while': 'WHILE',
    'do': 'DO',
    'int': 'INT',
    'float': 'FLOAT',
    'string': 'STRING',
    'print': 'PRINT'
}

# Lo hacemos tupla para que no haya errores y se mofidiquen valores
tokens = tuple(['ID', 'FLOAT_VAL', 'INT_VAL', 'STR_VAL', 'PLUS', 'MINUS', 'MULT', 'DIV', 'EXP', 'ASSIGN', 'NOT_EQUALS',
                'EQ_MORE', 'EQ_LESS', 'MORE', 'LESS', 'EQUALS', 'LPAREN', 'RPAREN', 'LKEY', 'RKEY', 'FINISH'] + list(reservedWordDict.values()))

# Regex para que Lex identifique nuestros tokens
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULT = r'\*'
t_DIV = r'/'
t_EXP = r'\^'
t_ASSIGN = r'='
t_EQUALS = r'=='
t_NOT_EQUALS = r'!='
t_EQ_MORE = r'>='
t_EQ_LESS = r'<='
t_MORE = r'>'
t_LESS = r'<'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LKEY = r'{'
t_RKEY = r'}'
t_FINISH = r';'

# Regex más complicados con funciones


def t_FLOAT_VAL(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t


def t_INT_VAL(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_STR_VAL(t):
    r'\"[^\n]+\"'
    t.value = t.value.replace("\"", "")
    return t


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reservedWordDict.get(t.value, 'ID')
    return t


# Se ignora el tab ya que cuenta como espacio vacio, el cual no debe de importar
t_ignore = ' \t'


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


# Inicializamos el analizador léxico
lexer = lex.lex()

# ----------------------------------ANALISIS SINTÁCTICO---------------------------------------

# Reglas de precedencia. Igual, se manejan tuplas porque es contenido que no se modificará.
precedence = (
    ('left', 'AND', 'OR'),
    ('nonassoc', 'EQUALS', 'NOT_EQUALS', 'EQ_MORE', 'EQ_LESS', 'MORE', 'LESS'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULT', 'DIV'),
    ('left', 'EXP'),
    ('right', 'UMINUS'),  # Uminus es el operador para decir que un número es negativo
)

# Tablas de variables con sus valores, sus tipos y registro de las instrucciones
instructions = ()

# Listado de las reglas de producción de mi lenguaje

# Producción inicial


def p_start(p):
    '''start : statement'''
    global instructions
    instructions = p[1]


def p_statement(p):
    '''statement : print_stmt FINISH statement
                | register_stmt FINISH statement
                | condition_stmt statement
                | for_stmt statement
                | while_stmt statement
                | empty'''
    if len(p) > 2:
        if p[2] == ';':
            p[2] = p[3]
        p[0] = (p[1],) + p[2]
    else:
        p[0] = ()


def p_expression_operation(p):
    '''expression : expression AND expression
                | expression OR expression
                | expression PLUS expression
                | expression MINUS expression
                | expression MULT expression
                | expression DIV expression
                | expression EXP expression
                | expression EQUALS expression
                | expression NOT_EQUALS expression
                | expression EQ_MORE expression
                | expression EQ_LESS expression
                | expression MORE expression
                | expression LESS expression'''
    p[0] = ('operation', p[1], p[2], p[3])


def p_expression_id(p):
    "expression : ID"
    p[0] = p[1]


def p_expression_val(p):
    '''expression : FLOAT_VAL
                 | INT_VAL
                 | STR_VAL
                 | bool_val'''
    p[0] = p[1]


def p_bool_val(p):
    '''bool_val : TRUE
              | FALSE'''
    if p[1] == "true":
        p[0] = True
    elif p[1] == "false":
        p[0] = False


def p_expression_parenthesis(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]


def p_expression_uminus(p):
    'expression : MINUS expression %prec UMINUS'
    p[0] = -p[2]


def p_print_stmt(p):
    'print_stmt : PRINT expression'
    p[0] = ('print', p[2])


def p_register_stmt(p):
    '''register_stmt : declare_reg
            | declare_assign_reg
            | assign_reg'''
    p[0] = p[1]


def p_condition_stmt(p):
    '''condition_stmt : if_cond elif_cond else_cond'''
    p[0] = ('condition', p[1], p[2], p[3])


def p_for_stmt(p):
    '''for_stmt : FOR LPAREN declare_assign_reg FINISH expression FINISH assign_reg RPAREN LKEY statement RKEY'''
    p[0] = ('for', p[3], p[5], p[7], p[10])


def p_while_stmt(p):
    '''while_stmt : WHILE LPAREN expression RPAREN LKEY statement RKEY
                 | DO LKEY statement RKEY WHILE LPAREN expression RPAREN FINISH'''
    if p[1] == "while":
        p[0] = ('while', p[3], p[6])
    else:
        p[0] = ('do-while', p[7], p[3])


def p_empty(p):
    'empty :'
    pass


def p_type(p):
    '''type : BOOL
           | INT
           | FLOAT
           | STRING'''
    p[0] = p[1]


def p_declare_reg(p):
    '''declare_reg : type ID'''
    p[0] = ('declare', p[1], p[2])


def p_declare_assign_reg(p):
    '''declare_assign_reg : type ID ASSIGN expression'''
    p[0] = ('declare_assign', p[1], p[2], p[4])


def p_assign_reg(p):
    '''assign_reg : ID ASSIGN expression'''
    p[0] = ('assign', p[1], p[3])


def p_if_cond(p):
    '''if_cond : IF LPAREN expression RPAREN LKEY statement RKEY'''
    p[0] = ('if', p[3], p[6])


def p_elif_cond(p):
    '''elif_cond : ELIF LPAREN expression RPAREN LKEY statement RKEY elif_cond
                | empty'''
    if len(p) > 2:
        p[0] = (('elif', p[3], p[6]), ) + p[8]
    else:
        p[0] = ()


def p_else_cond(p):
    '''else_cond : ELSE LKEY statement RKEY
                | empty'''
    if len(p) > 2:
        p[0] = ('else', p[3])


def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")


# Iniciamos el analizador sintactico y procesamos el archivo input para generar el arbol
yacc.yacc()
file = open("input.txt", "r")
s = file.read()
yacc.parse(s)


# ----------------------------------GENERACION DE CODIGO INTERMEDIO---------------------------------------

output = open("output.txt", "w")


def parseInstruction(instruction):
  # Guard para verificar que el dato sea tupla (y que la siguiente linea no truene)
    if type(instruction) is not tuple:
        return instruction

    currentStatement = instruction[0]

    # Switch para leer de que tipo de declaracion se trata
    if currentStatement == "print":
        parsedStatement = parseInstruction(instruction[1])
        output.write(f'print {parsedStatement} \n')

    elif currentStatement == "declare":
        dataType = instruction[1]
        id = instruction[2]
        output.write(f'{dataType} {id} \n')

    elif currentStatement == "declare_assign":
        dataType = instruction[1]
        id = instruction[2]
        output.write(f'{dataType} {id} \n')
        parsedStatement = parseInstruction(instruction[3])
        output.write(f'{id} = {parsedStatement} \n')

    elif currentStatement == "assign":
        id = instruction[1]
        parsedStatement = parseInstruction(instruction[2])
        output.write(f'{id} = {parsedStatement} \n')

    elif currentStatement == "operation":
        leftStatement = parseInstruction(instruction[1])
        operation = instruction[2]
        rightStatement = parseInstruction(instruction[3])
        addressCode = outputVariable()

        output.write(
            f'{addressCode} = {leftStatement} {operation} {rightStatement} \n')

        return addressCode

    elif currentStatement == "condition":
        ifStatement = instruction[1]
        elifStatements = instruction[2]
        elseStatement = instruction[3]

        condition = parseInstruction(ifStatement[1])
        currentCheckpoint = outputLabel()
        endingCheckpoint = outputLabel()
        statements = ifStatement[2]
        output.write(f'if {condition} fails, go to {currentCheckpoint}\n')

        for statement in statements:
            parseInstruction(statement)
        output.write(f'go to {endingCheckpoint}\n')
        output.write(f'label {currentCheckpoint}\n')

        for elifStatement in elifStatements:
            condition = parseInstruction(elifStatement[1])
            statements = elifStatement[2]
            currentCheckpoint = outputLabel()

            output.write(f'if {condition} fails, go to {currentCheckpoint}\n')

            for statement in statements:
                parseInstruction(statement)

            output.write(f'go to {endingCheckpoint}\n')
            output.write(f'label {currentCheckpoint}\n')

        if elseStatement is not None:
            statements = elseStatement[1]

            for statement in statements:
                parseInstruction(statement)

        output.write(f'label {endingCheckpoint}\n')

    elif currentStatement == "for":
        parseInstruction(instruction[1])
        innerStatements = instruction[4]

        startCheckpoint = outputLabel()
        endingCheckpoint = outputLabel()

        output.write(f'label {startCheckpoint}\n')

        condition = parseInstruction(instruction[2])
        output.write(f'if {condition} fails, go to {endingCheckpoint}\n')

        for statement in innerStatements:
            parseInstruction(statement)

        output.write(f'go to {startCheckpoint}\n')
        output.write(f'label {endingCheckpoint}\n')

    elif currentStatement == "while":
        statements = instruction[2]
        startCheckpoint = outputLabel()
        endingCheckpoint = outputLabel()
        condition = parseInstruction(instruction[1])

        output.write(f'label {startCheckpoint}\n')
        output.write(f'if {condition} fails, go to {endingCheckpoint}\n')

        for statement in statements:
            parseInstruction(statement)

        output.write(f'go to {startCheckpoint}\n')
        output.write(f'label {endingCheckpoint}\n')

    elif currentStatement == "do-while":
        statements = instruction[2]
        startCheckpoint = outputLabel()

        output.write(f'go to {startCheckpoint}\n')

        for statement in statements:
            parseInstruction(statement)

        condition = parseInstruction(instruction[1])
        output.write(f'if {condition} fails, go to {startCheckpoint}\n')

    else:
        output.write(
            f'-----ERROR: Unknown statement: {currentStatement}-----\n')


def outputLabel():
    global labelCounter
    labelCounter += 1
    return "L" + str(labelCounter)


def outputVariable():
    global variableCounter
    variableCounter += 1
    return "V" + str(variableCounter)


variableCounter = -1
labelCounter = -1

# iniciamos a parsear y escribir los valores dentro del archivo output.txt
for instruction in instructions:
    parseInstruction(instruction)
output.close()
