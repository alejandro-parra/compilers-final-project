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
