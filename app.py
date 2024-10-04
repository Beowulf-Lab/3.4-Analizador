from flask import Flask, request, render_template 
import ply.lex as lex
import ply.yacc as yacc
import re

app = Flask(__name__)

# Definir los tokens para el lexer
tokens = [
    'RESERVED', 'IDENTIFIER', 'VARIABLE', 'LBRACE', 'RBRACE',
    'LPAREN', 'RPAREN', 'SEMICOLON', 'EQUAL', 'PLUS',
    'NUMBER', 'STRING', 'COMMA'
]

# Lista de palabras reservadas
reserved = {
    'programa': 'RESERVED',
    'int': 'RESERVED',
    'read': 'RESERVED',
    'printf': 'RESERVED',
    'end': 'RESERVED'
}
# Definir las variables a, b, c explícitamente
def t_VARIABLE(t):
    r'\b[a-c]\b'  # Reconocer solo las variables 'a', 'b' y 'c'
    return t

# Expresión regular para strings
def t_STRING(t):
    r'"[^"]*"'  # Las cadenas deben estar entre comillas
    return t

# Otros tokens
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_SEMICOLON = r';'
t_EQUAL = r'='
t_PLUS = r'\+'
t_COMMA = r','

# Expresión regular para números
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Expresión regular para identificadores
def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'IDENTIFIER')  # Reservar palabras clave como 'programa', 'int', etc.
    return t

# Ignorar espacios y tabs
t_ignore = ' \t'

# Manejar saltos de línea
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Lista para almacenar errores léxicos
lexer_errors = []

# Definir el manejador de errores léxicos
def t_error(t):
    error_message = f"Caracter ilegal '{t.value[0]}' en la línea {t.lineno}"
    lexer_errors.append(error_message)
    t.lexer.skip(1)

# Crear el lexer
lexer = lex.lex()

# Reglas del análisis sintáctico
def p_program(p):
    '''program : RESERVED IDENTIFIER LPAREN RPAREN LBRACE block RBRACE'''
    p[0] = ('program', p[2], p[6])

def p_block(p):
    '''block : statement block
             | statement'''
    if len(p) == 3:
        p[0] = [p[1]] + p[2]
    else:
        p[0] = [p[1]]

def p_declaration(p):
    '''declaration : RESERVED identifier_list SEMICOLON'''
    p[0] = ('declaration', p[1], p[2])

def p_read_statement(p):
    '''read_statement : RESERVED VARIABLE SEMICOLON'''
    p[0] = ('read', p[2])

def p_printf_statement(p):
    '''printf_statement : RESERVED LPAREN STRING RPAREN SEMICOLON'''  # Asegúrate de que requiera paréntesis
    p[0] = ('printf', p[3])  # Aquí se captura la cadena

def p_end_statement(p):
    '''end_statement : RESERVED SEMICOLON'''
    p[0] = ('end',)

def p_assignment_statement(p):
    '''assignment_statement : VARIABLE EQUAL expression SEMICOLON'''
    p[0] = ('assignment', p[1], p[3])

def p_expression(p):
    '''expression : VARIABLE PLUS VARIABLE
                  | NUMBER PLUS NUMBER
                  | VARIABLE PLUS NUMBER
                  | NUMBER PLUS VARIABLE'''
    p[0] = ('expression', p[1], '+', p[3])

def p_identifier_list(p):
    '''identifier_list : VARIABLE
                       | VARIABLE COMMA identifier_list'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

def p_statement(p):
    '''statement : declaration
                 | read_statement
                 | printf_statement
                 | end_statement
                 | assignment_statement'''
    p[0] = p[1]

# Manejo de errores sintácticos
def p_error(p):
    if p:
        error_message = f"Sintáctico en '{p.value}' en la línea {p.lineno}"
        raise SyntaxError(error_message)
    else:
        raise SyntaxError("Sintáctico al final de la entrada")

# Crear el parser
parser = yacc.yacc()

# Función para analizar el texto léxicamente
def lexico(text):
    global lexer_errors
    lexer_errors = []  # Resetear errores antes de cada análisis
    lines = text.splitlines()
    tokens_list = []
    line_info = []

    for i, line in enumerate(lines, start=1):
        lexer.input(line)
        while True:
            tok = lexer.token()
            if not tok:
                break  # No más tokens en la línea actual
            tokens_list.append((i, tok.type, tok.value))

            # Distinguir tipos de símbolos, palabras reservadas y otros tokens
            tipo_palabra = 'Símbolo Desconocido'
            if tok.type == 'RESERVED':
                tipo_palabra = 'Palabra Reservada'
            elif tok.type == 'IDENTIFIER':
                tipo_palabra = 'Identificador'
            elif tok.type == 'VARIABLE':
                tipo_palabra = 'Variable'
            elif tok.type == 'NUMBER':
                tipo_palabra = 'Número'
            elif tok.type == 'STRING':
                tipo_palabra = 'Cadena'
            elif tok.type == 'LBRACE':
                tipo_palabra = 'Llave Izquierda'
            elif tok.type == 'RBRACE':
                tipo_palabra = 'Llave Derecha'
            elif tok.type == 'LPAREN':
                tipo_palabra = 'Paréntesis Izquierdo'
            elif tok.type == 'RPAREN':
                tipo_palabra = 'Paréntesis Derecho'
            elif tok.type == 'SEMICOLON':
                tipo_palabra = 'Punto y Coma'
            elif tok.type == 'EQUAL':
                tipo_palabra = 'Igual'
            elif tok.type == 'PLUS':
                tipo_palabra = 'Más'
            elif tok.type == 'COMMA':
                tipo_palabra = 'Coma'

            line_info.append((i, tipo_palabra, tok.value))

    return tokens_list, line_info, lexer_errors

# Función para el análisis sintáctico
def sintactico(tokens_list):
    sintactico_info = []
    token_values = ' '.join([str(t[2]) for t in tokens_list])

    try:
        result = parser.parse(token_values)
        if result:
            sintactico_info.append((1, 'Correcto', result))
    except SyntaxError as e:
        # Extraer número de línea del mensaje de error si es posible
        error_message = str(e)
        match = re.search(r"en la línea (\d+)", error_message)
        if match:
            line_num = match.group(1)
        else:
            line_num = 'Unknown'
        sintactico_info.append((line_num, 'Error', error_message))

    return sintactico_info

@app.route('/', methods=['GET', 'POST'])
def index():
    error_message = None
    success_message = None  # Añadir variable para el mensaje de éxito
    tokens = []
    line_info = []
    sintactico_info = []
    text = ''

    if request.method == 'POST':
        text = request.form['text']
        tokens, line_info, lexer_errors = lexico(text)

        # Si hay errores léxicos, mostrar el primero y no seguir con el análisis sintáctico
        if lexer_errors:
            error_message = lexer_errors[0]
            sintactico_info = []
        else:
            sintactico_info = sintactico(tokens)
            # Verificar si hay errores en la información sintáctica
            if sintactico_info and sintactico_info[0][1] == 'Error':
                error_message = sintactico_info[0][2]  # Obtener el mensaje de error
            else:
                success_message = "Sintáctico correcto"

    return render_template('index.html', text=text, tokens=tokens, line_info=line_info,
                           sintactico_info=sintactico_info, error_message=error_message,
                           success_message=success_message)

if __name__ == '__main__':
    app.run(debug=True)