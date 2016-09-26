tokens = (
    'COLON',
    'IDENT',
    'NUM_OCT',
    'NUM_BIN',
    'NUM_HEX',
    'COMEFROM',
    'GOTO',
    'COND_START',
    'COND_FLAG',
    'COND_INV'
)

# Tokens
t_COLON = r':'
t_IDENT = r'[\w]+'
t_COMEFROM = r'<'
t_GOTO = r'>'

t_COND_START = r"@"
t_COND_FLAG = r"A-Z"
t_COND_INV = r"!"

def t_NUM_OCT(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t
def t_NUM_BIN(t):
    r'0b[01]+'
    t.value = int(t.value[2:], 2)
    return t
def t_NUM_HEX(t):
    r'0x[0-9a-f]+'
    t.value = int(t.value[2:], 16)
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

t_ignore = " "
t_ignore_COMMENT = r'\#[^\n]*'

import ply.lex as lex
lexer = lex.lex()

def p_entry(t):
    'entry : COLON ident entry_args COLON entry_contents'
    pass

def p_entry_args_empty(t):
    'entry_args : empty'
    return []
def p_entry_args_single(t):
    'entry_args : entry_arg'
    return [t[1]]
def p_entry_args_mult(t):
    'entry_args : entry_args entry_arg'
    return t[1].append(t[2])

def p_entry_arg(t):
    '''
    entry_arg : number
              | ident
    '''
    pass

def p_entry_contents(t):
    '''
    entry_contents : entry_content_item
                   | entry_contents entry_content_item
    '''
    pass

def p_entry_content_item_from(t):
    'entry_content_item : COMEFROM number'
    return ('from', t[2])
def p_entry_content_item_to(t):
    'entry_content_item : GOTO number'
    return ('to', t[2])
def p_entry_content_item_flag(t):
    'entry_content_item : ident'
    return ('flag', t[1])
def p_entry_content_item_flag_cond(t):
    'entry_content_item : flag_cond ident'

def p_flag_cond(t):
    'flag_cond : COND_START flag_cond_items'
    pass
def p_flag_cond_items_mult(t):
    'flag_cond_items : flag_cond_items flag_cond_item'
    return t[1].append(t[2])
def p_flag_cond_items_single(t):
    'flag_cond_items : flag_cond_item'
    return [t[1]]
def p_flag_cond_item(t):
    '''
    flag_cond_item : COND_INV COND_FLAG
                   | COND_FLAG
    '''
    if t[1] == "!":
        return (t[2], False)
    else:
        return (t[1], True)

def p_number(t):
    '''
    number : NUM_OCT
           | NUM_BIN
           | NUM_HEX
    '''
    return t[1]

def p_ident(t):
    'ident : IDENT'
    return t[1]

def p_empty(t):
    'empty :'
    pass

import ply.yacc as yacc
parser = yacc.yacc()

with open("tmpexp") as f:
    data = f.read()
    parser.parse(data)
