
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'leftPLUSMINUSleftTIMESDIVIDErightUMINUSAND DIVIDE ELSE EQUALITY EQUALS FALSE FOR IF INEQUALITY LESS LESS_OR_EQUAL LPAREN MINUS MORE MORE_OR_EQUAL NAME NOT NUMBER OR PLUS RPAREN SEMICOLON THEN TIMES TRUE WHILEblock : block statement\n             | statementstatement : expression SEMICOLONexpression : expression EQUALITY expression\n                  | expression INEQUALITY expression\n                  | expression LESS expression\n                  | expression MORE expression\n                  | expression LESS_OR_EQUAL expression\n                  | expression MORE_OR_EQUAL expression\n                  | expression AND expression\n                  | expression OR expression\n                  | expression PLUS expression\n                  | expression MINUS expression\n                  | expression TIMES expression\n                  | expression DIVIDE expressionstatement : NAME EQUALS expression SEMICOLONexpression : MINUS expression %prec UMINUSexpression : LPAREN expression RPARENexpression : NUMBERexpression : NAME'
    
_lr_action_items = {'NAME':([0,1,2,5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,40,],[4,4,-2,24,24,-1,-3,24,24,24,24,24,24,24,24,24,24,24,24,24,-16,]),'MINUS':([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,],[5,5,-2,19,-20,5,5,-19,-1,-3,5,5,5,5,5,5,5,5,5,5,5,5,5,-17,-20,19,19,19,19,19,19,19,19,19,-12,-13,-14,-15,19,-18,-16,]),'LPAREN':([0,1,2,5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,40,],[6,6,-2,6,6,-1,-3,6,6,6,6,6,6,6,6,6,6,6,6,6,-16,]),'NUMBER':([0,1,2,5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,40,],[7,7,-2,7,7,-1,-3,7,7,7,7,7,7,7,7,7,7,7,7,7,-16,]),'$end':([1,2,8,9,40,],[0,-2,-1,-3,-16,]),'SEMICOLON':([3,4,7,23,24,26,27,28,29,30,31,32,33,34,35,36,37,38,39,],[9,-20,-19,-17,-20,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,40,-18,]),'EQUALITY':([3,4,7,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,],[10,-20,-19,-17,-20,10,10,10,10,10,10,10,10,10,-12,-13,-14,-15,10,-18,]),'INEQUALITY':([3,4,7,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,],[11,-20,-19,-17,-20,11,11,11,11,11,11,11,11,11,-12,-13,-14,-15,11,-18,]),'LESS':([3,4,7,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,],[12,-20,-19,-17,-20,12,12,12,12,12,12,12,12,12,-12,-13,-14,-15,12,-18,]),'MORE':([3,4,7,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,],[13,-20,-19,-17,-20,13,13,13,13,13,13,13,13,13,-12,-13,-14,-15,13,-18,]),'LESS_OR_EQUAL':([3,4,7,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,],[14,-20,-19,-17,-20,14,14,14,14,14,14,14,14,14,-12,-13,-14,-15,14,-18,]),'MORE_OR_EQUAL':([3,4,7,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,],[15,-20,-19,-17,-20,15,15,15,15,15,15,15,15,15,-12,-13,-14,-15,15,-18,]),'AND':([3,4,7,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,],[16,-20,-19,-17,-20,16,16,16,16,16,16,16,16,16,-12,-13,-14,-15,16,-18,]),'OR':([3,4,7,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,],[17,-20,-19,-17,-20,17,17,17,17,17,17,17,17,17,-12,-13,-14,-15,17,-18,]),'PLUS':([3,4,7,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,],[18,-20,-19,-17,-20,18,18,18,18,18,18,18,18,18,-12,-13,-14,-15,18,-18,]),'TIMES':([3,4,7,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,],[20,-20,-19,-17,-20,20,20,20,20,20,20,20,20,20,20,20,-14,-15,20,-18,]),'DIVIDE':([3,4,7,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,],[21,-20,-19,-17,-20,21,21,21,21,21,21,21,21,21,21,21,-14,-15,21,-18,]),'EQUALS':([4,],[22,]),'RPAREN':([7,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,39,],[-19,-17,-20,39,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-18,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'block':([0,],[1,]),'statement':([0,1,],[2,8,]),'expression':([0,1,5,6,10,11,12,13,14,15,16,17,18,19,20,21,22,],[3,3,23,25,26,27,28,29,30,31,32,33,34,35,36,37,38,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> block","S'",1,None,None,None),
  ('block -> block statement','block',2,'p_block','calc.py',112),
  ('block -> statement','block',1,'p_block','calc.py',113),
  ('statement -> expression SEMICOLON','statement',2,'p_statement_expr','calc.py',124),
  ('expression -> expression EQUALITY expression','expression',3,'p_expression','calc.py',129),
  ('expression -> expression INEQUALITY expression','expression',3,'p_expression','calc.py',130),
  ('expression -> expression LESS expression','expression',3,'p_expression','calc.py',131),
  ('expression -> expression MORE expression','expression',3,'p_expression','calc.py',132),
  ('expression -> expression LESS_OR_EQUAL expression','expression',3,'p_expression','calc.py',133),
  ('expression -> expression MORE_OR_EQUAL expression','expression',3,'p_expression','calc.py',134),
  ('expression -> expression AND expression','expression',3,'p_expression','calc.py',135),
  ('expression -> expression OR expression','expression',3,'p_expression','calc.py',136),
  ('expression -> expression PLUS expression','expression',3,'p_expression','calc.py',137),
  ('expression -> expression MINUS expression','expression',3,'p_expression','calc.py',138),
  ('expression -> expression TIMES expression','expression',3,'p_expression','calc.py',139),
  ('expression -> expression DIVIDE expression','expression',3,'p_expression','calc.py',140),
  ('statement -> NAME EQUALS expression SEMICOLON','statement',4,'p_statement_assign','calc.py',145),
  ('expression -> MINUS expression','expression',2,'p_expression_uminus','calc.py',150),
  ('expression -> LPAREN expression RPAREN','expression',3,'p_expression_group','calc.py',155),
  ('expression -> NUMBER','expression',1,'p_expression_number','calc.py',160),
  ('expression -> NAME','expression',1,'p_expression_name','calc.py',165),
]
