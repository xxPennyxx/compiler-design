grammar hello3;

start returns [Integer val] : e1=exp{$val = $e1.val;System.out.println($val);};
exp returns [Integer val] : e1 = exp PLUS t = term {$val = $e1.val + $t.val;} | e1 = exp MINUS t = term {$val = $e1.val - $t.val;} | t = term {$val = $t.val;};
term returns [Integer val] : t=term MULT f=fact {$val = $t.val * $f.val;} | t=term DIV f=fact {$val = $t.val / $f.val;} | f = fact{$val = $f.val;};
fact returns [Integer val] : NUM {$val=	Integer.parseInt($NUM.text);} | LPAR e =exp RPAR {$val = $e.val;};

PLUS : '+';
MINUS : '-';
MULT : '*';
DIV : '/';
NUM : ('0' .. '9')+;
LPAR : '(';
RPAR : ')';

