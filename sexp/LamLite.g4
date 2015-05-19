/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

grammar LamLite;



program	->	{lexeme | whitespace }
lexeme	->	qvarid | qconid | qvarsym | qconsym
|	literal | special | reservedop | reservedid
literal	->	integer | float | char | string
special	->	( | ) | , | ; | [ | ] | `| { | }
whitespace	->	whitestuff {whitestuff}
whitestuff	->	whitechar | comment | ncomment
whitechar	->	newline | vertab | space | tab | uniWhite
newline	->	return linefeed | return | linefeed | formfeed
return	->	a carriage return
linefeed	->	a line feed
vertab	->	a vertical tab
formfeed	->	a form feed
space	->	a space
tab	->	a horizontal tab
uniWhite	->	any Unicode character defined as whitespace
comment	->	dashes [ any<symbol> {any}] newline
dashes	->	-- {-}
opencom	->	{-
closecom	->	-}
ncomment	->	opencom ANYseq {ncomment ANYseq}closecom
ANYseq	->	{ANY}<{ANY}( opencom | closecom ) {ANY}>
ANY	->	graphic | whitechar
any	->	graphic | space | tab
graphic	->	small | large | symbol | digit | special | : | " | '
small	->	ascSmall | uniSmall | _
ascSmall	->	a | b | ... | z
uniSmall	->	any Unicode lowercase letter
large	->	ascLarge | uniLarge
ascLarge	->	A | B | ... | Z
uniLarge	->	any uppercase or titlecase Unicode letter
symbol	->	ascSymbol | uniSymbol<special | _ | : | " | '>
ascSymbol	->	! | # | $ | % | & | * | + | . | / | < | = | > | ? | @
|	\ | ^ | | | - | ~
uniSymbol	->	any Unicode symbol or punctuation
digit	->	ascDigit | uniDigit
ascDigit	->	0 | 1 | ... | 9
uniDigit	->	any Unicode decimal digit
octit	->	0 | 1 | ... | 7
hexit	->	digit | A | ... | F | a | ... | f
varid	->	(small {small | large | digit | ' })<reservedid>
conid	->	large {small | large | digit | ' }
reservedid	->	case | class | data | default | deriving | do | else
|	if | import | in | infix | infixl | infixr | instance
|	let | module | newtype | of | then | type | where | _
varsym	->	( symbol {symbol | :})<reservedop | dashes>
consym	->	(: {symbol | :})<reservedop>
reservedop	->	.. | : | :: | = | \ | | | <- | -> | @ | ~ | =>
varid			(variables)
conid			(constructors)
tyvar	->	varid	(type variables)
tycon	->	conid	(type constructors)
tycls	->	conid	(type classes)
modid	->	conid	(modules)
qvarid	->	[ modid . ] varid
qconid	->	[ modid . ] conid
qtycon	->	[ modid . ] tycon
qtycls	->	[ modid . ] tycls
qvarsym	->	[ modid . ] varsym
qconsym	->	[ modid . ] consym
decimal	->	digit{digit}
octal	->	octit{octit}
hexadecimal	->	hexit{hexit}
integer	->	decimal
|	0o octal | 0O octal
|	0x hexadecimal | 0X hexadecimal
float	->	decimal . decimal [exponent]
|	decimal exponent
exponent	->	(e | E) [+ | -] decimal
char	->	' (graphic<' | \> | space | escape<\&>) '
string	->	" {graphic<" | \> | space | escape | gap}"
escape	->	\ ( charesc | ascii | decimal | o octal | x hexadecimal )
charesc	->	a | b | f | n | r | t | v | \ | " | ' | &
ascii	->	^cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
|	BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
|	DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN
|	EM | SUB | ESC | FS | GS | RS | US | SP | DEL
cntrl	->	ascLarge | @ | [ | \ | ] | ^ | _
gap	->	\ whitechar {whitechar}\



VARID 
    : SMALL (SMALL | LARGE | DIGIT | '_')*
    ;

CONID
    : LARGE (SMALL | LARGE | DIGIT | '_')*
    ;

VARSYM 
    : SYMBOL (SYMBOL | ':')*
    ;

CONSYM 
    : ':' (SYMBOL | ':')*
    ;

SMALL
    : ASCII_SMALL
    ;

LARGE 
    : ASCII_LARGE 
    ;

SYMBOL 
    : ASCII_SYMBOL
    ;

DIGIT
    : ASCII_DIGIT
    ;

fragment ASCII_SMALL 
    : [a-z]
    ;

fragment ASCII_LARGE 
    : [A-Z]
    ;

fragment ASCII_SYMBOL 
    : [!#$%&*+./<=>?@\^|~] | '-'
    ;

fragment ASCII_DIGIT 
    : [0-9]
    ;

fragment SPECIAL_SYM 
    : [(),;\[\]`{}]
    ;

GAP
    : [ \t\r\n\u000C]+ -> skip
    ;

LINE_COMMENT
    : '--' ~[\r\n]* -> skip
    ;

BLOCK_COMMENT 
    : '{-' .*? '-}' -> skip 
    ;

REST_COMMENT 
    : '----' .*? -> skip
    ;