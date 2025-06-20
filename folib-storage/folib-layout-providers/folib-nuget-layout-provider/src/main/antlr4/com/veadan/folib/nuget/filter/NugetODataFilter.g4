grammar NugetODataFilter;

filter
:
    filterExp
;

filterExp
:
    '(' vNestedFilterExp = filterExp ')'
    | vFilterExpLeft = filterExp vLogicalOp = logicalOp vFilterExpRight = filterExp
    | tokenExp
;

tokenExp
:
    vTokenExpLeft = tokenExpLeft vFilterOp = filterOp vTokenExpRight = tokenExpRight
    | TAG
;

tokenExpRight
:
    '\'' VALUE '\''
;

tokenExpLeft
:
    ATTRIBUTE
    | tokenExpFunction
;

tokenExpFunction
:
    fuctionExp '(' ATTRIBUTE ')'
;

fuctionExp
:
    TO_LOWER
;

filterOp
:
    EQ
    | GE
;

logicalOp
:
    AND
    | OR
;

TO_LOWER
:
    'tolower'
;

TAG
:
    'IsLatestVersion'
;

ATTRIBUTE
:
    'Id'
    | 'Version'
;

EQ
:
    'eq'
;

GE
:
    'ge'
;

AND
:
    'and'
;

OR
:
    'or'
;

NOT
:
    'not'
;

VALUE
:
    [a-zA-Z_0-9.\-]+
;


WHITESPACE
:
    ' ' -> skip
;