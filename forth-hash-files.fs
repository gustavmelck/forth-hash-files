\ forth hash-files
\ by gustav melck, october 2020
\ vim: fdm=marker

\ {{{
private{

: gthrow  ( ior addr u -- )
    2 pick 0<>  if  type ." ; forth-hash-files error " dup . cr throw  else  2drop drop  then  ;

0 value fid  0 value hash-size

create buffer 256 chars allot  0 value bufferlen

: buffer+  ( addr u -- )  dup >r buffer bufferlen + swap cmove  r> bufferlen + to bufferlen  ;
: 0buffer+  ( addr u -- )  0 to bufferlen  buffer+  ;

2variable register

: file>  ( faddr -- )  s>d fid reposition-file s" file> error1" gthrow  ;

: file!  ( item faddr -- )  file>  register ! register cell fid write-file s" file! error1" gthrow  ;
: 2file!  ( item1 item2 faddr -- )
    file>  register cell+ ! register !  register 2 cells fid write-file s" 2file! error1" gthrow  ;
: file@  ( faddr -- item )
    file>  register cell fid read-file s" file@ error1" gthrow  cell <> s" file@ error2" gthrow  register @  ;
: 2file@  ( faddr -- item1 item2)
    file>  register 2 cells fid read-file s" 2file@ error2" gthrow 2 cells <> s" 2file@ error3" gthrow
    register @ register cell+ @  ;

: fcons  ( -- faddr )  fid file-size s" fcons error1" gthrow d>s >r  0 0 r@ 2file!  r>  ;
: fcar!  ( item faddr -- )  file!  ;
: fcar@  ( faddr -- item )  file@  ;
: fcdr!  ( item faddr -- )  cell+ file!  ;
: fcdr@  ( faddr -- item )  cell+ file@  ;

: +flist  ( item faddr -- faddr' )  fcons dup >r  fcdr!  r@ fcar!  r>  ;

create current-key 256 chars allot  0 value current-key-len

: (find-key-cons-faddr)  ( flist in-loop? -- faddr )
    if  r> drop  then
    ?dup 0=  if  0  else
        dup >r fcar@ fcar@ file@ dup to bufferlen  buffer swap fid read-file s" (find-key-cons-faddr) error1" gthrow
        bufferlen <> s" (find-key-cons-faddr) error2" gthrow
        current-key current-key-len buffer bufferlen compare 0=  if  r> fcar@  else  r> fcdr@ true recurse  then
    then  ;
: find-key-cons-faddr  ( flist -- faddr )  false (find-key-cons-faddr)  ;

: (str>hash)  ( addr u hash in-loop? -- hash' )
    if  r> drop  then  >r
    ?dup 0=  if  drop r>  else
        over c@  r@ 5 lshift r> + xor  hash-size mod >r  1- swap 1+ swap  r> true recurse
    then  ;
: str>hash  ( addr u -- u' )  5381 hash-size mod  false (str>hash)  ;

0 value new-fid

: (initialise-hash-file)  ( u in-loop? -- )
    if  r> drop  then
    ?dup 0<>  if
        buffer cell new-fid write-file s" (initialise-hash-file) error 1" gthrow  1- true recurse
    then  ;
: initialise-hash-file  ( u -- )
    dup buffer !  buffer cell new-fid write-file s" initialise-hash-file error1" gthrow
    0 buffer !  false (initialise-hash-file)  ;

0 value kv-faddr  0 value hash-faddr

: fs  ( -- )  fid file-size s" fs error1" gthrow d>s ." file size: " .  ;

}private
\ }}}

: make-hash-file  ( addr u size -- )  \ this leaves the new file closed; open it using open-hash-file
    >r w/o bin open-file s" make-hash-file error1" gthrow to new-fid
    r> initialise-hash-file  new-fid close-file s" make-hash-file error2" gthrow  ;
: open-hash-file  ( addr u -- )  r/w bin open-file s" open-hash-file error1" gthrow to fid  0 file@ to hash-size  ;
: close-hash-file  ( -- )  fid close-file s" close-hash-file error1" gthrow  ;

: create-hf-vial  ( -- addr )  2 cells allocate s" create-hf-vial error1" gthrow  ;
: free-hf-vial  ( addr -- )  free s" free-hf-vial error1" gthrow  ;
: suspend-hash-file  ( addr -- )  >r fid r@ !  hash-size r> cell+ !  ;
: resume-hash-file  ( addr -- )  dup >r @ to fid  r> cell+ @ to hash-size  ;

: with-hf-key  ( addr u -- exists? )
    2dup dup to current-key-len current-key swap cmove
    str>hash 1+ cells dup to hash-faddr file@ find-key-cons-faddr dup to kv-faddr 0<>  ;
: hf-item!  ( addr u -- faddr )  \ write arbitrary number (u) of bytes, starting at addr, to the ht-file
    \ first write a cell, storing u
    fid file-size s" hf-item! error1" gthrow d>s >r dup r@ file!  fid write-file s" hf-item! error2" gthrow  r>  ;
: hf-item-len  ( faddr -- u )  file@  ;
: hf-item@  ( faddr addr u -- u' )
    rot file@ min dup >r  fid read-file s" hf-item@ error1" gthrow  r@ <> s" hf-item@ error2" gthrow  r>  ;
: hf!  ( item -- )
    kv-faddr 0=  if
        fcons to kv-faddr  fcons >r  kv-faddr r@ fcar!  hash-faddr file@ r@ fcdr!  r> hash-faddr file!
        current-key current-key-len hf-item! kv-faddr fcar!
    then  kv-faddr fcdr!  ;
: hf@  ( -- item )  kv-faddr dup 0<> swap fcdr@ and  ;

privatize

\ : test  ( -- )  \ {{{
\     s" gustav1" 2dup file-status nip 0<>  if  2dup 8 make-hash-file  then
\     open-hash-file
\     s" key-one" with-hf-key  if  hf@  else  0  then  10 + hf! hf@ .
\     s" key-two" with-hf-key  if  hf@  dup 3 >=  if  drop -1  then  else  0  then  1 + hf! hf@ .
\     s" key-three" with-hf-key .  s" gustav" hf-item! hf!  hf@ pad 10 hf-item@ cr pad swap type ." ;" cr
\     close-hash-file  ;
\ 
\ test .s
\ }}}

