#lang racket

(module+ test
  (require rackunit
           xdsmith/normalize)

  (check-equal? (normalize-js #<<EOF
false-1952553523
()
-1915767876multiset{[2046490000]}
[{{()}}]D
$d>++|^j?%:i*^StvK<AfdNC|&-509650175
-1737838651(B, ($~PKs-lk, {({false, true}, (!, true)), ({true}, (-, true)), ({true}, (C, true)), ({}, (c, true))}))
function (_75_arg__331, _76_arg__332, _77_arg__333, _78_arg__334) {
          return _dafny.MultiSet.fromElements('d', _74_lift__335);
        }
((jBatOChV=z=gNPe'tGnOifP^vk, -1377431704, [multiset{?}, multiset{N}]), 752723663, [])true
{}>
ylig'I:W>GnpR%L:
332839281multiset{%, w, u}
falsetrue
720719814
{(true, 1236021549)}
false1
falsemultiset{((), []), ((), [-2026845849])}

EOF
                              )
                #<<EOF
false-1952553523
()
-1915767876multiset{[2046490000]}
[{{()}}]D
$d>++|^j?%:i*^StvK<AfdNC|&-509650175
-1737838651(B ,  ($~PKs-lk ,  {({false, true} ,  (! ,  true)), ({true} ,  (- ,  true)), ({true} ,  (C ,  true)), ({} ,  (c ,  true))}))
Function
((jBatOChV=z=gNPe'tGnOifP^vk ,  -1377431704 ,  [multiset{?}, multiset{N}]) ,  752723663 ,  [])true
{}>
ylig'I:W>GnpR%L:
332839281multiset{%, u, w}
falsetrue
720719814
{(true ,  1236021549)}
false1
falsemultiset{(() ,  [-2026845849]), (() ,  [])}

EOF
                )

  (check-equal? (normalize-go #<<EOF
false-1952553523
()
-1915767876multiset{[2046490000]}
[{{()}}]D
$d>++|^j?%:i*^StvK<AfdNC|&-509650175
-1737838651(B, ($~PKs-lk, {({false, true}, (!, true)), ({true}, (-, true)), ({true}, (C, true)), ({}, (c, true))}))
func(dafny.Tuple, dafny.Tuple, dafny.Seq, dafny.Int) dafny.MultiSet
((jBatOChV=z=gNPe'tGnOifP^vk, -1377431704, [multiset{?}, multiset{N}]), 752723663, [])true
{}>
ylig'I:W>GnpR%L:
332839281multiset{%, w, u}
falsetrue
720719814
{(true, 1236021549)}
false1
falsemultiset{((), []), ((), [-2026845849])}

EOF
                             )
                #<<EOF
false-1952553523
()
-1915767876multiset{[2046490000]}
[{{()}}]D
$d>++|^j?%:i*^StvK<AfdNC|&-509650175
-1737838651(B ,  ($~PKs-lk ,  {({false, true} ,  (! ,  true)), ({true} ,  (- ,  true)), ({true} ,  (C ,  true)), ({} ,  (c ,  true))}))
Function
((jBatOChV=z=gNPe'tGnOifP^vk ,  -1377431704 ,  [multiset{?}, multiset{N}]) ,  752723663 ,  [])true
{}>
ylig'I:W>GnpR%L:
332839281multiset{%, u, w}
falsetrue
720719814
{(true ,  1236021549)}
false1
falsemultiset{(() ,  [-2026845849]), (() ,  [])}

EOF
                )

  (check-equal? (normalize-cs #<<EOF
false-1952553523
()
-1915767876multiset{[2046490000]}
[{{()}}]D
$d>++|^j?%:i*^StvK<AfdNC|&-509650175
-1737838651(B, ($~PKs-lk, {({false, true}, (!, true)), ({true}, (-, true)), ({true}, (C, true)), ({}, (c, true))}))
System.Func`5[_System._ITuple3`3[Dafny.ISequence`1[System.Char],Dafny.IMultiSet`1[System.Boolean],System.Numerics.BigInteger],_System._ITuple2`2[_System._ITuple3`3[System.Numerics.BigInteger,System.Char,System.Boolean],_System._ITuple0],Dafny.ISequence`1[System.Numerics.BigInteger],System.Numerics.BigInteger,Dafny.IMultiSet`1[System.Char]]
((jBatOChV=z=gNPe'tGnOifP^vk, -1377431704, [multiset{?}, multiset{N}]), 752723663, [])true
{}>
ylig'I:W>GnpR%L:
332839281multiset{%, u, w}
falsetrue
720719814
{(true, 1236021549)}
false1
falsemultiset{((), [-2026845849]), ((), [])}

EOF
                              )
                #<<EOF
false-1952553523
()
-1915767876multiset{[2046490000]}
[{{()}}]D
$d>++|^j?%:i*^StvK<AfdNC|&-509650175
-1737838651(B ,  ($~PKs-lk ,  {({false, true} ,  (! ,  true)), ({true} ,  (- ,  true)), ({true} ,  (C ,  true)), ({} ,  (c ,  true))}))
Function
((jBatOChV=z=gNPe'tGnOifP^vk ,  -1377431704 ,  [multiset{?}, multiset{N}]) ,  752723663 ,  [])true
{}>
ylig'I:W>GnpR%L:
332839281multiset{%, u, w}
falsetrue
720719814
{(true ,  1236021549)}
false1
falsemultiset{(() ,  [-2026845849]), (() ,  [])}

EOF

)

  (check-equal? (normalize-java #<<EOF
false-1952553523
()
-1915767876multiset{[2046490000]}
[{{()}}]D
$d>++|^j?%:i*^StvK<AfdNC|&-509650175
-1737838651(B, ($~PKs-lk, {({false, true}, (!, true)), ({}, (c, true)), ({true}, (C, true)), ({true}, (-, true))}))
Function
((jBatOChV=z=gNPe'tGnOifP^vk, -1377431704, [multiset{?}, multiset{N}]), 752723663, [])true
{}>
ylig'I:W>GnpR%L:
332839281multiset{%, u, w}
falsetrue
720719814
{(true, 1236021549)}
false1
falsemultiset{((), [-2026845849]), ((), [])}

EOF
                                )
                #<<EOF
false-1952553523
()
-1915767876multiset{[2046490000]}
[{{()}}]D
$d>++|^j?%:i*^StvK<AfdNC|&-509650175
-1737838651(B ,  ($~PKs-lk ,  {({false, true} ,  (! ,  true)), ({true} ,  (- ,  true)), ({true} ,  (C ,  true)), ({} ,  (c ,  true))}))
Function
((jBatOChV=z=gNPe'tGnOifP^vk ,  -1377431704 ,  [multiset{?}, multiset{N}]) ,  752723663 ,  [])true
{}>
ylig'I:W>GnpR%L:
332839281multiset{%, u, w}
falsetrue
720719814
{(true ,  1236021549)}
false1
falsemultiset{(() ,  [-2026845849]), (() ,  [])}

EOF
                ))
