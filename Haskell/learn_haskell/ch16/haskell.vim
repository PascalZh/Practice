if exists('g:no_haskell_conceal') || !has('conceal') || &enc != 'utf-8'
    finish
endif

hi! link Conceal Operator

function! s:HSConceal(from, to, name, preserve, word)
    let pattern_ = a:word ? '\<'.a:from.'\>' : a:from
    if !a:preserve
        exe 'syntax match Operator /'.pattern_.'/ conceal cchar='.a:to
        return
    endif
    " only if a:preserve is v:true, a:operator will be used.
    " TODO if a:from has more than one a:from[0] in it, this will fail.
    let hsc = []
    for i in range(len(a:from))
        let l:cchar = i == 0 ? a:to : ' '
        let pat = '/' . a:from[i] . '/'
        exe 'syntax match HSC_' . a:name . string(i) . ' contained ' . pat
                    \ . ' conceal cchar=' . l:cchar
        let hsc += ['HSC_'.a:name.string(i)]
    endfor
    exe 'syntax match HSC_Operator "'.pattern_.'" contains='.join(hsc, ',')
endfunction

let s:symbolList = [
            \      [ 'undefined' , '⊥' , 'Undefined' , v:false , v:true  ]
            \ ,    [ 'forall'    , '∀' , 'Forall'    , v:true  , v:true  ]
            \ ]
            "\ ,    [ '<='        , '⩽' , 'LT'        , v:true  , v:false ]
            "\ ,    [ '>='        , '⩾' , 'Gt'        , v:true  , v:false ]
            "\ ,    [ '=='        , '≡' , 'Eq'        , v:false , v:false ]
            "\ ,    [ '\/='       , '≢' , 'NotEq'     , v:false , v:false ]

let s:symbolList += [
            \       [ 'reducedPlanckConstant\|planckConstantOver2Pi\|hbar\|hslash', 'ℏ', 'Hbar', v:false , v:true]
            \ ,     [ 'pi'             , 'π' , 'Pi'            , v:false , v:true  ]
            \ ,     [ 'tau'            , 'τ' , 'Tau'           , v:false , v:true  ]
            \ ,     [ 'planckConstant' , 'ℎ' , 'PlankConstant' , v:false , v:true  ]
            \ ,     [ 'sum'            , '∑' , 'Sum'           , v:false , v:true  ]
            \ ,     [ 'product'        , '∏' , 'Product'       , v:false , v:true  ]
            \ ,     [ 'sqrt'           , '√' , 'Sqrt'          , v:false , v:true  ]
            \ ,     [ 'not'            , '¬' , 'Not'           , v:true  , v:true  ]
            \ ,     [ 'realPart'       , 'ℜ' , 'RealPart'      , v:true  , v:true  ]
            \ ,     [ 'imagPart'       , 'ℑ' , 'ImagPart'      , v:true  , v:true  ]
            \ ,     [ '*'              , '⋅' , 'Multiply'      , v:false , v:false ]
            \ ]

let s:symbolList += [
            \ , [ 'return' , 'η' , 'Return' , v:false , v:true]
            \ , [ 'join'   , 'µ' , 'Join'   , v:false , v:true]
            \
            \ ]

"let s:symbolList += [
            "\   ['-<'  , '↢' , 'LArrow1' , v:false , v:false ]
            "\ , ['>-'  , '↣' , 'RArrow1' , v:false , v:false ]
            "\ , ['-<<' , '⤛' , 'LArrow2' , v:false , v:false ]
            "\ , ['>>-' , '⤜' , 'RArrow2' , v:true  , v:false ]
            "\ ]

let s:symbolList += [
            \       [ '`div`'    , '÷' , 'Divide'   , v:false , v:false ]
            \ ,     [ 'Either'   , '𝐄' , 'Either'   , v:true  , v:true  ]
            \ ,     [ 'Right'    , '𝑅' , 'Right'    , v:true  , v:true  ]
            \ ,     [ 'Maybe'    , '𝐌' , 'Maybe'    , v:true  , v:true  ]
            \ ,     [ 'Just'     , '𝐽' , 'Just'     , v:true  , v:true  ]
            \ ,     [ 'Nothing'  , '𝑁' , 'Nothing'  , v:true  , v:true  ]
            \ ,     [ 'True'     , '𝐓' , 'True'     , v:true  , v:true  ]
            \ ,     [ 'False'    , '𝐅' , 'False'    , v:true  , v:true  ]
            \ ,     [ 'String'   , '𝐒' , 'String'   , v:true  , v:true  ]
            \ ,     [ 'Text'     , '𝐓' , 'Text'     , v:true  , v:true  ]
            \ ,     [ 'Bool'     , '𝔹'  , 'Bool'     , v:true  , v:true  ]
            \ ,     [ 'Rational' , 'ℚ'  , 'Rational' , v:true  , v:true  ]
            \ ,     [ 'Integer'  , 'ℤ'  , 'Integer'  , v:true  , v:true  ]
            \ ,     [ 'Natural'  , 'ℕ'  , 'Natural'  , v:true  , v:true  ]
            \ ,     [ 'Nat'      , 'ℕ'  , 'Nat'      , v:true  , v:true  ]
            \ ,     [ 'Double'   , '𝔻'  , 'Double'   , v:true  , v:true  ]
            \ ,     [ 'Complex'  , 'ℂ'  , 'Complex'  , v:true  , v:true  ]
            \                                                           ]

"let s:symbolList += [
            "\ ['>>', '≫', 'RArrow3', v:false, v:false]
            "\ ]

syntax match Operator "\\\ze[[:alpha:][:space:]_([]" conceal cchar=λ
syntax match Operator /\s\.\s/ms=s+1,me=e-1 conceal cchar=∘

for sym in s:symbolList
    call s:HSConceal(sym[0],sym[1],sym[2],sym[3],sym[4])
endfor

syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)0\ze\_W" conceal cchar=⁰
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)1\ze\_W" conceal cchar=¹
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)2\ze\_W" conceal cchar=²
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)3\ze\_W" conceal cchar=³
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)4\ze\_W" conceal cchar=⁴
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)5\ze\_W" conceal cchar=⁵
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)6\ze\_W" conceal cchar=⁶
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)7\ze\_W" conceal cchar=⁷
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)8\ze\_W" conceal cchar=⁸
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)9\ze\_W" conceal cchar=⁹
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)a\ze\_W" conceal cchar=ᵃ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)b\ze\_W" conceal cchar=ᵇ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)c\ze\_W" conceal cchar=ᶜ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)d\ze\_W" conceal cchar=ᵈ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)e\ze\_W" conceal cchar=ᵉ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)f\ze\_W" conceal cchar=ᶠ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)g\ze\_W" conceal cchar=ᵍ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)h\ze\_W" conceal cchar=ʰ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)i\ze\_W" conceal cchar=ⁱ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)j\ze\_W" conceal cchar=ʲ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)k\ze\_W" conceal cchar=ᵏ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)l\ze\_W" conceal cchar=ˡ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)m\ze\_W" conceal cchar=ᵐ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)n\ze\_W" conceal cchar=ⁿ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)o\ze\_W" conceal cchar=ᵒ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)p\ze\_W" conceal cchar=ᵖ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)r\ze\_W" conceal cchar=ʳ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)s\ze\_W" conceal cchar=ˢ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)t\ze\_W" conceal cchar=ᵗ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)u\ze\_W" conceal cchar=ᵘ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)v\ze\_W" conceal cchar=ᵛ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)w\ze\_W" conceal cchar=ʷ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)x\ze\_W" conceal cchar=ˣ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)y\ze\_W" conceal cchar=ʸ
syntax match hsNiceOperator "\(\*\*\|\^\|\^\^\)z\ze\_W" conceal cchar=ᶻ
