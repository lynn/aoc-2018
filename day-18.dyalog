⍝ https://adventofcode.com/2018/day/18 part 1 in Dyalog APL.
⎕IO ← 0
area ← 50 50 ↑ 50 51 ⍴ '...input goes here...'
tl ← {+⌿(,⍵)∘.='|#'}
rule ← {X←⍵[⊂1 1] ⋄ T L←(tl ⍵)-(tl X) ⋄ X='.':'.|'[T≥3] ⋄ X='|':'|#'[L≥3] ⋄ '.#'[0≠T⌊L]}
×/ tl (rule⌺3 3⍣10) area
