⍝ https://adventofcode.com/2018/day/18 in Dyalog APL.
⎕IO ← 0
area ← 50 50 ↑ 50 51 ⍴ '...input goes here...'
tl ← {+⌿(,⍵)∘.='|#'}
rule ← {X←⍵[⊂1 1] ⋄ T L←(tl ⍵)-(tl X) ⋄ X='.':'.|'[T≥3] ⋄ X='|':'|#'[L≥3] ⋄ '.#'[0≠T⌊L]}
×/ tl (rule⌺3 3⍣10) area

⍝ Part 2:
tls ← {⍺=0:⍬⋄ (⊂tl ⍵),(⍺-1)∇step ⍵}
history ← 1000 tls area
⎕ ← history   ⍝ I eyeballed the eventual period-28 oscillation.

i ← 999+28|(1000000000-999)
⎕← ×/ tl (step⍣i) area
