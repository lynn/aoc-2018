-- https://adventofcode.com/2018/day/16 in Idris.
import Data.Bits as B
import Data.Fin
import Data.Vect

-- On HARD MODE.
%default total

-- This is how you know it's going to be "fun" code:
ℤ : Type
ℤ = Integer

-- Let's describe our machine a little. It has about yea many registers:
numRegs : Nat
numRegs = 4

-- Said registers are 0, 1, 2, and 3. Hey, I know what that type is called!!
Register : Type
Register = Fin numRegs

-- Their values are represented by a 4-tuple of integers, then.
RegisterValues : Type
RegisterValues = Vect numRegs ℤ

-- Oh, it can do these tricks:
bAnd : ℤ -> ℤ -> ℤ
bAnd x y = bitsToInt (intToBits {n=32} x `B.and` intToBits y)

bOr : ℤ -> ℤ -> ℤ
bOr x y = bitsToInt (intToBits {n=32} x `B.or` intToBits y)

-- And these:
gtZ : ℤ -> ℤ -> ℤ
gtZ a b = if a > b then 1 else 0

eqZ : ℤ -> ℤ -> ℤ
eqZ a b = if a == b then 1 else 0

-- We'll also want to do this at some point:
intToFin : (n : Nat) -> ℤ -> Maybe (Fin n)
intToFin n z = natToFin (fromIntegerNat z) n

-- Anyway, here are the opcodes.
data Opcode = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori
            | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr

-- All of them in a list:
allOpcodes : List Opcode
allOpcodes = [Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori,
              Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr]

-- Idris doesn't have "deriving", which is a little sad, and means we have to write this:
implementation Eq Opcode where
  Addr == Addr = True
  Addi == Addi = True
  Mulr == Mulr = True
  Muli == Muli = True
  Banr == Banr = True
  Bani == Bani = True
  Borr == Borr = True
  Bori == Bori = True
  Setr == Setr = True
  Seti == Seti = True
  Gtir == Gtir = True
  Gtri == Gtri = True
  Gtrr == Gtrr = True
  Eqir == Eqir = True
  Eqri == Eqri = True
  Eqrr == Eqrr = True
  _ == _ = False

-- Whew. Anyway, here's how we represent instructions in their most executable form.
data Source = Imm ℤ | Reg Register   -- Where an operand comes from.
data Instruction = Inst (ℤ -> ℤ -> ℤ) Register Source Source

-- We run them like so (it's pretty straightforward).
execute : Instruction -> RegisterValues -> RegisterValues
execute (Inst f c srcA srcB) regs =
  replaceAt c result regs
    where grab : Source -> ℤ
          grab (Imm i) = i
          grab (Reg i) = index i regs
          result = f (grab srcA) (grab srcB)

-- Now, to actually map Opcode × (ℤ, ℤ, Register) to those high-level Instructions!

-- We have some helper functions of type (ℤ -> Maybe Source), which means they try
-- to map a number like 3 into a Source like `Just (Reg (the Register 3))`. But, for
-- example, reg' 7 will yield `Nothing`, 'cause there's no register 7.
reg' : ℤ -> Maybe Source
reg' = map Reg . intToFin numRegs

-- Pretty much any number can be an immediate argument, though.
imm' : ℤ -> Maybe Source
imm' = map Imm . Just

-- The translation is achieved by this juicy big table:
toInstruction : Opcode -> (ℤ, ℤ, Register) -> Maybe Instruction
toInstruction Addr (a, b, c) = Inst (+)   c <$> reg' a <*> reg' b
toInstruction Addi (a, b, c) = Inst (+)   c <$> reg' a <*> imm' b
toInstruction Mulr (a, b, c) = Inst (*)   c <$> reg' a <*> reg' b
toInstruction Muli (a, b, c) = Inst (*)   c <$> reg' a <*> imm' b
toInstruction Banr (a, b, c) = Inst bAnd  c <$> reg' a <*> reg' b
toInstruction Bani (a, b, c) = Inst bAnd  c <$> reg' a <*> imm' b
toInstruction Borr (a, b, c) = Inst bOr   c <$> reg' a <*> reg' b
toInstruction Bori (a, b, c) = Inst bOr   c <$> reg' a <*> imm' b
toInstruction Setr (a, b, c) = Inst const c <$> reg' a <*> imm' b
toInstruction Seti (a, b, c) = Inst const c <$> imm' a <*> imm' b
toInstruction Gtir (a, b, c) = Inst gtZ   c <$> imm' a <*> reg' b
toInstruction Gtri (a, b, c) = Inst gtZ   c <$> reg' a <*> imm' b
toInstruction Gtrr (a, b, c) = Inst gtZ   c <$> reg' a <*> reg' b
toInstruction Eqir (a, b, c) = Inst eqZ   c <$> imm' a <*> reg' b
toInstruction Eqri (a, b, c) = Inst eqZ   c <$> reg' a <*> imm' b
toInstruction Eqrr (a, b, c) = Inst eqZ   c <$> reg' a <*> reg' b

-- This task involves us piecing together how `Fin 16` maps to our opcodes.
-- Let's describe how that goes:

-- An instruction we haven't yet decoded. What could it be???
data MysteryInstruction = Mystery (Fin 16) (ℤ, ℤ, Register)

-- A sample of a mystery instruction being applied to some registers.
record Sample where
  constructor MkSample
  before : RegisterValues
  instruction : MysteryInstruction
  after : RegisterValues

-- Get the instruction number out of a sample. Handy.
instructionNumber : Sample -> Fin 16
instructionNumber s =
  let Mystery n _ = instruction s in n

-- Given a sample, return a list of opcodes it *might* be.
candidates : Sample -> List Opcode
candidates (MkSample before (Mystery _ abc) after) =
  filter fitsSample allOpcodes
  where
    fitsSample op =
      case toInstruction op abc of            -- Try to decode it as `op`...
        Nothing => False                      -- If that *fails* (pffft), then `op` definitely isn't right.
        Just i => execute i before == after   -- Execute `i` and check its work, otherwise.

-- This doesn't fully narrow it down, so it's time to solve SUDOKUS (sort of).

-- Whittle a mutually exclusive list of candidate lists down to a single assignment.
-- For example, `sudoku [[a,b,c],[b],[a,b]] == Just [c,b,a]`.
-- (The middle one is b, so we scrap that from the others, and now we've isolated a... etc.)
sudoku : Eq a => {n : Nat} -> Vect n (List a) -> Maybe (Vect n a)
sudoku = solutionHopefully . untilEquality whittle
  where
    -- Okay, you got me. This isn't actually total, but for our purposes it is.
    -- (The boxes can only lose elements, so `untilEquality whittle` must terminate.)
    untilEquality : Eq b => (b -> b) -> b -> b
    untilEquality =
      assert_total $ \f, x =>
        let x' = f x in
          if x == x' then x else untilEquality f x'

    -- Remove all isolated cell values from all not-yet-isolated cells.
    whittle : Eq a => Vect n (List a) -> Vect n (List a)
    whittle boxes = map narrowDown boxes
      where
        isolated : List a   -- Gather all the values we have on lockdown so far.
        isolated = concat $ filter ((==1) . length) $ toList boxes

        unisolated : a -> Bool
        unisolated y = not (elem y isolated)

        narrowDown : List a -> List a
        narrowDown x = if length x == 1 then x else filter unisolated x

    -- Return the only element in a list.
    only : List a -> Maybe a
    only [x] = Just x
    only _ = Nothing

    -- Return the unique solution, praying there is one.
    solutionHopefully : Vect n (List a) -> Maybe (Vect n a)
    solutionHopefully = sequence . map only

-- (For example, `sudoku [[1,2],[1,2,3]]` stops early, at `[[1,2],[1,2]]`.
-- Then, `only` will return some `Nothing`s, and the whole `sequence` will fail.)

-- Anyway, now we can start to figure out the whole mapping:
OpcodeMapping : Type
OpcodeMapping = Vect 16 Opcode

-- From a list of samples, try to deduce a mapping from [0..15] to Opcodes.
deduceMapping : List Sample -> Maybe OpcodeMapping
deduceMapping samples = sudoku (map candidateOpcodes range)
  where
    candidateOpcodes : Fin 16 -> List Opcode
    candidateOpcodes i =
      let relevantSamples = filter ((== i) . instructionNumber) samples
      in [op | op <- allOpcodes, all (elem op . candidates) relevantSamples]

-- Decode a mystery instruction using a given opcode mapping.
-- (This should always return a "Just" result, if the supplied mapping is correct.)
reveal : OpcodeMapping -> MysteryInstruction -> Maybe Instruction
reveal v (Mystery i abc) = toInstruction (index i v) abc

-- Okay, time to parse the input,








-- Actually, this code is a big mess, and maybe you should look away.
-- Just scroll on down to `main`, where the exciting stuff happens.



-- No? Okay? I'm sorry ;w; here it is
partial
readNats : String -> List Nat
readNats s =
  let (nonDigits, s') = span (not . isDigit) s
      (digits, s'') = span isDigit s'
  in
    case digits of
      "" => []
      d  => cast d :: readNats s''

partial
parseMystery : String -> MysteryInstruction
parseMystery s =
  let [m, a, b, c] = readNats s
      Just m' = natToFin m 16
      Just c' = natToFin c numRegs
  in Mystery m' (cast a, cast b, c')

-- Thank you Anton Trunov (https://stackoverflow.com/a/44083910/257418)
fromListOfLength : (n : Nat) -> (xs : List a) -> Maybe (Vect n a)
fromListOfLength n xs with (decEq (length xs) n)
  fromListOfLength n xs | (Yes prf) = rewrite (sym prf) in Just (fromList xs)
  fromListOfLength n xs | (No _) = Nothing

partial
parse : List String -> (List Sample, List MysteryInstruction)
parse (""::""::programLines) = ([], map parseMystery programLines)
parse (b::i::a::""::rest) =
  let
    Just before = fromListOfLength 4 $ map toIntegerNat (readNats b)
    instruction = parseMystery i
    Just after  = fromListOfLength 4 $ map toIntegerNat (readNats a)
    sample      = MkSample before instruction after
    (ss, ms)    = parse rest
  in (sample::ss, ms)




-- Okay, this is where the magic happens!!!
partial
main : IO ()
main = do
  -- Parse the input.
  Right input <- fGetChars stdin 99999
  let (samples, program) = parse (lines input)

  putStrLn "Task 1:"
  printLn $ length [s | s <- samples, length (candidates s) >= 3]

  putStrLn "Task 2:"
  let Just mapping = deduceMapping samples
  let Just instructions = sequence (reveal mapping <$> program)
  printLn $ foldl (flip execute) [0,0,0,0] instructions
