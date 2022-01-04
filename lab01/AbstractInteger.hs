{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- Lewis Arnsten, Lab01
-- A Simple Reverse Polish Notation Calculator

-- A module for defining the integers.

module AbstractInteger where

-- The definition of AbstractNatural for the natural numbers.

data AbstractNatural =  Zero | S AbstractNatural
    deriving (Show)

-- The equality comparisons for AbstractNatural. 

instance Eq AbstractNatural where
   Zero == Zero = True
   Zero == S _  = False
   S _  == Zero = False
   S x  == S y  = x == y

instance Ord AbstractNatural where
   Zero <= Zero = True
   Zero <= S _  = True
   S _  <= Zero = False
   S x  <= S y  = x <= y

-- The addition function for AbstracctNatural. 
addN :: AbstractNatural -> AbstractNatural -> AbstractNatural
addN x Zero = x
addN Zero x = x
addN (S x) y = S (addN x y)

-- The difference function for AbstractNatural.
diffN :: AbstractNatural -> AbstractNatural -> AbstractNatural
diffN Zero x = x
diffN x Zero = x
diffN (S x) (S y) = diffN x y

-- The multiplication function for AbstractNatural.
multN :: AbstractNatural -> AbstractNatural -> AbstractNatural -> AbstractNatural
multN Zero x z = Zero
multN x Zero z = Zero
multN x (S Zero) z = x
multN (S Zero) x z = x
multN x (S y) z = multN (addN x z) y z

-- Division and modulus function for AbstractNatural. Given a numerator and denominator 
-- it returns a tuple containing the quotient and remainder.
-- I recieved help from Professor Kurtz while creating this function.
divModN :: AbstractNatural -> AbstractNatural -> (AbstractNatural, AbstractNatural)
divModN Zero x = (Zero, Zero)
divModN x (S Zero) = (x, Zero)
divModN (S y) x =
    if addN r' (S Zero) == x
    then (S x', Zero)
    else (x', S r')
    where
    (x', r') = divModN y x

-- The definition of AbstractInteger. Establishes positive and negatives or AbstractNatural.
data AbstractInteger = Pos AbstractNatural | Neg AbstractNatural 
    deriving (Show)

-- The successor function counts forward one by one.
successor :: AbstractInteger -> AbstractInteger
successor (Pos x) = Pos (S x)
successor (Neg (S x)) = Neg x
successor (Neg Zero) = Pos (S Zero)

-- The predecessor function counts backwards one by one.
predecessor :: AbstractInteger -> AbstractInteger
predecessor (Pos (S x)) = Pos x
predecessor (Neg x) = Neg (S x)
predecessor (Pos Zero) = Neg (S Zero)

-- The negator function turns positive numbers negative.
negator :: AbstractInteger -> AbstractInteger
negator (Pos x) = Neg x

-- The absolute value function makes all values positive.
absolute :: AbstractInteger -> AbstractInteger
absolute (Pos x) = Pos x
absolute (Neg x) = Pos x

-- The addition function performs addition on two integers.
add :: AbstractInteger -> AbstractInteger -> AbstractInteger
add (Pos x) (Pos y) = Pos (addN x y)
add (Neg x) (Neg y) = Neg (addN x y)
add (Pos x) (Neg y) 
   | x >= y     = Pos (diffN x y)
   | x <  y     = Neg (diffN y x)
add (Neg x) (Pos y) 
   | x >= y     = Neg (diffN x y)
   | x <  y     = Pos (diffN y x)

-- The difference function subtracts one integer from another.
difference :: AbstractInteger -> AbstractInteger -> AbstractInteger
difference (Pos x) (Pos y)
   | x >= y     = Pos (diffN x y)
   | x <  y     = Neg (diffN y x)
difference (Neg x) (Neg y)
   | x >= y     = Neg (diffN x y)
   | x <  y     = Pos (diffN y x)
difference (Pos x) (Neg y) = Pos (addN x y)
difference (Neg x) (Pos y) = Neg (addN x y)

-- The multiply function multiplys two integers.
multiply :: AbstractInteger -> AbstractInteger -> AbstractInteger
multiply (Pos x) (Pos y) = Pos (multN x y x)
multiply (Pos x) (Neg y) = Neg (multN x y x)
multiply (Neg x) (Pos y) = Neg (multN x y x)
multiply (Neg x) (Neg y) = Pos (multN x y x)

-- The equality comparisons for AbstractInteger. 
instance Eq AbstractInteger where
    Pos Zero == Neg Zero = True
    Neg Zero == Pos Zero = True
    Pos Zero == Pos Zero = True
    Neg Zero == Neg Zero = True

    Pos _ == Neg _ = False
    Neg _ == Pos _ = False

    Pos x == Pos y = x == y
    Neg x == Neg y = x == y

instance Ord AbstractInteger where
   Pos Zero <= Pos Zero = True
   Neg Zero <= Pos Zero = True
   Pos Zero <= Neg Zero = True
   Neg Zero <= Neg Zero = True

   Neg x <= Pos y = True
   Pos x <= Neg y = False
   Pos x <= Pos y = x <= y
   Neg x <= Neg y = y <= x

-- The division function divides one integer by another. 
divide :: AbstractInteger -> AbstractInteger -> AbstractInteger
divide (Pos x) (Pos y) = Pos (fst (divModN x y))
divide (Neg x) (Neg y) 
    | Pos (snd (divModN x y)) == (Pos Zero)   = Pos (fst (divModN x y)) 
    | x > y            = Pos (addN (fst (divModN x y)) (S Zero))
    | x < y            = Pos (addN (fst (divModN x y)) (S Zero))
    | x == y           = Pos (fst (divModN x y))   
divide (Neg x) (Pos y)
    | Pos (snd (divModN x y)) == (Pos Zero)   = Neg (fst (divModN x y))
    | x > y            = Neg (addN (fst (divModN x y)) (S Zero))
    | x <= y           = Neg (fst (divModN x y))
divide (Pos x) (Neg y) = Neg (fst (divModN x y))

-- The modulo function finds the positive remainder when one integer divides another. 
modulo :: AbstractInteger -> AbstractInteger -> AbstractInteger
modulo (Pos x) (Pos y) = Pos (snd (divModN x y)) 
modulo (Neg x) (Neg y) = 
    if Pos (snd (divModN x y)) == (Pos Zero)
    then Pos (snd (divModN x y))
    else Pos (diffN y (snd (divModN x y)))
modulo (Neg x) (Pos y) =
    if Pos (snd (divModN x y)) == (Pos Zero)
    then Pos (snd (divModN x y))
    else Pos (diffN y (snd (divModN x y)))
modulo (Pos x) (Neg y) = Pos (snd (divModN x y)) 

-- This function converts real integers to AbstractInteger.
toAbstract :: Integer -> AbstractInteger
toAbstract 0 = Pos Zero
toAbstract x
    | x > 0  = successor $ toAbstract (x - 1)
    | x < 0  = predecessor $ toAbstract (x + 1)

-- This function converts numbers from AbstractInteger to normal integers.
fromAbstract :: AbstractInteger -> Integer
fromAbstract (Pos Zero) = 0
fromAbstract (Neg Zero) = 0
fromAbstract (Pos (S x)) = (1 + (fromAbstract (Pos x)))
fromAbstract (Neg (S x)) = - (1 - (fromAbstract (Neg x)))

-- Take a list of strings, calculate, and return a string result.
evaluateRPN :: [String] -> AbstractInteger
evaluateRPN inputList = evalRPNStack [] inputList

-- The core of the RPN caluculator, Stack -> InputList -> Output
evalRPNStack :: [AbstractInteger] -> [String] -> AbstractInteger
evalRPNStack stack inputList =
    case (stack, inputList) of
        ( x:_,           [] )            -> x -- No more input, return top of stack.
        ( y:x:stackRest, "+":inputRest ) -> evalRPNStack (add x y        : stackRest) inputRest
        ( y:x:stackRest, "*":inputRest ) -> evalRPNStack (multiply x y   : stackRest) inputRest
        ( y:x:stackRest, "-":inputRest ) -> evalRPNStack (difference x y   : stackRest) inputRest
        ( y:x:stackRest, "/":inputRest ) -> evalRPNStack (divide x y   : stackRest) inputRest
        ( y:x:stackRest, "%":inputRest ) -> evalRPNStack (modulo x y   : stackRest) inputRest
        ( x:stackRest, "abs":inputRest ) -> evalRPNStack (absolute x   : stackRest) inputRest
        -- This last case handles numeric inputs, "0" "-2" "34" etc...
        ( _,          numStr:inputRest ) -> evalRPNStack (toAbstract (read numStr) : stack) inputRest

-- Convenience constructors. Handy for testing in ghci.
zero  = Pos Zero 

one   = successor zero
two   = successor one
three = successor two
four  = successor three
five  = successor four
six   = successor five
seven = successor six
eight = successor seven
nine  = successor eight
ten   = successor nine

negativeZero = Neg Zero 
negativeOne   = predecessor zero
negativeTwo   = predecessor negativeOne
negativeThree = predecessor negativeTwo
negativeFour  = predecessor negativeThree
negativeFive  = predecessor negativeFour
negativeSix   = predecessor negativeFive
negativeSeven = predecessor negativeSix
negativeEight = predecessor negativeSeven
negativeNine  = predecessor negativeEight
negativeTen   = predecessor negativeNine
