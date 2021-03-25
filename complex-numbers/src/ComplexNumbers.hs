module ComplexNumbers
(Complex,
 conjugate,
 abs,
 exp,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding ( div, abs, exp )
import qualified Prelude

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (x, y) = Complex x y

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex x y) = Complex x (-y) 

abs :: Floating a => Complex a -> a
abs (Complex x y) = sqrt ( x^2 + y^2 )

real :: Num a => Complex a -> a
real (Complex x y) = x

imaginary :: Num a => Complex a -> a
imaginary (Complex x y) = y

exp :: Floating a => Complex a -> Complex a
exp (Complex x y) = Complex (Prelude.exp x * cos y) (Prelude.exp x * sin y)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) = Complex (a * c - b * d) (b * c + a * d)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) = Complex (a + c) (b + d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a b) (Complex c d) = Complex (a - c) (b - d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) = Complex ((a * c + b * d)/(c^2 + d^2)) ((b * c - a * d)/(c^2 + d^2))
