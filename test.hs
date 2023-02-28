

CalcInit::CalcStack
CalcSolve::[CalcInstr]->CalcStack->CalcStack
CalcTop::CalcStack->Double

--Tipus CalcStack:
type CalcStack = [Double]
-- Tipus CalcInstr
data CalcInstr = CalcEnter Double | CalcAdd | CalcSub | CalcMatt | CalcDiv | CalcNeg | CalcInv 

module Main where
import Calc
main::IO()
main = do
