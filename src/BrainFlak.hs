module BrainFlak (runBrainFlak) where

import Data.List

data State = State {
 steps :: Integer,
 left  :: [Integer],
 right :: [Integer],
 scope :: [Integer]}

accumulate :: Integer -> State -> State
accumulate value st@(State _ _ _ (top : rem)) = st { scope = value + top : rem }

takeStep :: State -> State
takeStep st = st { steps = steps st - 1 } 

count :: (Eq a, Num n) => a -> [a] -> n
count = (genericLength .) . (filter . (==))

insideLoop :: String -> String
insideLoop = last . takeWhile (\x -> count '{' x /= count '}' x - 1) . inits

afterLoop :: String -> String
afterLoop = head . dropWhile (\x -> count '{' x /= count '}' x) . tails

runBrainFlak :: String -> Integer -> Maybe Integer
runBrainFlak source maxSteps = fmap (head . scope) $ stepBrainFlak source $ State { left = [], right = [], scope = [0], steps = maxSteps }

stepBrainFlak :: String -> State -> Maybe State
stepBrainFlak "" endState = Just endState
stepBrainFlak _ currState
 | steps currState == 0 = Nothing
stepBrainFlak ('<' : '>' : rest) currState = stepBrainFlak rest $ takeStep $ currState { left = right currState, right = left currState }
stepBrainFlak ('(' : ')' : rest) currState = stepBrainFlak rest $ takeStep $ accumulate 1 $ currState
stepBrainFlak ('[' : ']' : rest) currState = stepBrainFlak rest $ takeStep $ accumulate (genericLength $ left currState) $ currState
stepBrainFlak ('{' : '}' : rest) currState
 | [] <- left currState = stepBrainFlak rest $ takeStep $ currState
 | (tos : left) <- left currState = stepBrainFlak rest $ takeStep $ accumulate tos $ currState {left = left}
stepBrainFlak ('<' : rest) currState = stepBrainFlak rest $ takeStep $ currState { scope = 0 : scope currState }
stepBrainFlak ('(' : rest) currState = stepBrainFlak rest $ takeStep $ currState { scope = 0 : scope currState }
stepBrainFlak ('[' : rest) currState = stepBrainFlak rest $ takeStep $ currState { scope = 0 : scope currState }
stepBrainFlak ('>' : rest) currState = stepBrainFlak rest $ takeStep $ currState { scope = tail $ scope currState }
stepBrainFlak (']' : rest) currState
 | (sHead : sTail) <- scope currState = stepBrainFlak rest $ takeStep $ accumulate (-sHead) $ currState { scope = tail $ scope currState }
stepBrainFlak (')' : rest) currState
 | (sHead : sTail) <- scope currState = stepBrainFlak rest $ takeStep $ accumulate sHead $ currState { left = sHead : left currState, scope = tail $ scope currState }
stepBrainFlak ('{' : rest) currState =
 nextLoop >>= case fmap left nextLoop of
  Just (0 : _) -> stepBrainFlak (afterLoop rest)
  Just []      -> stepBrainFlak (afterLoop rest)
  _            -> stepBrainFlak ('{' : rest)
 where nextLoop = stepBrainFlak (insideLoop rest) currState


