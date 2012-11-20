import qualified Copilot.Tools.CBMC  as CBMC
import qualified Copilot.Compile.C99 as C99
import           Language.Copilot    hiding (even, odd)
import qualified Prelude             as P

nats :: Stream Int32
nats = [0] ++ (1 + nats)

even :: (P.Integral a, Typed a) => Stream a -> Stream Bool
even x = x `mod` 2 == 0

odd :: (P.Integral a, Typed a) => Stream a -> Stream Bool
odd = not . even

spec :: Spec
spec = do
  trigger "trigger1" (even nats) []
  trigger "trigger2" (odd nats)  []

main :: IO ()
main = do
  putStrLn "PrettyPrinter:\n\n"
  prettyPrint spec
  putStrLn "\nInterpreter:\n\n"
  interpret 100 spec
  putStrLn "\n"
  reify spec >>= \s -> do
    C99.compile  C99.defaultParams  s
    CBMC.genCBMC CBMC.defaultParams s
