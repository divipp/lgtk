-- | https://github.com/eugenkiss/7guis/wiki#the-seven-tasks

module Temperature where

import Control.Lens
import Control.Lens.Extras (is)
import Data.LensRef
import LGtk
import Numeric.Lens

main :: IO ()
main = runWidget $ do
    x <- newRef 0
    hcat [ entryShow x
         , label (return "Celsius = ")
         , entryShow (celsiusToFahrenheit `lensMap` x)
         , label (return "Fahrenheit")
         ]

celsiusToFahrenheit :: Iso' Double Double
celsiusToFahrenheit = multiplying 1.8 . adding 32

-- Note: If we really wanted to be properly typesafe, we'd use newtypes:
-- celsiusToFahrenheit =
--     _Wrapping Celsius . multiplying 1.8 . adding 32 . _Unwrapping Fahrenheit

{-
entryCanFail :: forall m a r . (MonadRegister m, RefClass r, RefReaderSimple r ~ RefReader m) => Prism' String a -> RefSimple r a -> Widget m
entryCanFail prism r = return $ Entry (is prism) (getContent, changed)
  where
    getContent = review prism <$> readRef r
    changed x = forM_ (x ^? prism) (setRef r)

entryEither :: forall m a r . (MonadRegister m, RefClass r, RefReaderSimple r ~ RefReader m) => Prism' String a -> RefSimple r (Either String a) -> Widget m
entryEither prism r = return $ Entry (const True) (getContent, changed)
  where
    getContent = either id (review prism) <$> readRef r
    changed x = setRef r (maybe (Left x) Right (x ^? prism))

-- | Text entry with automatic show-read conversion.
entryShow :: forall m a r . (MonadRegister m, Show a, Read a, RefClass r, RefReaderSimple r ~ RefReader m) => RefSimple r a -> Widget m
entryShow = entryCanFail _Show
-}
