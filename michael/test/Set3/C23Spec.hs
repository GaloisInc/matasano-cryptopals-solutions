module Set3.C23Spec where


import Control.Monad
import Test.Hspec
import Test.QuickCheck as QC

import Set3.C21
import Set3.C23

spec :: Spec
spec = do
  describe "untemper" $ do
    it "satisfies identity property with temper" $ do
      property prop_untemper_temper_id

  forM_ [ ("t0 / u0", t0, u0)
        , ("t1 / u1", t1, u1)
        , ("t2 / u2", t2, u2)
        , ("t3 / u3", t3, u3) ]

        $ \(name, tF, uF) -> do
          describe name $ do
            it "satisfies identity property" $ do
              property (prop_t_u_inverse tF uF)


prop_untemper_temper_id :: Word32 -> QC.Property
prop_untemper_temper_id x =
  QC.conjoin
    [ untemper (temper x) QC.=== x
    , temper (untemper x) QC.=== x
    ]

prop_t_u_inverse :: (Word32 -> Word32)
                -> (Word32 -> Word32)
                -> Word32 -> QC.Property
prop_t_u_inverse tF uF x =
  QC.conjoin
    [ uF (tF x) QC.=== x
    , tF (uF x) QC.=== x
    ]
