module UnsafeSTM where
import GHC.Conc
newtype UnsafeSTM a = STM a

unsafeRead :: TVar a -> UnsafeSTM a
unsafeRead a =  (readTVar a)

unsafeLiftSTM :: UnsafeSTM a -> STM a
unsafeLiftSTM a = return a

main =
 do
  a <- newTVarIO 1
  content <- unsafeRead a
  print (show (a))

