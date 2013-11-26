-- | Hahet runtime.
module Hahet.Runtime where

import Hahet.Internal

-- * Apply
---------------------------------------------------------

-- | Execute an @Apply@ computation given some RTConf.
runApply :: RTConf c -> Apply c a -> IO a
runApply app f = shelly $ runReaderT (unApply f) app

-- | Apply a configuration on the running system.
apply :: (Typeable c) => Configuration c -> IO (Results c)
apply conf = do
    let logHandle = IO.stdout     -- XXX: duplicate to a file?
    logger <- mkLogger True logHandle
    let rtconf = RTConf conf True logger
        in runApply rtconf apply' -- XXX: get application and discrete rtconf instead?

apply' :: (Typeable c) => Apply c (Results c)
apply' = do
    app <- view rtConfiguration

    $status   [qc|Applying configuration { show $ typeOf (app ^. aconf) }|]
    $logDebug [qc|Configuration:
{ prettyPrintTargets (app ^. atargets) }|]

    results <- mapM (Tr.mapM handleTarget) (app ^. atargets)

    $status [qc|Configuration applied.|]

    return results
    where
        handleTarget (Left  mident) =
            $status [qc|Apply module { mident }|] >> return (mident, ResNoop)

        handleTarget (Right (tident, MkTarget t)) =
            $logDebug [qc|Applying target {typeOf t}|] >> targetApply t >>= return . (,) tident
