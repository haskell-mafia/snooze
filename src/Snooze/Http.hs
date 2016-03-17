{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Snooze.Http (

  ) where

import           P

req :: Registry -> Service -> [Component] -> [Query] -> Request
req b url a = do
 .....
