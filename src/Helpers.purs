module Helpers where
import Prelude
import Effect (Effect)
import Data.String as String
import Web.HTML (window)
import Web.HTML.Location (hash)
import Web.HTML.Window (location)

getHash :: Effect String
getHash = window >>= location >>= hash <#> String.drop 1