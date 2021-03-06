{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- pulling this in to control the logging
import System.IO
import System.Log.Logger ( updateGlobalLogger
                         , rootLoggerName
                         , setLevel
                         , Priority(..)
                         )

main :: IO ()
--main = serve Nothing myApp
main = do
	updateGlobalLogger rootLoggerName (setLevel INFO)
	serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
	[ dir "echo" $ echo
	, dir "query" $ queryParams
	, homePage
	]

-- example of BlazeHTML
template  :: Text -> Html -> Response
template title body = toResponse $
	H.html $ do
		H.head $ do
			H.title (toHtml title)
		H.body $ do
			body
			p $ a ! href "/" $ "back home"

homePage :: ServerPart Response
homePage =
	ok $ template "home page" $ do
		H.h1 "Hello, web"
		H.p "Apps:"
		H.p $ a ! href "/echo/this%20is%20a%20test" $ "echo"
		H.p $ a ! href "/query?foo=bar" $ "query parameters"

echo :: ServerPart Response
echo = path $
	\(msg ::String) ->
		ok $ template "echo" $ do
			p $ "echoing: " >> toHtml msg
			p $ "length" >> toHtml (length msg)

queryParams :: ServerPart Response
queryParams = ok $ template "under construction" $ do p "Unimplemented"

