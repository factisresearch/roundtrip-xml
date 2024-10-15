{-# LANGUAGE OverloadedStrings #-}
module Text.Roundtrip.Xml.Printer (

    XmlPrinter, runXmlPrinter,

    runXmlPrinterByteString, runXmlPrinterLazyByteString,
    runXmlPrinterText, runXmlPrinterLazyText,
    runXmlPrinterString

) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Exception (SomeException)

import Control.Monad.Primitive

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Text.XML.Stream.Render as CXR

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char (ord)
import Data.XML.Types

import Control.Isomorphism.Partial
import Text.Roundtrip
import Text.Roundtrip.Printer
import Numeric (showHex)

data PxEvent = XmlTypesEvent Event
             | PxBeginElement Name
             | PxAttribute Name T.Text
               deriving (Show)

-- FIXME: don't use lists for  collecting the events, this makes an inefficient
-- monoid. Better use Data.Sequence?!

newtype XmlPrinter a = XmlPrinter { _unXmlPrinter :: Printer Identity [PxEvent] a }

instance IsoFunctor XmlPrinter where
    iso <$> (XmlPrinter p) = XmlPrinter $ iso `printerApply` p

instance ProductFunctor XmlPrinter where
    XmlPrinter p <*> XmlPrinter q = XmlPrinter (p `printerConcat` q)

instance Alternative XmlPrinter where
    XmlPrinter p <||> XmlPrinter q = XmlPrinter (p `printerAlternative` q)
    empty = XmlPrinter printerEmpty

instance Syntax XmlPrinter where
    pure x = XmlPrinter (printerPure x)

-- Rendering a list of events into a string/text/bytestring is done via
-- enumerators. This is not optimal because the resulting list is too strict.
-- However, currently no other functions exists for such a conversion.

runXmlPrinterGen :: (PrimMonad m) => XmlPrinter a -> a
                 -> (m (Either SomeException [c]) -> Either SomeException [c])
                 -> (CXR.RenderSettings -> C.ConduitM Event c (ExceptT SomeException m) ()) -> Maybe [c]
runXmlPrinterGen p x run render =
    case runXmlPrinter p x of
      Nothing -> Nothing
      Just l ->
          case
            run $ runExceptT $ C.runConduit $ CL.sourceList l C..| render CXR.def C..| CL.consume
          of
            Left _ -> Nothing
            Right t -> Just t

runXmlPrinterByteString :: XmlPrinter a -> a -> Maybe BS.ByteString
runXmlPrinterByteString p x =
    do l <- runXmlPrinterGen p x unsafePerformIO CXR.renderBytes
       return $ BS.concat l

runXmlPrinterLazyByteString :: XmlPrinter a -> a -> Maybe BSL.ByteString
runXmlPrinterLazyByteString p x =
    do l <- runXmlPrinterGen p x unsafePerformIO CXR.renderBytes
       return $ BSL.fromChunks l

runXmlPrinterText :: XmlPrinter a -> a -> Maybe T.Text
runXmlPrinterText p x =
    do l <- runXmlPrinterGen p x unsafePerformIO CXR.renderText
       return $ T.concat l

runXmlPrinterLazyText :: XmlPrinter a -> a -> Maybe TL.Text
runXmlPrinterLazyText p x =
    do l <- runXmlPrinterGen p x unsafePerformIO CXR.renderText
       return $ TL.fromChunks l

runXmlPrinterString :: XmlPrinter a -> a -> Maybe String
runXmlPrinterString p x =
    do tl <- runXmlPrinterLazyText p x
       case TL.unpack tl of
         ('<':'?':'x':'m':'l':z) -> Just (eat z)
         str -> Just str
    where
      eat l =
          case dropWhile (/= '?') l of
            '>':xs -> xs
            [] -> []
            _:xs -> eat xs

runXmlPrinter :: XmlPrinter a -> a -> Maybe [Event]
runXmlPrinter (XmlPrinter (Printer p)) x =
    fmap convEvents $ runIdentity (p x)
    where
      convEvents :: [PxEvent] -> [Event]
      convEvents pxes =
          case pxes of
            [] -> []
            XmlTypesEvent ev : rest -> ev : convEvents rest
            PxBeginElement name : rest ->
                let (attrs, nonAttrs) = convAttrs rest
                in EventBeginElement name attrs : convEvents nonAttrs
            attr@(PxAttribute _ _) : _ -> error $ "unexpected " ++ show attr
      convAttrs :: [PxEvent] -> ([(Name, [Content])], [PxEvent])
      convAttrs pxes =
          case pxes of
            PxAttribute name t : rest ->
                let (attrs, nonAttrs) = convAttrs rest
                in ((name, [ContentText t]) : attrs, nonAttrs)
            _ -> ([], pxes)

instance XmlSyntax XmlPrinter where
    xmlBeginDoc = xmlPrinterBeginDoc
    xmlEndDoc = xmlPrinterEndDoc
    xmlBeginElem = xmlPrinterBeginElem
    xmlEndElem = xmlPrinterEndElem
    xmlAttrValue = xmlPrinterAttrValue
    xmlTextNotEmpty = xmlPrinterTextNotEmpty

mkXmlPrinter :: (a -> [PxEvent]) -> XmlPrinter a
mkXmlPrinter f = XmlPrinter $ Printer $ \x -> return (Just (f x))

xmlPrinterBeginDoc :: XmlPrinter ()
xmlPrinterBeginDoc = mkXmlPrinter $ \() -> [XmlTypesEvent EventBeginDocument]

xmlPrinterEndDoc :: XmlPrinter ()
xmlPrinterEndDoc = mkXmlPrinter $ \() -> [XmlTypesEvent EventEndDocument]

xmlPrinterBeginElem :: Name -> XmlPrinter ()
xmlPrinterBeginElem name = mkXmlPrinter $ \() -> [PxBeginElement name]

xmlPrinterEndElem :: Name -> XmlPrinter ()
xmlPrinterEndElem name = mkXmlPrinter $ \() -> [XmlTypesEvent (EventEndElement name)]

xmlPrinterAttrValue :: Name -> XmlPrinter T.Text
xmlPrinterAttrValue aName = mkXmlPrinter $ \value -> [PxAttribute aName value]

-- xml-conduit fixed the handling of trailing newlines in version 1.9.1.2 by turning
-- \r\n and \r into \n. See https://www.w3.org/TR/REC-xml/#sec-line-ends
-- Thus, we represent certain characters as entities.
xmlPrinterTextNotEmpty :: XmlPrinter T.Text
xmlPrinterTextNotEmpty =
    mkXmlPrinter $ \value ->
        map (XmlTypesEvent . EventContent) (splitContent value)

splitContent :: T.Text -> [Content]
splitContent t =
    if T.null t
        then []
        else let (pref, suf) = T.span needsNoEntity t
             in if T.null pref
                    then handleLeadingEntity suf
                    else ContentText pref : handleLeadingEntity suf
    where
        entities = ['\t', '\n', '\r']
        needsNoEntity c = c `notElem` entities
        handleLeadingEntity t =
            if T.null t
                then []
                else encodeEntity (T.head t) : splitContent (T.tail t)
        encodeEntity c = ContentEntity (T.pack ('#' : 'x' : showHex (ord c) ""))
