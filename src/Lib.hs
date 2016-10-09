{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( Issue(..)
    , PackageName(..)
    , VersionSpecifier(..)
    , Version(..)
    -- * API
    , readDb
    , queryDb
    ) where

import           Data.Yaml
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar
import           Data.Version
import           Text.ParserCombinators.ReadP
import           Distribution.Version
import qualified Distribution.Text         as C (parse)
import qualified Distribution.Compat.ReadP as C (readP_to_S)

-- useful ?
-- data Severity = Critical | Major | Minor

newtype PackageName = PackageName String
    deriving (Show, Eq, FromJSON, ToJSON)

data VersionSpecifier =
      VersionOne Version
    | VersionRange (Maybe Version) (Maybe Version)
    deriving (Show,Eq)

data Issue = Issue
    { issuePackage      :: PackageName
    , issueVersions     :: [VersionRange]
    , issueReason       :: String
    , issueUrl          :: Maybe [String]
    , issueReportedDate :: Day
    }
    deriving (Show,Eq)
instance FromJSON VersionRange where
    parseJSON (String t) =
        case C.readP_to_S C.parse $ T.unpack t of
            []          -> case C.readP_to_S C.parse $ ("== " ++) $ T.unpack t of
                                 []          -> fail ("version range parsing failed empty: " ++ show t)
                                 l           -> return $ fst $ last l
            l           -> return $ fst $ last l


instance FromJSON VersionSpecifier where
    parseJSON (String t) =
        case readP_to_S parseVersion $ T.unpack t of
            []          -> fail ("version parsing failed empty: " ++ show t)
            [(ver, "")] -> return $ VersionOne ver
            l           -> return $ VersionOne $ fst $ last l

data Db = Db [Issue]
    deriving (Show,Eq)

instance FromJSON Issue where
    parseJSON (Object v) = Issue
        <$> v .: "package"
        <*> v .: "versions"
        <*> v .: "reason"
        <*> v .:? "urls"
        <*> v .: "date"

instance FromJSON Db where
    parseJSON v = Db <$> parseJSON v

-- | Try to read the whole list of issues defined
readDb :: IO (Either ParseException Db)
readDb = decodeFileEither "issues.yaml"

-- | Get all issues matching the package version

queryDb db pkgs = undefined
