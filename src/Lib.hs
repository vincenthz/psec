{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Issue(..)
    , PackageName(..)
    , Version(..)
    -- * API
    , readDb
    , readDbBS
    , queryDb
    , readPackageIdentifier
    , getProjectDependencies
    , getAllIssues
    ) where

import           Data.Yaml
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar
import           Data.Version
import qualified Data.ByteString.Char8 as S8
import           Text.ParserCombinators.ReadP
import           Distribution.Version(VersionRange(..), withinRange)
import           Distribution.Package (PackageName(..),PackageIdentifier(..))
import qualified Distribution.Text         as C (parse)
import qualified Distribution.Compat.ReadP as C (readP_to_S)
import           System.Process

-- useful ?
-- data Severity = Critical | Major | Minor

deriving instance FromJSON PackageName
deriving instance ToJSON PackageName


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

readPackageIdentifier :: String -> Maybe PackageIdentifier
readPackageIdentifier v =
   case C.readP_to_S C.parse $ v of
     []          -> fail ("version range parsing failed empty: " ++ show v)
     l           -> return $ fst $ last l

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


-- $setup
-- >>> import qualified Data.ByteString.Char8 as S8

-- | Read Db from Yaml
--
-- Example:
--
-- >>> readDbBS $ S8.pack "- package: foo\n  versions:\n  - 1.0.0\n  - 1.0.1\n  - 0.9.0\n  reason: Remote execution hole\n  urls:\n  - https://github.com/foo/foo/issues/51\n  date: 2016-06-09\n\n- package: bar\n  versions:\n  - 1.0.1\n  - 1.0.1\n  - 0.9.0\n  reason: Local execution hole\n  urls:\n  - https://github.com/bar/bar/issues/1\n  date: 2016-09-09\n"
-- Right (Db [Issue {issuePackage = PackageName {unPackageName = "foo"}, issueVersions = [ThisVersion (Version {versionBranch = [1,0,0], versionTags = []}),ThisVersion (Version {versionBranch = [1,0,1], versionTags = []}),ThisVersion (Version {versionBranch = [0,9,0], versionTags = []})], issueReason = "Remote execution hole", issueUrl = Just ["https://github.com/foo/foo/issues/51"], issueReportedDate = 2016-06-09},Issue {issuePackage = PackageName {unPackageName = "bar"}, issueVersions = [ThisVersion (Version {versionBranch = [1,0,1], versionTags = []}),ThisVersion (Version {versionBranch = [1,0,1], versionTags = []}),ThisVersion (Version {versionBranch = [0,9,0], versionTags = []})], issueReason = "Local execution hole", issueUrl = Just ["https://github.com/bar/bar/issues/1"], issueReportedDate = 2016-09-09}])
readDbBS :: S8.ByteString -> Either ParseException Db
readDbBS = decodeEither'

-- | Try to read the whole list of issues defined
readDb :: IO (Either ParseException Db)
readDb = decodeFileEither "issues.yaml"

-- | Get all issues matching the package version
queryDb :: Db -> PackageIdentifier -> [Issue]
queryDb (Db db) (PackageIdentifier pkgName pkgVersion) =
   filter (\Issue {..} -> issuePackage == pkgName && any (withinRange pkgVersion) issueVersions) db

-- | Use Stack to get a list of all dependencies for a local project
getProjectDependencies :: IO [PackageIdentifier]
getProjectDependencies =
  catMaybes . map readPackageIdentifier . lines . (\(_,r,_)->r) <$>
    readProcessWithExitCode "stack" ["list-dependencies", "--separator=-"] ""

getAllIssues :: Db -> [PackageIdentifier] -> [Issue]
getAllIssues db = concatMap (queryDb db)
