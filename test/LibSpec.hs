module LibSpec where



import           Test.Hspec
import           Lib

import qualified Data.ByteString.Char8 as S8


spec :: Spec
spec = do
   describe  "ok" $ do
     let Right db = readDbBS $ S8.pack dbS
     pdep <- runIO getProjectDependencies
     let pb =  getAllIssues db pdep
     it "should find a pb here" $
         show pb `shouldBe` pbPsec


dbS = "- package: foo\n  versions:\n  - 1.0.0\n  - 1.0.1\n  - 0.9.0\n  reason: Remote execution hole\n  urls:\n  - https://github.com/foo/foo/issues/51\n  date: 2016-06-09\n\n- package: Cabal\n  versions:\n  - <2\n  reason: Remote execution hole\n  urls:\n  - https://github.com/foo/foo/issues/51\n  date: 2016-06-19\n\n- package: bar\n  versions:\n  - 1.0.1\n  - 1.0.1\n  - 0.9.0\n  reason: Local execution hole\n  urls:\n  - https://github.com/bar/bar/issues/1\n  date: 2016-09-09\n"

pbPsec = "[Issue {issuePackage = PackageName {unPackageName = \"Cabal\"}, issueVersions = [EarlierVersion (Version {versionBranch = [2], versionTags = []})], issueReason = \"Remote execution hole\", issueUrl = Just [\"https://github.com/foo/foo/issues/51\"], issueReportedDate = 2016-06-19}]"
