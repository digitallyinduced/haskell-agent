-- | Pure validation of delivery destinations and Git references.
module Agent.CLI.RepositoryDelivery.Validation
    ( validRemoteUrl
    , githubRepositoryFromUrl
    , validateBranchName
    , validateFullBranchRef
    , validateRemoteName
    , validateRepositoryName
    , validObjectId
    , zeroObjectId
    ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isAlphaNum, isHexDigit, isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

validRemoteUrl :: BS.ByteString -> Bool
validRemoteUrl url =
    not (BS.null url)
        && BS.length url <= 4096
        && BS.all (\byte -> byte >= 0x20 && byte /= 0x7f) url
        && BS8.head url /= '-'
        && (validHttps url
            || validSsh url
            || validScpLike url)
  where
    validHttps value =
        "https://" `BS8.isPrefixOf` value
            && validUrlAuthority "https://" value
            && not (authorityContains '@' "https://" value)
            && not (containsQueryOrFragment value)
            && not (authorityContains '%' "https://" value)
    validSsh value =
        "ssh://" `BS8.isPrefixOf` value
            && validSshAuthority value
            && not (containsQueryOrFragment value)
    authorityContains character prefix value =
        BS8.elem character
            (BS8.takeWhile (/= '/') (BS.drop (BS.length prefix) value))
    validSshAuthority value =
        let authority =
                BS8.takeWhile (/= '/') (BS.drop (BS.length "ssh://") value)
            (userinfo, separatorAndHost) = BS8.break (== '@') authority
            host = BS.drop 1 separatorAndHost
        in not (BS.null authority)
            && not (BS8.elem '%' authority)
            && if BS.null separatorAndHost
                then validHostPort authority
                else validUsername userinfo
                    && validHostPort host
                    && not (BS8.elem '@' host)
    validUrlAuthority prefix value =
        validHostPort
            (BS8.takeWhile (/= '/') (BS.drop (BS.length prefix) value))
    validHostPort authority =
        case BS8.break (== ':') authority of
            (host, port)
                | BS.null port -> validHost host
                | otherwise ->
                    validHost host
                        && BS.length port > 1
                        && BS8.all
                            (\character ->
                                character >= '0' && character <= '9')
                            (BS.drop 1 port)
    validHost host =
        not (BS.null host)
            && BS8.all
                (\character ->
                    isAsciiAlphaNumeric character
                        || character `elem` (".-" :: String))
                host
            && BS8.head host /= '.'
            && BS8.last host /= '.'
    validUsername username =
        not (BS.null username)
            && BS8.all
                (\character ->
                    isAsciiAlphaNumeric character
                        || character `elem` ("._-" :: String))
                username
    isAsciiAlphaNumeric character =
        (character >= 'a' && character <= 'z')
            || (character >= 'A' && character <= 'Z')
            || (character >= '0' && character <= '9')
    containsQueryOrFragment value =
        BS8.elem '?' value || BS8.elem '#' value
    validScpLike value =
        case BS8.break (== ':') value of
            (authority, path) ->
                let (username, separatorAndHost) = BS8.break (== '@') authority
                    host = BS.drop 1 separatorAndHost
                in validUsername username
                    && not (BS.null separatorAndHost)
                    && validHost host
                    && not (BS8.elem '@' host)
                    && not (BS8.elem '%' authority)
                    && BS.length path > 1
                    && not (BS8.elem '@' path)
                    && not (containsQueryOrFragment value)

githubRepositoryFromUrl :: BS.ByteString -> Maybe Text
githubRepositoryFromUrl bytes
    | not (BS.all (< 0x80) bytes) = Nothing
    | otherwise =
        let url = Text.pack (BS8.unpack bytes)
        in parseHttps url
            <|> parseSsh url
            <|> parseScp url
  where
    parseHttps url =
        Text.stripPrefix "https://github.com/" url >>= repositoryPath
    parseSsh url =
        (Text.stripPrefix "ssh://git@github.com/" url
            <|> Text.stripPrefix "ssh://github.com/" url)
            >>= repositoryPath
    parseScp url =
        Text.stripPrefix "git@github.com:" url >>= repositoryPath
    repositoryPath path =
        let withoutGit = fromMaybe path (Text.stripSuffix ".git" path)
        in if validateRepositoryName withoutGit
            then Just withoutGit
            else Nothing

validateBranchName :: Text -> Bool
validateBranchName branch =
    validateFullBranchRef ("refs/heads/" <> branch)

validateFullBranchRef :: Text -> Bool
validateFullBranchRef ref =
    not (Text.null ref)
        && Text.length ref <= 1024
        && "refs/heads/" `Text.isPrefixOf` ref
        && not ("/" `Text.isSuffixOf` ref)
        && not ("." `Text.isSuffixOf` ref)
        && not ("." `Text.isPrefixOf` ref)
        && not ("-" `Text.isPrefixOf` Text.drop (Text.length "refs/heads/") ref)
        && not (".." `Text.isInfixOf` ref)
        && not ("@{" `Text.isInfixOf` ref)
        && not ("//" `Text.isInfixOf` ref)
        && Text.all safeRefCharacter ref
        && all validComponent (Text.splitOn "/" ref)
  where
    safeRefCharacter character =
        not (isSpace character)
            && character >= '\x20'
            && character /= '\x7f'
            && character `notElem` ("~^:?*[\\" :: String)
    validComponent component =
        not (Text.null component)
            && not ("." `Text.isPrefixOf` component)
            && component /= "."
            && component /= ".."
            && not (".lock" `Text.isSuffixOf` component)

validateRemoteName :: Text -> Bool
validateRemoteName remote =
    not (Text.null remote)
        && Text.length remote <= 255
        && Text.head remote /= '-'
        && Text.all
            (\character ->
                isAlphaNum character
                    || character `elem` ("._/-" :: String))
            remote
        && not (".." `Text.isInfixOf` remote)
        && not ("//" `Text.isInfixOf` remote)

validateRepositoryName :: Text -> Bool
validateRepositoryName name =
    case Text.splitOn "/" name of
        [owner, repository] ->
            validPart owner && validPart repository
        _ -> False
  where
    validPart value =
        not (Text.null value)
            && value /= "."
            && value /= ".."
            && Text.length value <= 100
            && Text.all
                (\character ->
                    isAlphaNum character
                        || character `elem` ("-._" :: String))
                value

validObjectId :: Text -> Bool
validObjectId oid =
    Text.length oid `elem` [40, 64] && Text.all isHexDigit oid

zeroObjectId :: Text -> Text
zeroObjectId oid = Text.replicate (Text.length oid) "0"
