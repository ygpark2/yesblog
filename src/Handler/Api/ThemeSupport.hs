{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api.ThemeSupport where

import Import
import Handler.Api.Shared (apiError, nonEmptyText, normalizeOptionalTextarea)
import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time (UTCTime)
import Network.HTTP.Types.Status (status400)

data ThemeDraftInput = ThemeDraftInput
    { themeDraftName :: Text
    , themeDraftSlug :: Text
    , themeDraftDescription :: Text
    , themeDraftBackgroundColor :: Text
    , themeDraftSurfaceColor :: Text
    , themeDraftTextColor :: Text
    , themeDraftAccentColor :: Text
    , themeDraftHeadingFont :: Text
    , themeDraftBodyFont :: Text
    , themeDraftHeaderTemplate :: Text
    , themeDraftBodyTemplate :: Text
    , themeDraftFooterTemplate :: Text
    , themeDraftCustomCss :: Text
    , themeDraftPriceCents :: Int
    }

data ThemeAdminOptions = ThemeAdminOptions
    { themeAdminStatus :: Text
    , themeAdminLicense :: Maybe Text
    , themeAdminActive :: Bool
    }

readThemeDraftInput :: Handler ThemeDraftInput
readThemeDraftInput = do
    name <- T.strip <$> runInputPost (ireq textField "name")
    rawSlug <- T.strip <$> runInputPost (ireq textField "slug")
    rawDescription <- runInputPost $ fromMaybe "" <$> iopt textField "description"
    backgroundColor <- normalizeColorInput =<< runInputPost (ireq textField "backgroundColor")
    surfaceColor <- normalizeColorInput =<< runInputPost (ireq textField "surfaceColor")
    textColor <- normalizeColorInput =<< runInputPost (ireq textField "textColor")
    accentColor <- normalizeColorInput =<< runInputPost (ireq textField "accentColor")
    rawHeadingFont <- runInputPost $ fromMaybe "" <$> iopt textField "headingFont"
    rawBodyFont <- runInputPost $ fromMaybe "" <$> iopt textField "bodyFont"
    rawHeaderTemplate <- runInputPost $ fromMaybe "" <$> iopt textField "headerTemplate"
    rawBodyTemplate <- runInputPost $ fromMaybe "" <$> iopt textField "bodyTemplate"
    rawFooterTemplate <- runInputPost $ fromMaybe "" <$> iopt textField "footerTemplate"
    rawCustomCss <- runInputPost $ fromMaybe "" <$> iopt textField "customCss"
    rawPriceCents <- runInputPost $ fromMaybe "0" <$> iopt textField "priceCents"
    let slug = normalizeThemeSlug rawSlug
    when (name == "") $
        apiError status400 "Theme name is required."
    when (slug == "") $
        apiError status400 "Theme slug is required."
    validateThemeHtmlTemplate rawHeaderTemplate
    validateThemeHtmlTemplate rawBodyTemplate
    validateThemeHtmlTemplate rawFooterTemplate
    validateThemeCss rawCustomCss
    pure
        ThemeDraftInput
            { themeDraftName = name
            , themeDraftSlug = slug
            , themeDraftDescription = rawDescription
            , themeDraftBackgroundColor = backgroundColor
            , themeDraftSurfaceColor = surfaceColor
            , themeDraftTextColor = textColor
            , themeDraftAccentColor = accentColor
            , themeDraftHeadingFont = rawHeadingFont
            , themeDraftBodyFont = rawBodyFont
            , themeDraftHeaderTemplate = rawHeaderTemplate
            , themeDraftBodyTemplate = rawBodyTemplate
            , themeDraftFooterTemplate = rawFooterTemplate
            , themeDraftCustomCss = rawCustomCss
            , themeDraftPriceCents = parseNonNegativeInt rawPriceCents
            }

readThemeAdminOptions :: Handler ThemeAdminOptions
readThemeAdminOptions = do
    rawStatus <- runInputPost $ fromMaybe "published" <$> iopt textField "status"
    rawLicense <- runInputPost $ fromMaybe "" <$> iopt textField "license"
    rawActive <- runInputPost $ fromMaybe "true" <$> iopt textField "active"
    pure
        ThemeAdminOptions
            { themeAdminStatus = normalizeThemeStatus rawStatus
            , themeAdminLicense = nonEmptyText rawLicense
            , themeAdminActive = parseThemeActive rawActive
            }

buildUserTheme :: UserId -> Maybe ThemeId -> UTCTime -> ThemeDraftInput -> Theme
buildUserTheme userId mParentThemeId now draft =
    Theme
        { themeName = themeDraftName draft
        , themeSlug = themeDraftSlug draft
        , themeDescription = normalizeOptionalTextarea $ Just $ themeDraftDescription draft
        , themeAuthor = Just userId
        , themeParent = mParentThemeId
        , themeBackgroundColor = themeDraftBackgroundColor draft
        , themeSurfaceColor = themeDraftSurfaceColor draft
        , themeTextColor = themeDraftTextColor draft
        , themeAccentColor = themeDraftAccentColor draft
        , themeHeadingFont = nonEmptyText $ themeDraftHeadingFont draft
        , themeBodyFont = nonEmptyText $ themeDraftBodyFont draft
        , themeHeaderTemplate = normalizeOptionalTextarea $ Just $ themeDraftHeaderTemplate draft
        , themeBodyTemplate = normalizeOptionalTextarea $ Just $ themeDraftBodyTemplate draft
        , themeFooterTemplate = normalizeOptionalTextarea $ Just $ themeDraftFooterTemplate draft
        , themeCustomCss = normalizeOptionalTextarea $ Just $ themeDraftCustomCss draft
        , themePriceCents = themeDraftPriceCents draft
        , themeStatus = "review"
        , themeLicense = Just "marketplace-remix"
        , themeActive = True
        , themeCreatedAt = now
        , themeUpdatedAt = now
        }

buildAdminTheme :: Maybe Theme -> UTCTime -> ThemeDraftInput -> ThemeAdminOptions -> Theme
buildAdminTheme mExistingTheme now draft adminOptions =
    Theme
        { themeName = themeDraftName draft
        , themeSlug = themeDraftSlug draft
        , themeDescription = normalizeOptionalTextarea $ Just $ themeDraftDescription draft
        , themeAuthor = mExistingTheme >>= themeAuthor
        , themeParent = mExistingTheme >>= themeParent
        , themeBackgroundColor = themeDraftBackgroundColor draft
        , themeSurfaceColor = themeDraftSurfaceColor draft
        , themeTextColor = themeDraftTextColor draft
        , themeAccentColor = themeDraftAccentColor draft
        , themeHeadingFont = nonEmptyText $ themeDraftHeadingFont draft
        , themeBodyFont = nonEmptyText $ themeDraftBodyFont draft
        , themeHeaderTemplate = normalizeOptionalTextarea $ Just $ themeDraftHeaderTemplate draft
        , themeBodyTemplate = normalizeOptionalTextarea $ Just $ themeDraftBodyTemplate draft
        , themeFooterTemplate = normalizeOptionalTextarea $ Just $ themeDraftFooterTemplate draft
        , themeCustomCss = normalizeOptionalTextarea $ Just $ themeDraftCustomCss draft
        , themePriceCents = themeDraftPriceCents draft
        , themeStatus = themeAdminStatus adminOptions
        , themeLicense = themeAdminLicense adminOptions
        , themeActive = themeAdminActive adminOptions
        , themeCreatedAt = maybe now themeCreatedAt mExistingTheme
        , themeUpdatedAt = now
        }

normalizeColorInput :: Text -> Handler Text
normalizeColorInput rawValue = do
    let value = T.strip rawValue
    when (value == "") $
        apiError status400 "Theme colors are required."
    pure value

normalizeThemeSlug :: Text -> Text
normalizeThemeSlug source =
    T.intercalate "-" $
        filter (/= "") $
            map
                (T.filter (\char -> Char.isAlphaNum char || char == '-'))
                (T.words $ T.map (\char -> if Char.isAlphaNum char then char else ' ') $ T.toLower source)

parseThemeActive :: Text -> Bool
parseThemeActive rawValue =
    T.toLower (T.strip rawValue) `elem` ["true", "1", "on", "yes"]

normalizeThemeStatus :: Text -> Text
normalizeThemeStatus rawValue =
    let value = T.toLower $ T.strip rawValue
    in if value `elem` ["draft", "review", "published", "suspended"]
        then value
        else "review"

parseNonNegativeInt :: Text -> Int
parseNonNegativeInt rawValue =
    case TR.decimal (T.strip rawValue) of
        Right (value, _) -> max 0 value
        Left _ -> 0

validateThemeHtmlTemplate :: Text -> Handler ()
validateThemeHtmlTemplate rawTemplate = do
    let template = T.strip rawTemplate
    unless (template == "") $ do
        let lowered = T.toLower template
        when (containsAny lowered ["<script", "<iframe", "<object", "<embed", "<form", "<input", "<button", "<textarea", "<select", "<option", "<link", "<meta", "<base", "<img", "<svg", "<math"]) $
            apiError status400 "Theme templates may only use safe structural HTML."
        when ("style=" `T.isInfixOf` lowered || "src=" `T.isInfixOf` lowered) $
            apiError status400 "Theme templates cannot use inline styles or source attributes."
        when ("javascript:" `T.isInfixOf` lowered || "data:text/html" `T.isInfixOf` lowered) $
            apiError status400 "Theme templates cannot embed executable URLs."
        let tags = extractHtmlTags template
            invalidTags = filter (`Set.notMember` allowedThemeTags) tags
        unless (null invalidTags) $
            apiError status400 "Theme templates include unsupported HTML tags."
        when (any (hasUnsafeHref template) tags) $
            apiError status400 "Theme template links must be relative or http/https/mailto URLs."

validateThemeCss :: Text -> Handler ()
validateThemeCss rawCss = do
    let css = T.toLower $ T.strip rawCss
    unless (css == "") $
        when (containsAny css ["@import", "url(", "expression(", "javascript:", "-moz-binding", "behavior:", "</style", "<style"]) $
            apiError status400 "Theme CSS contains unsupported constructs."

allowedThemeTags :: Set.Set Text
allowedThemeTags =
    Set.fromList
        [ "a", "article", "blockquote", "code", "div", "em", "footer", "h1", "h2", "h3", "h4"
        , "header", "hr", "li", "main", "nav", "ol", "p", "pre", "section", "small", "span"
        , "strong", "time", "ul"
        ]

extractHtmlTags :: Text -> [Text]
extractHtmlTags template =
    mapMaybe extractTagName $ T.splitOn "<" template

extractTagName :: Text -> Maybe Text
extractTagName chunk =
    let stripped = T.strip chunk
    in if stripped == "" || T.isPrefixOf "!" stripped || T.isPrefixOf "?" stripped || T.isPrefixOf "/" stripped
        then if T.isPrefixOf "/" stripped
            then extractTagName (T.drop 1 stripped)
            else Nothing
        else
            let name = T.takeWhile (\char -> Char.isAlphaNum char || char == '-') stripped
            in nonEmptyText name

hasUnsafeHref :: Text -> Text -> Bool
hasUnsafeHref _ tagName
    | tagName /= "a" = False
hasUnsafeHref template _ =
    any isUnsafeHref $ mapMaybe extractHrefValue (T.splitOn "<a" template)

extractHrefValue :: Text -> Maybe Text
extractHrefValue chunk = do
    let lowered = T.toLower chunk
    hrefStart <- T.stripPrefix "href=" =<< findAttributeStart lowered "href="
    case T.uncons hrefStart of
        Just ('"', rest) -> Just $ T.takeWhile (/= '"') rest
        Just ('\'', rest) -> Just $ T.takeWhile (/= '\'') rest
        _ -> Just $ T.takeWhile (\char -> not (Char.isSpace char) && char /= '>') hrefStart

findAttributeStart :: Text -> Text -> Maybe Text
findAttributeStart haystack needle =
    snd <$> T.breakOnAll needle haystack L.!? 0

isUnsafeHref :: Text -> Bool
isUnsafeHref rawHref =
    let href = T.toLower $ T.strip rawHref
    in href /= "" &&
        not
            ( "/" `T.isPrefixOf` href
            || "#" `T.isPrefixOf` href
            || "http://" `T.isPrefixOf` href
            || "https://" `T.isPrefixOf` href
            || "mailto:" `T.isPrefixOf` href
            )

containsAny :: Text -> [Text] -> Bool
containsAny haystack = any (`T.isInfixOf` haystack)
