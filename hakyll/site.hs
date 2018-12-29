-- -*- tab-width: 4; -*-

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Options
import           System.Directory
import           System.FilePath.Posix


--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyllWith myConfiguration $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.html"]) $ do
        route   $ idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" myDefaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ myPandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/page.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    myDefaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< (loadAllSnapshots "posts/*" "content")
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Lab notes" <>
                    myDefaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls


    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
myDefaultContext :: Context String
myDefaultContext =
  constField "github_username" "artempyanykh" <>
  constField "linkedin_username" "artempyanykh" <>
  constField "twitter_username" "artem_pyanykh" <>
  defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    teaserField "teaser" "content" <>
    myDefaultContext

myWriterOptions = defaultHakyllWriterOptions
  { writerHTMLMathMethod = MathJax "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" }

myPandocCompiler = pandocCompilerWith defaultHakyllReaderOptions myWriterOptions

myConfiguration = defaultConfiguration { deployCommand = "rm -rf ../blog && cp -r _site ../blog" }
