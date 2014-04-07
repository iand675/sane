{-# LANGUAGE OverloadedStrings #-}
module Shakefile where
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import System.Directory


{-
main = shakeArgs shakeOptions $ do
  want
    [ "api-server.tar.gz"
    , "lib/feature-flags"
    , "lib/librato"
    , "lib/logentries"
    , "lib/metrics"
    , "lib/stripe-api"
    -- , "sane-site.tar.gz"
    -- , "sane-framework.tar.gz"
    -- , "sane-executor.tar.gz"
    ]
    [ "site.tar.gz" ]
  "api-server.tar.gz" *> bundleServer
  "site.tar.gz" *> bundleSite
  "lib/*" *> retrieveGitDependency
  "..//Gruntfile.js" *> grunt

installNodeDependencies :: FilePath -> Action ()
installNodeDependencies fp = do
  alwaysRerun
  need [dropFileName fp </> "package.json"]
  command_ [] "npm" ["install"]

grunt :: FilePath -> Action ()
grunt fp = do
  alwaysRerun
  let dir = dropFileName fp
  need [dir </> "Gruntfile.js", dir </> "node_modules/"]
  command_ [Cwd dir] "grunt" []

bundleSite :: FilePath -> Action ()
bundleSite out = do
  need
    [ "../www/Gruntfile.js"
    , "../www/package.json"
    ]
  command_ [Cwd "../www"] "grunt" ["less:dev", "jade:dev"]

bundleServer :: FilePath -> Action ()
bundleServer out = do
  let project = dropExtensions out
  need ["lib/webmachine", "lib/metrics"]
  command_ [] "echo" ["bundle server plz"]

retrieveGitDependency :: FilePath -> Action ()
retrieveGitDependency fp = do
  exists <- doesDirectoryExist fp
  if exists
    then do
      command_ [Cwd fp] "git" ["pull", "--rebase"]
    else do
      liftIO $ putStrLn $ takeFileName fp
      command_ [Cwd "lib"] "git" ["clone", "git@github.com:SaneApp/" ++ takeFileName fp ++ ".git"]
      -- command_ [] "cabal" ["sandbox", "add-source", fp]

dump :: Show a => a -> Action ()
dump = liftIO . print

main = shakeArgs shakeOptions $ do
  want
    [ "dist/www.tar.gz"
    ]
  "//*.tar.gz" *> \out -> do
    need
      [ "www/index.html"
      , "www/styles/main.css"
      ]
    let dir = addTrailingPathSeparator $ joinPath $ tail $ splitPath $ dropExtensions out
    need [dir]
    command_ [] "tar" ["-zcvf", out, dir]
  "www//*.html" *> \out -> do
    let jadeFile = ".." </> replaceExtension out "jade"
    need [jadeFile]
    command_ [Cwd "../www"] "grunt" ["jade:dev"]
    copyFile' (".." </> out) out
  "www//*.css" *> \out -> do
    command_ [Cwd "../www"] "grunt" ["less:dev"]
    copyFile' (".." </> out) out
  "www.tar.gz" *> \_ -> do
    let lessFile = "www/styles/main.css"
    jadeFiles <- getDirectoryFiles "../www/" ["//*.jade"]
    let htmlFiles = map (\f -> "www/" </> replaceExtension f "html") jadeFiles
    let frontendApp = [ lessFile ] ++ htmlFiles
    need frontendApp
  phony "deploy" $ do
    let deploymentPackages = [ "dist/www.tar.gz"
                             ]
    need deploymentPackages
    machines <- liftIO productionMachines
    let uploads = [ (machine, package) | machine <- machines, package <- deploymentPackages ]
    let format (machine, package) = liftIO $ putStrLn ("Uploading " ++ package ++ " to " ++ T.unpack machine)
    mapM_ (\x -> format x >> uncurry scp x) uploads

-}

bundleSite = "dist/site.tar.gz" *> \out -> do
  let srcDir = "www/src"
  let bundleDir = "build/site"
  compileableAssets <- getDirectoryFiles srcDir ["//*.jade", "//*.less"]
  need $ map (srcDir </>) compileableAssets
  command_ [Cwd "www"] "grunt" ["jade:dev", "less:dev"]
  liftIO $ createDirectoryIfMissing True "build/site"
  compiledAssets <- getDirectoryFiles srcDir ["//*.html", "//*.css", "//*.js", "//*.png"]
  need $ map (srcDir </>) compiledAssets
  forM_ compiledAssets $ \asset -> do
    let srcAsset = srcDir </> asset
    let destAsset = bundleDir </> asset
    liftIO $ putStrLn ("Copying " ++ srcAsset ++ " to " ++ destAsset)
    liftIO $ createDirectoryIfMissing True $ takeDirectory destAsset
    copyFile' srcAsset destAsset
  command_ [] "tar" ["-zcvf", out, bundleDir]

bundleServer = "dist/server.tar.gz" *> \out -> do
  let srcDir = "server/src"
  return ()

main = shakeArgs shakeOptions $ do
  want []
  bundleServer
  bundleSite
  "bundle" ~> do
    need ["dist/site.tar.gz", "dist/server.tar.gz"]

