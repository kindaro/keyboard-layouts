#!/usr/bin/env stack
-- stack --resolver lts-14 script

{-# language NoImplicitPrelude #-}
{-# language OverloadedStrings #-}
{-# language BlockArguments #-}
{-# language QuasiQuotes #-}
{-# language TypeApplications #-}
{-# language PartialTypeSignatures #-}
{-# options_ghc -Wno-partial-type-signatures #-}


import RIO
import RIO.Process
import qualified RIO.Text as Text
import Path
import Path.IO
import Control.Monad.Trans.Cont

main :: IO ()
main = runSimpleApp do
    (_, files) <- listDirRecur [reldir|src|]
    let sources = filter ((== ".json") . fileExtension) files
    logInfo $ "Sources found: " <> displayShow sources
    xkbDir <- fmap (</> [reldir|xkb|]) getCurrentDir
    traverse_ (compile xkbDir) sources

    (_, names') <- listDirRecur (xkbDir </> [reldir|symbols|])
    let names = fmap filename names'
    logInfo $ "Symbols found: " <> displayShow names
    xkmDir <- fmap (</> [reldir|xkm|]) getCurrentDir
    traverse_ (build xkbDir xkmDir) names

compile :: _ => Path Abs Dir -> Path Abs File -> RIO env ()
compile targetDir x = do
    logInfo $ "Compiling: " <> displayShow x
    ensureDir targetDir
    proc "klfc" ["--from-json", fromAbsFile x, "--xkb", fromAbsDir targetDir] runProcess_

build :: _ => Path Abs Dir -> Path Abs Dir -> Path Rel File -> RIO env ()
build include targetDir x = do
    targetFileName  <- x -<.> "xkb"
    let target = targetDir </> targetFileName
    logInfo $ "Building: " <> displayShow x
    ensureDir targetDir
    hasTypes <- doesFileExist (include </> [reldir|types|] </> x)
    evalContT do
        setxkbmap' <- ContT $ proc "setxkbmap"
            $  [ "-I", fromAbsDir include ]
            ++ (if hasTypes then [ "-types", "complete+" <> fromRelFile x ] else [ ])
            ++ [ "-print", fromRelFile x ]
        let setxkbmap = setStdout createPipe setxkbmap'
        setxkbmapProcess <- ContT $ withProcessWait setxkbmap
        xkbcomp' <- ContT $ proc "xkbcomp"
            [ "-I" <> fromAbsDir include
            , "-xkb"
            , "-", fromAbsFile target
            ]
        let xkbcomp = setStdin (useHandleClose (getStdout setxkbmapProcess)) xkbcomp'
        runProcess_ xkbcomp
