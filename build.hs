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
    targetFileName  <- x -<.> "xkm"
    let target = targetDir </> targetFileName
    logInfo $ "Building: " <> displayShow x
    ensureDir targetDir
    proc "setxkbmap" ["-I", fromAbsDir include, "-print", fromRelFile x] \setxkbmap' ->
        let setxkbmap = setStdout createPipe setxkbmap'
        in withProcessWait setxkbmap \setxkbmapProcess ->
            proc "xkbcomp" ["-I" <> fromAbsDir include, "-xkm", "-", fromAbsFile target] \xkbcomp' ->
                let xkbcomp = setStdin (useHandleClose (getStdout setxkbmapProcess)) xkbcomp'
                in runProcess_ xkbcomp
