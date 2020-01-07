#!/usr/bin/env stack
-- stack --resolver lts-14 script

{-# language NoImplicitPrelude #-}
{-# language OverloadedStrings #-}
{-# language BlockArguments #-}
{-# language QuasiQuotes #-}
{-# language TypeApplications #-}
{-# language PartialTypeSignatures #-}

import RIO
import RIO.Process
import Path
import Path.IO

main :: IO ()
main = runSimpleApp do
    (_, files) <- listDirRecur [reldir|src|]
    let sources = filter ((== ".json") . fileExtension) files
    logInfo $ "Sources found: " <> displayShow sources
    traverse_ compile sources

compile :: _ => Path Abs File -> RIO env ()
compile x = do
    logInfo $ "Compiling: " <> displayShow x
    target <- fmap (</> [reldir|xkb|]) getCurrentDir
    ensureDir target
    proc "klfc" ["--from-json", fromAbsFile x, "--xkb", fromAbsDir target] runProcess_
