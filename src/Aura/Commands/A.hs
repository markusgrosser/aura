{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- Handles all `-A` operations

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

module Aura.Commands.A
    ( install
    , upgradeAURPkgs
    , aurPkgInfo
    , aurPkgSearch
    , displayPkgDeps
    , cloneSource
    , displayPkgbuild ) where

import           Control.Monad
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import qualified Data.Set as S (member, fromList)
import qualified Data.Text as T
import           Data.Foldable (traverse_, fold)
import           Linux.Arch.Aur
import           Text.Printf (printf)
import           Text.Regex.PCRE ((=~))

import           Aura.Install (InstallOptions(..))
import qualified Aura.Install as I

import           Aura.Pkgbuild.Base
import           Aura.Settings.Base
import           Aura.Packages.ABS (absDepsRepo)
import           Aura.Packages.AUR
import           Aura.Colour.Text
import           Aura.Monad.Aura
import           Aura.Languages
import           Aura.Utils
import           Aura.Bash (namespace, Namespace)
import           Aura.Core
import           Aura.Utils.Numbers

import           Internet (gitClone)
import           Utilities (whenM)

---

installOptions :: Aura I.InstallOptions
installOptions = do
    depsRepo <- absDepsRepo
    pure I.InstallOptions { label         = "AUR"
                          , installLookup = aurLookup
                          , repository    = depsRepo <> aurRepo
                          }

install :: [String] -> [String] -> Aura ()
install pacOpts ps = do
    opts <- installOptions
    I.install opts pacOpts ps

upgradeAURPkgs :: [String] -> [String] -> Aura ()
upgradeAURPkgs pacOpts pkgs = ask >>= \ss -> do
  let notIgnored p = splitName p `notElem` ignoredPkgsOf ss
  notify upgradeAURPkgs_1
  foreignPkgs <- filter (\(n, _) -> notIgnored n) <$> foreignPackages
  aurInfos    <- aurInfo (T.pack . fst <$> foreignPkgs)
  let aurPkgs   = filter (\(n, _) -> T.pack n `elem` (aurNameOf <$> aurInfos)) foreignPkgs
      toUpgrade = filter isntMostRecent $ zip aurInfos (snd <$> aurPkgs)
  auraFirst <- auraCheck (aurNameOf . fst <$> toUpgrade)
  if auraFirst
     then auraUpgrade pacOpts
     else do
       devel <- develPkgCheck  -- [String]
       notify upgradeAURPkgs_2
       if null toUpgrade && null devel
          then warn upgradeAURPkgs_3
          else reportPkgsToUpgrade $ (T.unpack . prettify <$> toUpgrade) <> devel
       install pacOpts $ (T.unpack . aurNameOf . fst <$> toUpgrade) <> pkgs <> devel
           where prettify (p, v) = aurNameOf p <> " : " <> T.pack v <> " => " <> aurVersionOf p
-- TODO: Use `printf` with `prettify` to line up the colons.

auraCheck :: [T.Text] -> Aura Bool
auraCheck toUpgrade = if "aura" `elem` toUpgrade
                         then optionalPrompt auraCheck_1
                         else pure False

auraUpgrade :: [String] -> Aura ()
auraUpgrade pacOpts = install pacOpts ["aura"]

develPkgCheck :: Aura [String]
develPkgCheck = ask >>= \ss ->
  if rebuildDevel ss then develPkgs else pure []

aurPkgInfo :: [String] -> Aura ()
aurPkgInfo (fmap T.pack -> pkgs) = aurInfo pkgs >>= traverse_ displayAurPkgInfo

-- By this point, the Package definitely exists, so we can assume its
-- PKGBUILD exists on the AUR servers as well.
displayAurPkgInfo :: AurInfo -> Aura ()
displayAurPkgInfo ai = ask >>= \ss -> do
    let name = T.unpack $ aurNameOf ai
    ns <- fromJust <$> pkgbuild name >>= namespace name . T.unpack
    liftIO $ putStrLn $ renderAurPkgInfo ss ai ns <> "\n"

renderAurPkgInfo :: Settings -> AurInfo -> Namespace -> String
renderAurPkgInfo ss ai ns = entrify ss fields entries
    where fields   = fmap bForeground . infoFields . langOf $ ss
          empty x  = case x of [] -> "None"; _ -> x
          entries = [ magenta "aur"
                    , bForeground $ T.unpack $ aurNameOf ai
                    , T.unpack $ aurVersionOf ai
                    , outOfDateMsg (dateObsoleteOf ai) $ langOf ss
                    , orphanedMsg (T.unpack <$> aurMaintainerOf ai) $ langOf ss
                    , cyan $ T.unpack $ urlOf ai
                    , pkgUrl $ T.unpack $ aurNameOf ai
                    , T.unpack . T.unwords $ licenseOf ai
                    , empty . unwords $ depends ns
                    , empty . unwords $ makedepends ns
                    , yellow . show $ aurVotesOf ai
                    , yellow $ printf "%0.2f" (popularityOf ai)
                    , T.unpack $ aurDescriptionOf ai ]

aurPkgSearch :: [String] -> Aura ()
aurPkgSearch [] = pure ()
aurPkgSearch (fold -> regex) = ask >>= \ss -> do
    db <- S.fromList . fmap fst <$> foreignPackages
    let t = case truncationOf ss of  -- Can't this go anywhere else?
              None -> id
              Head n -> take n
              Tail n -> reverse . take n . reverse
    results <- fmap (\x -> (x, T.unpack (aurNameOf x) `S.member` db)) . t
                 <$> aurSearch (T.pack regex)
    traverse_ (liftIO . putStrLn . renderSearch ss regex) results

renderSearch :: Settings -> String -> (AurInfo, Bool) -> String
renderSearch ss r (i, e) = searchResult
    where searchResult = if beQuiet ss then sparseInfo else verboseInfo
          sparseInfo   = T.unpack $ aurNameOf i
          verboseInfo  = repo <> n <> " " <> v <> " (" <> l <> " | " <> p <>
                         ")" <> (if e then s else "") <> "\n    " <> d
          c cl cs = case cs =~ ("(?i)" <> r) of
                      (b, m, a) -> cl b <> bCyan m <> cl a
          repo = magenta "aur/"
          n = c bForeground $ T.unpack $ aurNameOf i
          d = c noColour $ T.unpack $ aurDescriptionOf i
          l = yellow . show $ aurVotesOf i  -- `l` for likes?
          p = yellow $ printf "%0.2f" (popularityOf i)
          v = case dateObsoleteOf i of
            Just _  -> red $ T.unpack $ aurVersionOf i
            Nothing -> green $ T.unpack $ aurVersionOf i
          s = c bForeground (" [installed]" :: String)
                              
displayPkgDeps :: [String] -> Aura ()
displayPkgDeps ps = do
    opts <- installOptions
    I.displayPkgDeps opts ps

-- | Clone several AUR packages' PKGBUILD, .SRCINFO and other files from
-- the AUR
--
-- Handles split packages, albeit in a not very elegant way
cloneSource :: [String] -> Aura ()
cloneSource pkgs = traverse_ clone pkgs
  where
    clone pkg = whenM (isAurPackage pkg) $ do
        notify $ cloneSource_1 pkg
        void $ liftIO $ gitClone (gitUrl pkg) Nothing

displayPkgbuild :: [String] -> Aura ()
displayPkgbuild = I.displayPkgbuild (traverse (fmap (fmap T.unpack) . pkgbuild))

isntMostRecent :: (AurInfo, String) -> Bool
isntMostRecent (ai, v) = trueVer > currVer
  where trueVer = version $ T.unpack $ aurVersionOf ai
        currVer = version v

------------
-- REPORTING
------------
reportPkgsToUpgrade :: [String] -> Aura ()
reportPkgsToUpgrade pkgs = asks langOf >>= \lang ->
  printList green cyan (reportPkgsToUpgrade_1 lang) pkgs
