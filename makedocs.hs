{-#LANGUAGE OverloadedStrings#-}
import Shelly
import Prelude hiding (FilePath)

configure    = command_ "cabal" ["configure","-fopencv23"] []
haddock      = command_ "cabal" ["haddock"] []
moveToTemp   = command_ "mv"    ["-f","dist/doc","/tmp"] []
cleanTmp     = command_ "rm"    ["-rf", "/tmp/doc"] []
moveFromTemp = command_ "cp"    ["-rf","/tmp/doc","dist/"] []
checkout a   = command_ "git"   ["checkout",a] []
add          = command_ "git"   ["add","dist/doc/*"] []
commit       = command_ "git"   ["commit","-m 'documentation updated'"] []

main = shelly $ verbosely $ do
      echo "Running haddock"
      silently haddock
      cleanTmp
      moveToTemp
      silently checkout "gh-pages"
      moveFromTemp
      add
      commit
      echo "documentation updated. You can now push `gh-pages` branch to github"
      silently checkout "VT/development"
