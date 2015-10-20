@echo off

SET GIT_Weka_Data="https://github.com/fehu/min-dat--weka-data.git"
SET GIT_Decision_Trees="https://github.com/fehu/min-dat--decision-trees.git"

SET ROOT_DIR=%cd%


call git clone %GIT_Weka_Data% WekaData
call git clone %GIT_Decision_Trees% DecisionTrees


cd "%ROOT_DIR%\WekaData"
call cabal sandbox init
call :build

cd "%ROOT_DIR%\DecisionTrees"

call cabal sandbox init
call cabal install "%ROOT_DIR%\WekaData"
call :build

goto:eof
:build 
echo "AAA"
call cabal configure
call cabal install --enable-tests --dependencies-only
call cabal build
goto:eof