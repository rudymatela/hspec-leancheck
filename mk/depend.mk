eg/minimal: \
  eg/minimal.hs \
  mk/toplibs
eg/minimal.o: \
  src/Test/Hspec/LeanCheck.hs \
  eg/minimal.hs
mk/All.o: \
  src/Test/Hspec/LeanCheck.hs \
  mk/All.hs
mk/Toplibs.o: \
  src/Test/Hspec/LeanCheck.hs \
  mk/Toplibs.hs
Setup.o: \
  Setup.hs
Setup: \
  Setup.hs \
  mk/toplibs
src/Test/Hspec/LeanCheck.o: \
  src/Test/Hspec/LeanCheck.hs
tests/test.o: \
  tests/test.hs \
  src/Test/Hspec/LeanCheck.hs
tests/test: \
  tests/test.hs \
  mk/toplibs
