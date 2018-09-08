eg/minimal: \
  eg/minimal.hs \
  mk/toplibs
eg/minimal.o: \
  src/Test/Hspec/LeanCheck.hs \
  eg/minimal.hs
eg/should: \
  eg/should.hs \
  mk/toplibs
eg/should.o: \
  src/Test/Hspec/LeanCheck.hs \
  eg/should.hs
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
src/Test/Hspec/LeanCheck: \
  mk/toplibs
src/Test/Hspec/LeanCheck.o: \
  src/Test/Hspec/LeanCheck.hs
tests/should.o: \
  tests/should.hs \
  src/Test/Hspec/LeanCheck.hs
tests/should: \
  tests/should.hs \
  mk/toplibs
tests/test.o: \
  tests/test.hs \
  src/Test/Hspec/LeanCheck.hs
tests/test: \
  tests/test.hs \
  mk/toplibs
