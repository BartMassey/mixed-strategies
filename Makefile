HC = ghc
HCFLAGS = -Wall

oms: oms.hs OptimalMixedStrategy.hs Text/SimpleTabular.hs
	$(HC) $(HCFLAGS) --make oms.hs
