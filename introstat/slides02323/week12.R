
################################
## Sample size for givet power
## Stikprøvestørrelse for t-test
power.t.test(power = .80, delta = 4, sd = 12.21, sig.level=0.05,
      type = "one.sample")


################################
## Power beregning
## Beregn power for t-test
power.t.test(n = 40, delta = 4, sd = 12.21, sig.level=0.05,
      type = "one.sample")


################################
## Beregn stikprøvestørrelsen
power.t.test(power = 0.90, delta = 2, sd = 1, sig.level = 0.05)


################################
## Power beregning
power.t.test(n = 10, delta = 2, sd = 1, sig.level = 0.05)


################################
## Beregn margin of error
power.t.test(power = 0.90, n = 10, sd = 1, sig.level = 0.05)
