test_that("Functional Group arithmetic", {
  my_FG = FunctionalGroup$new()
  halved = 0.5 * my_FG
  expect_equal(halved$ST1, 0.5 * my_FG$ST1)

  added = my_FG + halved
  expect_equal(added$ST1, halved$ST1 + my_FG$ST1)
})

test_that("build_functional_group", {
 parameters = list(w_FGA = 0.4,
                   w_FGB = 0.3,
                   w_FGC = 0.2,
                   w_FGD = 0.1,
                   NI = 0.9,
                   ST1 = 1)
 new_parameters = build_functional_group(parameters)
 new_FG = parameters[["w_FGA"]] * FG_A +
          parameters[["w_FGB"]] * FG_B +
          parameters[["w_FGC"]] * FG_C +
          parameters[["w_FGD"]] * FG_D
 expect_equal(new_parameters$ST1, new_FG$ST1)
})
