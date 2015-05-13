context("SimBM, Data generating")

test_that("Brownian Motion Dimensions",{
    
    expect_equal(dim(SimBM(500))[1], 500)
    expect_equal(dim(SimBM(500))[2], 2)
    expect_error(SimBM(-3), paste0("Input length for Brownian motion ", -3," is illegal, use positive number!"))
    expect_error(SimBM(50,-2), paste0("Input sigma ", -2," for Brownian motion is illegal, use positive number!"))
})
