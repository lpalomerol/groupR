context('Tertilize tests')

test_that('Should split values in factors', {
  #Arrange
  input = c(1,2,3)
  #Act
  output = tertilize(input)
  #Assert
  expect_equal(
    as.numeric(output), c(1,2,3)
  )

  expect_equal(
    as.character(output[1]), '[1,1.67]'
  )
})

test_that('Should split values in factors, ignoring nulls', {
  #Arrange
  input = c(1,2,3, NA)
  #Act
  output = tertilize(input)
  #Assert
  expect_equal(
    as.numeric(output), c(1,2,3, NA)
  )
})

test_that('Should split values in factos with low-mid-high groups when required', {
  input = c(1,2,3)
  #Act
  output = tertilize(input, withLabels = TRUE)
  #Assert
  expect_equal(
    as.numeric(output), c(1,2,3)
  )

  expect_equal(
    as.character(output), c('LOW', 'MID', 'HIGH')
  )

})


test_that('Should split values in factors, boundary values are assigned to low group', {
  #Arrange
  input = c(1,2,3, 4)
  #Act
  output = tertilize(input, withLabels = TRUE)
  #Assert
  expect_equal(
    as.numeric(output), c(1,1,2,3)
  )

  expect_equal(
    as.character(output), c('LOW', 'LOW', 'MID', 'HIGH')
  )
})
