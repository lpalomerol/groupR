context("Medianize.R")

test_that('Should split two items in low-high using median as criteria', {
  #Arrange
  input = c(1,5)
  #Act
  output = medianize(input)
  #Assert
  expect_equal(as.character(output), c('LOW', 'HIGH'))
})


test_that('Should split two items in low-high using median as criteria (order matters)', {
  #Arrange
  input = c(5,1)
  #Act
  output = medianize(input)
  #Assert
  expect_equal(
    as.character(output),
    c('HIGH', 'LOW'))
})


test_that('Should ignore NA values', {
  #Arrange
  input = c(5,1, NA)
  #Act
  output = medianize(input)
  #Assert
  expect_equal(
    as.character(output),
    c('HIGH', 'LOW', NA))
})

test_that('With odd number of values, the middle value factor is assigned to LOW group', {
  #Arrange
  input = c(1,2,3)
  #Act
  output = medianize(input)
  #Assert
  expect_equal(
    as.character(output),
    c('LOW', 'LOW','HIGH'))
})


