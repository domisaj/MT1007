# Fahrenheit to Celsius
fahrenheit_to_celsius <- function(temp_F) {
  temp_c <- (temp_F - 32) * 5 / 9
  return(temp_c)
}
# freezing point of water
fahrenheit_to_celsius(32)
# boiling point of water
fahrenheit_to_celsius(212)

# square a number
square_number <- function(x) {
  squared_number <- x^2
  return(squared_number)
}
square_number(5)

# times a number by 5
times.by.5 <- function(x) {
  x*5
}
times.by.5(600)

# sum of squares
sum.of.squares <- function(x, y) {
  x^2 + y^2
}
sum.of.squares(5, 4)
