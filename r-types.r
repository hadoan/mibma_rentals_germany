x <- "dataset"
typeof(x)
attributes(x)
y <- 1:10
y
typeof(y)
length(y)
class(y)
attributes(y)
z <- as.numeric(y)
z
typeof(z)
vector() # an empty 'logical' (the default) vector
vector("character", length = 5) # a vector of mode 'character' with 5 elements
character(5) # the same thing, but using the constructor directly
numeric(5)   # a numeric vector with 5 elements
x <- c(1, 2, 3)
x
z <- c("Sarah", "Tracy", "Jon")
z
class(z)
x <- c(0.5, NA, 0.7)
x <- c(TRUE, FALSE, NA)
x <- c("a", NA, "c", "d", "e")
x <- c(1+5i, 2-3i, NA)
x
0/0
xx <- c(1.7, "a")
xx <- c(TRUE, 2)
xx <- c("a", TRUE)
xx
xx[2]
xx[1]
xx[0]

m <- matrix(nrow = 2, ncol = 2)
m
dim(m)
class(m)
typeof(m)

FOURS <- matrix(
  c(4, 4, 11, 41),
  nrow = 2,
  ncol = 2)
FOURS

x <- list(1, "a", TRUE, 1+4i)
x
x[4]

# Create the data frame.
emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25), 
  
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  stringsAsFactors = FALSE
)
# Print the data frame.			
print(emp.data) 
emp.data[1]

emp.data["emp_id"]
c(emp.data$emp_id)
