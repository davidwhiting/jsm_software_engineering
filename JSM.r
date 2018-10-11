# ----------------------------------------------------
# MODULE 1
# ----------------------------------------------------
# Motivating Example
# What does this code do?

gss <- function(f, ax, bx, cx, tol) {
  C <- (3 - sqrt(5))/2
  R <- 1 - C
  x0 <- ax; x3 <- cx
  if (abs(cx - bx) > abs(bx - ax)) {
    x1 <- bx; x2 <- bx + C * (cx - bx)
  } else {
    x2 <- bx; x1 <- bx - C * (bx - ax)
  }
  f1 <- f(x1); f2 <- f(x2)
  while (abs(x3 - x0) > tol * (abs(x1) + abs(x2))) {
    if (f2 < f1) {
      x0 <- x1; x1 <- x2
      x2 <- R * x1 + C * x3
      f1 <- f2; f2 <- f(x2)
    } else {
      x3 <- x2; x2 <- x1
      x1 <- R * x2 + C * x0
      f2 <- f1; f1 <- f(x1)
    }
  }
  return (if (f1 < f2) x1 else x2)
}

# ----------------------------------------------------
# MODULE 2
# ----------------------------------------------------
# Performance of disk operations.

# Create an example input file :

msg <- "Hello World this is text"  # 25 chars per line.
tmp.file <- tempfile()
out.file <- file(tmp.file, "wt")
writeLines(rep(msg, 40000000), out.file)  # 40m lines, 25 bytes = 1GB
close(out.file)

small.tmp.file <- tempfile()
out.file2 <- file(small.tmp.file, "wt")
writeLines(rep(msg, 400000), out.file2)
close(out.file2)

num.lines <- 10

# Now time your solution!
system.time(naive.tail(tmp.file, num.lines))

naive.tail <- function(input.file, num.lines=10) {
  lines <- readLines(file(input.file))
  tail(lines, num.lines)
}


# ----------------------------------------------------
# MODULE 3
# ----------------------------------------------------
# Floating Point / Numerical Representations

# What is the smallest number N which R errorneously reports
# that N == N + 1 ?

1 == 1 + 1
# FALSE

100 == 100+1
# FALSE


# What is smallest N?

# Example 1
tmp <- data.frame(n=c("72057594037927936",
                    "72057594037927937"),
                    name=c("foo", "bar"))
length(unique(tmp$n))  # 2
tmp.file <- tempfile()
write.csv(tmp, tmp.file, quote=FALSE, 
          row.names=FALSE)
data <- read.csv(tmp.file)
length(unique(data$n)) 
# What happened here?  Why?  (may be different after R 3.0.1)

# Example 2
# Same code from earlier modules:
golden <- function(f, ax, bx, cx, tol) {
  C <- (3 - sqrt(5))/2; R <- 1 - C
  x0 <- ax; x3 <- cx
  if (abs(cx - bx) > abs(bx - ax)) {
    x1 <- bx; x2 <- bx + C * (cx - bx)
  } else {
    x2 <- bx; x1 <- bx - C * (bx - ax)
  }
  f1 <- f(x1); f2 <- f(x2)
  while (abs(x3 - x0) > tol * (abs(x1) + abs(x2))) {
    if (f2 < f1) {
      x0 <- x1; x1 <- x2
      x2 <- R * x1 + C * x3
      f1 <- f2; f2 <- f(x2)
    } else {
      x3 <- x2; x2 <- x1
      x1 <- R * x2 + C * x0
      f2 <- f1; f1 <- f(x1)
    }
  }
  return (if (f1 < f2) x1 else x2)
}

##
f <- function(x) x + 1
g <- function(x) x^2
golden(g, -1, 1, 2, .0001)
# Returns something very near 0.

# Takes a really long time...
golden(f, 0, 1, 2, .0000001)



msg <- "Hello World this is text"  # 25 chars per line.
tmp.file <- tempfile()
out.file <- file(tmp.file, "wt")
writeLines(rep(msg, 40000000), out.file)  # 40m lines, 25 bytes = 1GB
close(out.file)

num.lines <- 10

system.time(newtail(tmp.file, num.lines))
system.time(naive.tail(tmp.file, num.lines))
# 33 seconds for 1GB file on my machine.

# Section 2. Numerical methods.

### Now duplicate this while loop problem directly.

# Take any two numbers,


# ----------------------------------------------------
# MODULE 4
# ----------------------------------------------------
# Revision Control Tools

# svnadmin create ~/myrepo
# mkdir ~/jsm/svn
# cd ~/jsm/svn
# echo 'hello world' > hello.txt
# svn import /Users/mstokely/jsm/svn file:///Users/mstokely/myrepo/jsm -m 'initial import'

# Then create a working directory:
#
# mkdir src ; cd src ; svn co file:///Users/mstokely/myrepo/jsm jsm
# Now we are ready to edit, make changes.

# cd jsm
# svn log hello.txt
# echo hello jsm > hello.txt
# svn ci hello.txt
# svn log hello.txt

# ----------------------------------------------------
# MODULE 5
# ----------------------------------------------------
# Testing Tools

# An example function we might want to test.
c2f <- function(c) return((9/5)*c + 32)

# An example unit test, verifying behavior of that function

test.c2f <- function() {
  checkEquals(c2f(0), 32)
  checkEquals(c2f(10), 50)
  checkException(c2f("xx"))
}

# Run it manually:

if (!require(RUnit)) {
  install.packages("RUnit")
  library(RUnit)
}
test.c2f()

## Run it automatically, as is done with C CMD CHECK .
# Copy test.c2f to runit.c2f.R in tests subdirectory, then:

test.suite <- defineTestSuite("JSM Course", dirs="tests")
results <- runTestSuite(test.suite)
printTextProtocol(results)

##


# MODULE 8
# ----------------------------------------------------
# Documentation Tools

# First we clear out our workspace
rm(list=ls())

# Then we define the c2f function again:


# Create a temp directory for our pkg
tmp.dir <- tempdir()
dir.create(tmp.dir)

# Then use package.skeleton() to create a new package directory as if we are
# creating a new package.
package.skeleton(name="c2fpkg2",path=tmp.dir)

print(paste("Now edit the file in", file.path(tmp.dir, "c2fpkg2",
                                              "R", "c2f.R"),
            "to include the Roxygen comments above the function definition."))
      
# Now edit the file in and add the comment headers back in
roxygenise(file.path(tmp.dir, "c2fpkg2"))

# Now a c2f.Rd file has been automatically generated for us with documentation.
# This step can be run automatically with R package installation tools.

