#Functions that mainly correspond to 8 keys in 
#the R-Instat calculator functions keyboard 

#Uncorrected sum-of-squares 
#For example ssq(c(8,2,5)) is 93
ssq <- function(x) {sum(x * x, na.rm=TRUE)}

#corrected sum of squares 
#For example cssq(c(2,8,5)) is 18
cssq <- function(x) {sum((x - mean(x))^2 ,na.rm=TRUE)}   

#Sum of the digits for an integer variable. For example 
#For example digitsum(c(8,23,471)) is 8, 5, 12
digitsum <- function(x) {sapply(x ,function(n){a<-as.integer(c(strsplit(as.character(n),split="")[[1]])); sum(a)})}
#It is supposed to be the same as DescTools::DigitSum key in the integer keyboard
#However there are differences that we discuss in the Help, because they
#illustrate the difference between what we term a script writer
#compared to a software developer!

#Squares of the digits in integers.  For example dsqu(c(8, 23, 471)) is 64, (4,9), (16, 49, 1)
digitsqu <- function(x) {sapply(x ,function(n){a<-as.integer(c(strsplit(as.character(n),split="")[[1]])); a^2})}

#digit sums of squares. For example dssq(c(8,23,471)) is 64, 13, 66

digitssq <- function(x) {sapply(x ,function(n){a<-as.integer(c(strsplit(as.character(n),split="")[[1]])); sum(a^2)})}

#Binomial coefficients, into a Pascal triangle if consecutive integers
#For example pascal(c(1,2,3,4)) gives (1, 1), (1, 2, 1), (1, 3, 3, 1), (1, 4, 6, 4, 1)
pascal <- function(x) {sapply(x ,function(x) {lapply(x, function(i) {choose(i, 0:i)})})}

#The impressive fractions function from the MASS package
#displayed as a character variable.
#for example fractions(c(0.75,2.3,0.28)) gives ("3/4", "23/10", "7/25" )
fractions <- function(x) {as.character(MASS::fractions(x))}

#The opposite of the fractions function and returns the values to decimals
#For example decimals(c("3/4", "23/10", "7/25")) gives (0.75, 2.3, 0.28)
decimals <- function(x)  {sapply(x , FUN = function(v) {sapply(v,FUN = function(w) eval(parse(text=w)))})}




