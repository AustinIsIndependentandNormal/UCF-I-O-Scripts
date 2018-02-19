# empty out R of all objects
rm(list = ls())

# package list that we will be using during the demo. this code will 
# download packages that you don't have, and then load all of the
# packages contained in the pkg object

pkg <- c('pryr', 'devtools', 'sqldf', 'biglm', 'speedglm', 'dplyr', 'ff', 
         'data.table', 'tidyr', 'readr', 'ffbase', 'doParallel', 'bigmemory', 
         'biganalytics', 'microbenchmark')

# nice function to load packages and install missing packages
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]; new.pkg
    if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

ipak(pkg)

## set working directory to the where the Tutorial folder lives on your machine

setwd("~/Desktop/Tutorial")

##### Efficient coding practices

# Memory

# how much memory does it take to store 1 million integers? Let's take a look

mem_change(x <- 1:2e6)

# if you enter the line above again you will see that the mem_change 
# function returns 1.23 kB, this is because even operations that dont 
# do anything use a bit of memory given that mem_change might be confusing when trying
# to determine memory when re-running code it might be helpful to instead look at the 
# size of certain objects to determine how much memory is being used

object_size(x)

# memory change will also show you how much memory you clear up with a given command
mem_change(remove(x))

# a quick check to see total memory used thus far
mem_used()

# you can use object_size to compare approaches 
# example: matrices are more memory efficient than data frames

m <- matrix(1:9, 3,3)
d <- as.data.frame(m)
object.size(m)
object.size(d)

# a demonstration of how the memory used in a vector does not increase proportionally 
# with vector length note that an empty vector still takes up 40 bytes of memory
sizes <- sapply(0:50, function(n) object_size(seq_len(n)))
plot(0:50, sizes, xlab = "Length", ylab = "Size (bytes)", type = "s")


# r only allocates memory for vectors that are 8, 16, 32, 48, 24, or 128 bytes long, 
# this is evident in the graph if you account for the 40 bytes used by an empty vector
# after 128 bytes R requests memory in 8 byte chunks


# we can use microbenchmark to demonstrate that mean() is actually slower than sum(x)/length(x)
mbDemo <- as.numeric(1:2e6)

microbenchmark(
  mean(mbDemo),
  sum(mbDemo)/length(mbDemo)
)

# why does this happen?
# let's take a look at the sourcecode for the base mean function
mean.default


# Preallocation

# create a function to fill in a vector of length n without preallocating the size
noallocation<- function(n) {
  a <- NULL
  for(i in 1:n) a <- c(a, i^2)
}

# create a function to fill in a vector of length n, but define the size of the vector first
preallocation<- function(n){
  a <- vector(length=n)
  for(i in 1:n) a[i] <- i^2
}

# now compare the time it takes to execute each function
# system.time will return the time in seconds, the definiton 
# of user and system time comes from your OS
system.time(noallocation(10000), gcFirst=T)
system.time(preallocation(10000), gcFirst=T)

# Avoiding copies in loops
x <- data.frame(matrix(runif(10000), ncol = 100))

# using vapply since it specifies the class of the return value 
# this will grab the std deviations of each column of x 
stddevs <- vapply(x, sd, numeric(1))

# let's compare the speed of using vapply and sapply
microbenchmark(vapply(x, sd, numeric(1)))
microbenchmark(sapply(x, sd))

# vapply is often a little faster because it specifcies the return value


# when using a data frame in a for loop, every iteration copies the dataframe
for(i in 1:5) {
  x[, i] <- x[, i] / stddevs[i]
  print(c(address(x), refs(x)))
}

# examine the address and refs to see how many different addresses are produced 
# for each item and how many times it is referenced

# this happens because data frame is not a primitive function, list is, 
# observe the difference once you switch to using a list
y <- as.list(x)

for(i in 1:5) {
  y[[i]] <- y[[i]] / stddevs[i]
  print(c(address(y), refs(y)))
}


#########################################################################################################
#########################################################################################################
#########################################################################################################


# Lets Look at Data
dat <- read.csv("Data/JAP.csv")
dim(dat)
names(dat)
str(dat)
apply(dat, 2, function(x) length(na.omit(x)))


# Overview of options. -------------------------------------------------------

### 1) What we used to do...

# Base R
dat <- read.csv("Data/JAP.csv", stringsAsFactors = FALSE)
dat_long <- data.frame(dat[1:15], stack(dat[16:37]))
dat_long <- dat_long[!is.na(dat_long$values), ]
author_count <- table(dat_long$values)
top_author <- (author_count[rev(order(author_count))])[1:20]
top_author


# What we do now...


# Dplyr, readr, & tidyr -- AKA The Hadleyverse
read_csv("Data/JAP.csv") %>%
gather(key = "order", value = "author", starts_with("author"), na.rm = TRUE) %>%
group_by(author) %>%
summarise(n= n()) %>% 
arrange(desc(n)) 

# Same thing thing without pipes.
dat <- read_csv("Data/JAP.csv")
dat_long   <- gather(dat, key = "order", value = "author", starts_with("author"), na.rm = TRUE)
dat_group  <- group_by(dat_long, author)
dat_count  <- summarise(dat_group, n= n()) 
top_author <- arrange(dat_count, desc(n))
top_author

# Data.table Way
dat <- fread("Data/JAP.csv", na.strings=c("NA", ""))
dat_long  <- melt(dat, id.vars = colnames(dat)[1:15], measure.vars = colnames(dat)[16:37])
dat_count <- dat_long[,list(count=.N), by="value"]
dat_count <- dat_count[complete.cases(dat_count), ]
dat_order <- dat_count[order(-count)]
dat_order[1:20]


### TIME

microbenchmark({
# Base R
dat <- read.csv("Data/JAP.csv", stringsAsFactors=FALSE)
dat_long <- data.frame(dat[1:15], stack(dat[16:37]))
dat_long <- dat_long[!is.na(dat_long$values), ]
author_count <- table(dat_long$values)
top_author <- (author_count[rev(order(author_count))])[1:20]
top_author
})
# 140.76 milliseconds


microbenchmark({
# Hadleyverse
read_csv("Data/JAP.csv") %>%
gather(key="order", value="author", starts_with("author"), na.rm=TRUE) %>%
group_by(author) %>%
summarise(n= n()) %>% 
arrange(desc(n)) 
})
# Mean = 66.6

microbenchmark({
# data.table
dat <- fread("Data/JAP.csv", na.strings=c("NA", ""))
dat_long  <- melt(dat, id.vars = colnames(dat)[1:15], measure.vars = colnames(dat)[16:37])
dat_count <- dat_long[,list(count=.N), by="value"]
dat_count <- dat_count[complete.cases(dat_count), ]
dat_order <- dat_count[order(-count)]
dat_order[1:20]
})
# Mean = 20.47 milliseconds


# Main Functions --------------------------------------------------------------


# Load Data into R
dat <- read_csv("Data/JAP.csv") 
dat

dat_sep <- separate(dat, pubdate, c("Year", "Month", "Day"))
dat_sep

dat_sel <- select(dat_sep, uid, Year, starts_with("author"))
dat_sel

dat_long   <- gather(dat_sel, key="order", value="author", starts_with("author"), na.rm=TRUE)
dat_long

dat_group  <- group_by(dat_long, uid, Year)
dat_group

dat_count  <- summarise(dat_group, n= n()) 
dat_count

dat_filter <- filter(dat_count, Year > 1978)
dat_filter

dat_group2  <- group_by(dat_filter, Year)
dat_group2

dat_avg <- summarise(dat_group2, Coauthors= mean(n))
plot(dat_avg)


# Pipe ------------------------------------------------------------------------


# Example1
sum(2, 5)
2 %>% sum(5)

# Example2
colnames(dat)
dat %>% colnames()


# Add piping ot our analysis --------------------------------------------------


# Load Data into R
read_csv("Data/JAP.csv") %>%
separate("pubdate", c("Year", "Month", "Day")) %>%
select(uid, Year, starts_with("author")) %>%
gather(key = "order", value = "author", starts_with("author"), na.rm = TRUE) %>%
group_by(uid, Year) %>%
summarise(n = n()) %>%
filter(Year > 1978) %>%
group_by(Year) %>%
summarise(Coauthors= mean(n)) %>%
plot()


#########################################################################################################
#########################################################################################################
#########################################################################################################



##### SQLite database creation, query, parallization, and biggish data package demos

### NOTE - For the remaining demonstrations we will be using smaller datasets (5 - 7 meg).
###        All of these functions were tested on data of over 1 gig and the timing will be shown 
###        for the full dataaset as well. At the end of this file, you will see code to recreate
###        the full datasets - the dataset ffTestFull.csv was included on the dropbox account for 
###        those who are interested.

### For this first section of the code, we will be using the dataset ffTestPart.csv which contains
### 90,000 cases on 6 numeric variables and 2 categorical variables. The dataset is a subset of
### the ffTestFull.csv dataset with 11,000,000 cases


# set the working directory to the Tutorial Folder that you downloaded
setwd("/users/jonesj/Desktop/Tutorial")

#### Example of creating a sqlite database from a pre-existing CSV file
# create empty sqlite database
db <- dbConnect(SQLite(), dbname = "Data/testPartSQLite.sqlite")
# Note - if database already exists, running the above command will connect to it

# write the data from ffTestPart.csv into the sqlite database
# name = "test" sets the name of the table 
dbWriteTable(conn = db, name = "test", value = "Data/ffTestPart.csv", 
             row.names = FALSE, header = TRUE)
dbDisconnect(db)
### using dplyr to access and manipulate sqlite database

db1 <- src_sqlite("Data/testPartSQLite.sqlite")
tb1 <- tbl(db1, "test")

nCases <- nrow(tb1); nCases

########## Chunking data - splitting the data into manageable sizes
########## Example of reading in chunks of data, doing some type of
########## computation, and then aggregating the results

# set chunkSize to grab 10000 cases at a time
chunkSize <- 10000

# compute how many total chunks will be created
numChunks <- nCases/chunkSize; numChunks

# create empty vecotr to contain results of each computation on each chunk
outTable  <- vector('list', length = as.numeric(numChunks))

# run through the dataset in chunks
system.time({
    for(i in 1:numChunks) {
     
        idRange <- ((i-1)*chunkSize + 1):(i*chunkSize)
    
        # this line executes filters the database table based on rowid range
        tmpDat <- collect(filter(tb1, rowid %in% idRange))
    
        # silly example of a computation - tablulate the gender variable in chunks
        outTable[[i]] <- table(tmpDat[, 'gender'])
        i <- i + 1
    
    }
}) # 0.350

# using testFUllSQlite.sqlite database this takes 54.363 seconds

# aggregate the tables into a single table
apply(do.call(rbind, outTable), 2, sum)

# the above example is silly b/c we can do the same summary table 
# entirely from SQLite commands - here is how to do it with dplyr
table_gender <- summarise(group_by(tb1, gender), count = n())
# collect turns the tbl_sqlite object into a tbl_df object and puts
# into RAM (hadleys version of a data.frame)
system.time({tg1 <- collect(table_gender); print(tg1)}) # .083 seconds

# using testFullSQL.sqlite database this takes 13.23 seconds

explain(table_gender) # shows the SQLite query

####### We have essentially written a map reduce algorithm for 
####### tabulating chunks of data (the map part), 
####### and then aggregating the separate tables into a single table (the reduce part)

###############################################################################################

### Example of running a regression model on all the cases in the database
### the package speedglm allows a user to update an existing (general) linear model
### with new data. We can take advantage of this feature, and build a regression model
### using the chunking approach from above

chunkSize <- 30000
numRuns <- nCases/chunkSize - 1; numRuns

startData <- filter(tb1, rowid %in% 1:chunkSize)

m1 <- speedlm(X1 ~ X2 + X3 + X4 + X5 + X6, data = startData)

for(i in 1:numRuns) {
    idRange <- (i*chunkSize + 1):((i+1) * chunkSize)
    updateData <- filter(tb1, rowid %in% idRange)
    m1 <- updateWithMoreData(m1, updateData)
}

summary(m1)

########################################## parallelization

### lets read in a chunk of data from the sqlite database
### to bootstrap a statistic from

bootCases <- 7000
bootData1 <- dplyr::select(tb1, X1, X2)
bootData  <- collect(filter(bootData1, rowid <= bootCases))

### bootstrap example sequentially
# number of bootstrap samples to take
numBoot <- 10000
outCor  <- vector('numeric', length = numBoot)

# this resamples the data 10000 times, calculates the correlaiton coefficients
# each time, and stores it in outCor
system.time({
  for(i in 1:numBoot) outCor[i] <- cor(bootData[sample(1:bootCases, bootCases, rep = TRUE), ])[1, 2]
}) # 4.9 seconds

#### this type of problem is known as "embarrassingly parallel" - time to parallelize

# set number of processors or cores to use
nCore <- 4
# set how many bootstrap iterations to run on each core
rBoot <- numBoot/nCore

system.time({
  # register number of cores
    registerDoParallel(nCore)

  # foreach command is just like a for loop - using foreach with 
  # %dopar% uses parallel processing. In this example the i index
  # is not used in any subsequent function - needed to split across the four cores
    outCor <- foreach(i = 1:4) %dopar% {
        tmpCor <- vector('numeric', length = rBoot)
        for(j in 1:rBoot) {
            tmpData   <- bootData[sample(1:bootCases, bootCases, rep = TRUE), ]
            tmpCor[j] <- cor(tmpData)[1, 2]
        }
        tmpCor
    }
    # outCor is a list of length 4 each element containing a vector of correlations of 
    # length rBoot = 2500
    outCor1 <- unlist(outCor)
}) # 1.051 seconds - 


#### an alternative method of using parallelization
# could also do this way, but it is a little slower...
# the times argument is set to the total number of bootstrap iterations
# this code registers 4 cores, and then sends a single iteration to each
# multiple times... this increases the overhead and is thus slower than 
# splitting the number of bootstraps into four large iteration cycles
system.time({
registerDoParallel(4)
outCor <- foreach(times(numBoot)) %dopar% cor(bootData[sample(1:bootCases, bootCases, rep = TRUE), ])[1, 2]
}) #3.385 seconds

## other parallelization packages exist: snow, snowfall, doMC, multicore 
## each has their own way of initializing cores and handling parallel tasks

##############################################################################
############################################ Specialty Packages
# bigmemory package for handling biggish data
# bigmemory implements 3 types of matrix objects
#
# big.matrix -- R object limited by RAM
# shared.big.matrix -- R object limited by RAM but shareable across cores
# filebacked.big.matrix -- Data stored on disk and can be shared across cores. This is 
#                          object we will focus on
#
#
#
# For this demonstration we will use the dataset testPart.csv which contains 50,000
# cases on 11 numeric variables. The testFull.csv contains 6,000,000 cases on the same
# variables

# remove all objects to free up memory
rm(list = ls())

## read in the testPart.csv as a filebacked.big.matrix object
## 
## A couple of notes
## 1 - must supply a complete path to the dataset - this package
##     does not seem to use path.expansion by way of '~'
## 2 - type = 'double' - b/c this is a matrix object, you cannot have
##     multi-modal data. No characters and numeric in the same object. 
##     type is set based on C++ data types (double, integer, short, char)
## 3 - backingfile = the name for the file(s) to be stored on disk
## 4 - descriptorfile = the name of the file to hold the backingfile
##     description - allows quick read upon subsequent access

system.time({
    bmData <- read.big.matrix("Data/testPart.csv", 
                               type           = "double", 
                               header         = TRUE, 
                               backingfile    = "testPart.bin",
                               descriptorfile = "testPart.desc",
                               backingpath    = "Data",
                               shared         = TRUE)
}) # .337 seconds

# when using testFUll.csv this takes 50 seconds

dim(bmData)
head(bmData)

### after reading the file and storing the backingfile and descriptorfile
### the read time is crazy fast on subsequent reads

rm(bmData)

system.time({  bmData1 <- attach.big.matrix("Data/testPart.desc") }) # .001 seconds

# this took .005 seconds when using the testFull.csv - pretty huge time savings

# regression using biglm.big.matrix from biganalytics library

system.time({
  m1 <- biglm.big.matrix(X1 ~ X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = bmData1)
  summary(m1)
}) #.067

# when using testFull.csv this took 16.9 seconds 


# run a regression for each unique group case (1, 2, 3)
system.time({
  ms <- Map(function(x) biglm.big.matrix(X1 ~ X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, 
          data = bmData1[bmData1[, 'group'] == x, ]), x = 1:3)
}) # .07 seconds

# 9.9 seconds when using testFull.csv 

# run same series of regression models using parallel processing
microbenchmark({
    registerDoParallel(cores = 3)
    bmOut <- foreach(i = 1:3) %dopar% {
        biglm.big.matrix(X1 ~ X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, 
        data = bmData1[bmData1[, 'group'] == i, ])
}
}, times = 20, unit = 's') # .056 seconds

# 3.53 seconds when using testFull.csv

# bigmemory and its helper packages (biganalytics) provides a lot of useful
# tools for analyzing and managing data on disk rather than in memory. However,
# you are limited to a matrix of a single data type. The package ff overcomes this
# limitation.. however the documentation is very cryptic


#######################################################################
##################################### ff package
# Allows the access of data.frames stored on disk. Data stored in this way
# can be analyzed using biglm package like we did for bigmemory
# package ffbase extends many of base R commands to work with ff data objects

# we will be using ffTestPart.csv for this demonstration (the same file we used
# earlier with dplyr)


rm(list = ls())

# Check temporary directory ff will write to 
getOption("fftempdir")

# Set new temporary directory
options(fftempdir = "Data/tmp")

system.time(dff <- read.csv.ffdf(file   = "Data/ffTestPart.csv", 
                                 header = TRUE,
                                 colClasses = c(rep("double", 6), rep("factor", 2)))) 
# colClasses lets the system know what the variable types are. Character variables must
# be read in as factors

# this read took .759 seconds for 
# when using ffTestFull.csv this takes about 2 minutes (107.668 seconds). 
# after saving the object onto diskc, subsequent reads are much faster

save.ffdf(dff, dir = "Data/ffData", overwrite = TRUE)
# overwrite previous stored ffdf, .Rdata fileSnapshot

# remove the data that we just saved and reload using the saved ff data frame on disk
rm(dff)
system.time(load.ffdf(dir = "Data/ffData")) # .003 seconds
# .005 seconds for ffTestFull.csv 

# note the functions save.ffdf and load.ffdf are wrappers from the 
# ffbase package that make saving and loading ff objects easier

# Also note, if we modify the ffdf object via data cleaning or et cetera, 
# we need to RESAVE the object! Otherwise, our modifications will not be 
# stored in the permanent directory and will only exist for the duration of the R session.

# checkout what is in the data
str(dff[1:10, ])

# ffbase provides a table method for ffdf objects
table(dff$gender, useNA = 'always')


# regression
system.time(m <- bigglm.ffdf(X1 ~ X2 + X3 + X4 + X5 + X6, data=dff, na.action=na.exclude)) 
# .266 seconds
# when using ffTestFull.csv this takes 33.704 seconds

# notice that the function is bigglm.ffdf - this function can be used fit generalized linear models
# like logistic, poisson, etc 


# run a regression for each group membership = A, B, C

registerDoParallel(4)
system.time({
    ffOut <- foreach(i = c('A', 'B', 'C'), .inorder = FALSE) %dopar% {
      bigglm.ffdf(X1 ~ X2 + X3 + X4 + X5 + X6, data = dff[dff$group == i, ], na.action = na.exclude)    
    }
}) # .238 seconds
# when using ffTempFull.csv 15.844 seconds

# .inorder = FALSE can speed up the computation time.. assumes you don't need the computations
# done in the order specified in the foreach statement

### other handy base functions useable with ff
### any, is.na, subset

any(is.na(dff$X1))
dff1 <- subset(dff, !(is.na(X1) | is.na(X2)))

## check out ffbase for more base functions that can be used with ff data.frame objects

################################################################ EXTRA
################################################################ EXTRA 
################################################################ EXTRA 

#### Example of creating a sqlite database from a pre-existing CSV file
# create empty sqlite database
db <- dbConnect(SQLite(), dbname = "Data/testPartSQLite.sqlite")
# Note - if database already exists, running the above command will connect to it

# write the data from ffTestPart.csv into the sqlite database
# name = "test" sets the name of the table 
# overwrite = TRUE overwrites any preexisting table (in this case table 'test')
dbWriteTable(conn = db, name = "test", value = "Data/ffTestPart.csv", row.names = FALSE, header = TRUE, overwrite = TRUE)

# list the tables (think data.frame) contained within the database
dbListTables(db)
# list the variables (or column names) contained in the table 'test'
dbListFields(db, "test")

# Command not executed. This command below will attempt to read the entire database into R
# dbReadTable( db, 'test')

# determine number of cases in the test table in the database
nCases <- dbFetch(dbSendQuery(db, "SELECT Count(*) FROM test"))
nCases


########## Chunking data - splitting the data into manageable sizes
########## Example of reading in chunks of data, doing some type of
########## computation, and then aggregate the results

# set chunkSize to grab 10000 cases at a time
chunkSize <- 10000

# compute how many total chunks will be created
numChunks <- nCases/chunkSize; numChunks

# create empty vecotr to contain results of each computation on each chunk
outTable  <- vector('list', length = as.numeric(numChunks))

# create SQLite query to grab all the columns from the table "test"
# NOTE: SQLite databases index start at 0, not at 1
q1 <- dbSendQuery(db, "SELECT * FROM test")
i  <- 1

# run through the dataset in chunks
# this loop will complete once the database has been exhausted of cases
# by using the dbHasCompleted() function
while(!dbHasCompleted(q1)) {

    # this line executes the SQLite query for the first 10000 cases of the table test
    tmpDat <- dbFetch(q1, n = chunkSize)

    # silly example of a computation - tablulate the gender variable in chunks
    outTable[[i]] <- table(tmpDat[, 'gender'], exclude = c("NA", NA))
    i <- i + 1

}
dbClearResult(q1) # This command frees up all resources associated with the query contained in q1

# aggregate the tables into a single table
apply(do.call(rbind, outTable), 2, sum)


# quicker way to do the above
tg <- dbSendQuery(db, "SELECT gender, count FROM (SELECT gender, COUNT() AS count
FROM test GROUP BY gender)")
dbFetch(tg, n = -1)

# n = -1 says fetch all the data


####### We have essentially written a map reduce algorithm for 
####### tabulating chunks of data (the map part), 
####### and then aggregating the separate tables into a single table (the reduce part)

### Example of running a regression model on all the cases in the database
### the package speedglm allows a user to update an existing (general) linear model
### with new data. We can take advantage of this feature, and build a regression model
### using the chunking approach from above

q2        <- dbSendQuery(db, "SELECT * FROM test") # same query as before
startData <- dbFetch(q2, n = chunkSize) # data to begin the process

m1 <- speedlm(X1 ~ X2 + X3 + X4 + X5 + X6, data = startData)

while(!dbHasCompleted(q2)) {
    updateData <- dbFetch(q2, n = chunkSize)
    if(nrow(updateData) != 0) m1 <- updateWithMoreData(m1, updateData)
}
dbClearResult(q2)

summary(m1)

# close out the database connection
# dbDisconnect(db)

###########################
##########################################################################
##########################################################################
# don't run during session takes too long - here is the timing for 
# reading the testFull.csv data and doing a regression using base functions (
# so in RAM)
#

if(FALSE) {
    system.time(d <- read.csv("Data/testFull.csv")) #  about 5 minutes (5.34)
    
    system.time({
    
        m <- lm(X1 ~ X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = d)
        summary(m)
    
    }) # 7.724 seconds - pretty quick b/c the the data is loaded into RAM
    
    ### do three regressions - one for each value of group
    system.time({
        dsplit <- split(d, d[, 'group'])
          ms <- lapply(dsplit, function(x) lm(X1 ~ X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = x))
    }) # 10.834 seconds
    
    lapply(ms, summary)
    
    ### refactor as parallel task
    
    system.time({
            registerDoParallel(3)
            mOut <- foreach(i = 1:3) %dopar% {
                lm(X1 ~ X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = d[d[, 'group'] == i, ])
        }
    }) # 13.848 Notice its a bit slower - too much overhead
}


