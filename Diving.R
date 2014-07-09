# The Main Diving Script
#
# I provide this version of the script for use starting
# Wednesday July 9.  I will expand it through the rest of
# this case study.  It is a reduced version of what we
# did Monday/Tuesday, but I have removed some things.

###################################
# Special note for Wednesday July 9
#
# If you get to class early and download this file, please do
# not read ahead too far: read only this section.  If you have
# extra time, please review things we did in the last two days
# and prepare a question to ask me at the start of class.
#
# Thank you!
#
# See English.R and ToyExamples.R
#
# Please: if you know (from reading my paper) or think you have
# figured out a good way of studying nationalistic bias, please do
# not announce it to the class.  This will spoil the experience for
# the other students.  

# I want to give you lots of chances today to practice working
# in R.  For many of you, this is most important.  But I also
# want to provide a real data challenge that is interesting for
# those of you experienced with R.  If you think you are good with
# R, I challenge you to seek multiple solutions to the same
# problem.  Decide which you like, and why?  Explain this to
# someone else.  Argue with them.  If they are still learning,
# teach them.
#
# Other aspects of the problem are statistical in nature.  Something
# may seem easy or obvious.  It may be.  It may not be.  Keep an open
# mind, and challenge your own assumptions.  Real data analysis is
# different from pure mathematical statistics.
#
# End special note for Wednesday
###################################

x <- read.csv("Diving2000.csv", as.is=TRUE)
dim(x)

plot(jitter(x$Difficulty), jitter(x$JScore),
          col=factor(x$Round),
               xlab="Degree of Difficulty",
                    ylab="Judges' Scores")

# Create the matching variable; recall this will not work
# if the country variables are factors.
x$match <- x$Country == x$JCountry
table(x$match)

# Let's create a variable for use that will be
# convenient to have for efficient exploration:
thisjudge <- "WANG Facheng"

# A series of calculculations, perhaps useful, perhaps not:
mean(x$JScore)
mean(x$JScore[x$Judge==thisjudge & x$match])
mean(x$JScore[x$Judge==thisjudge & !x$match])
mean(x$JScore[x$JCountry != "CHN" &
                   x$Country == "CHN"])

## Challenge: figure out the nationality
##            of 'thisjudge' in a general way.

thisjudge <- "WANG Facheng"
thiscountry <- x$JCountry[x$Judge==thisjudge][1]       # One way
thiscountry <- unique(x$JCountry[x$Judge==thisjudge])  # Another way
if (length(thiscountry) != 1 ) print("Judge country problem!") # Sanity check

## Challenge: the check, above, is not sufficient to "catch" all
## possible errors (unless you make some specific assumption).
## Why?  What could go wrong that we could also try to "catch"?

# How many "matching" dives do we have for thisjudge?
sum(x$Judge==thisjudge & x$match)     # One solution
table(x$Judge, x$match)[thisjudge, 2] # Another solution.  CONSIDER!

# Can we establish "bias" for a judge?  Perhaps this is closer:
t.test(x$JScore[x$Judge==thisjudge & x$match],
              x$JScore[x$Judge!=thisjudge & x$Country==thiscountry])

## Challenge: the test above isn't great.  Why?  Find some aspect
## of the problem relating specifically to Judge Wang and to things
## we discussed in our diving exploration that causes a problem for
## this approach.  There may be many problems, of course.

### STOP.  Write down one problem on paper (in English) in a very
### short paragraph.  Show it to another student.  Read theirs.
### Discuss the problem.  Discuss the English.

# Jake: Is T-test suitable for this problem?
# Find the relationship between Round and match
table(x$Round, x$match)

## Challenge: identify all dives in the data set where the diver
## is from 'thiscountry' (perhaps China) and the judge is 'thisjudge'
## (Judge Wang in our case).  Create a reduced data set for only
## these dives.  Create a new data.frame 'y' containing only these
## dives.

#### Your solution should go here:

#### End of your solution.  Is it nicely indented and spaced?
#### Did you use <- for assignments?  Would I be impressed
#### if I looked at your code?

### FOR THE CHALLENGE IMMEDIATELY ABOVE:
### 'y' should have 22*7 rows for Judge Wang.

### STOP.  Without 'y' from above, you can't continue below. ###

# Once we have this reduced data set, y, we might consider
# the following test:
t.test(y$JScore[y$match], y$JScore[!y$match])  # TODO

## CHALLENGE: describe exactly and precisely the two sets of scores
## being compared in the t-test, above.  Note that it requires
## considering how 'y' was extracted from 'x' in the earlier challenge.

### STOP.  Make very sure you are right, above, before continuing. ###
###        Talk with each other!  Argue!  Debate!                  ###

## CHALLENGE: How do you feel about this test?  Do you like it?  Is
## Is it fair to Judge Wang?  Is it effective at uncovering biased
## judging (if present)?

### STOP.  Write down your answer on paper (in English) in a very
### short paragraph.  Show it to another student.  Read theirs.
### Discuss the problem.  Discuss the English.

#################################################################
## POSSIBLE CHALLENGE: you may feel that you know how to solve
## this problem of assessing nationalistic bias.  Maybe you are
## right.  Maybe not.  I'm not going to tell you now.
##
## But one question that students often ask is
## whether a t-test is appropriate for this problem.  What do you
## think?  If you don't like the t-test, propose a better choice.
## Be ready to defend your method.
#################################################################


