# What is tidy data?
# 1) Each variable must have its own column.
# 2) Each observation must have its own row.
# 3) Each value must have its own cell.


### Exercise: improving messy data

#1) Describe five things about this data that are not tidy and how you could fix
#each of those issues.

# - 3 tables for each species when these could be combined to one table with species as a column
# - Weight variable contains information about the metric (grams) and value, these should be separated
# - Weight variable also contains some information about scale calibration
# - The tables do not contain identical information
# - The data are weirdly centered in the excel file
 
#2) Could this data easily be imported into a programming language or a database
#   in its current form?
# - Nope.  


#3) Do you think it’s a good idea to enter the data like this and clean it up later,
# or to have a good data structure for analysis by the time data is being entered? Why?

 # - Its better to have a good data structure in the first place. Raw data shouldn't be edited