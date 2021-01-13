
library(stringr)

# read in raw data file
problem_data_day2_raw = read.table('/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day2/day2_input.txt')

# process data frame
problem_data_day2 = as.data.frame(t(data.frame(strsplit(problem_data_day2_raw[,1], '-'))))
problem_data_day2[,1] = as.numeric(problem_data_day2[,1]) # not doing this led to an undetected bug as chars were wrongly parsed to numericals
problem_data_day2[,2] = as.numeric(problem_data_day2[,2])
rownames(problem_data_day2) = NULL
colnames(problem_data_day2) = c('range_start','range_end')
problem_data_day2$symbol = str_replace( problem_data_day2_raw[,2], ':', '' )
problem_data_day2$pwd = problem_data_day2_raw[,3]

############################################################
# Part A: find valid passwords

# First determine counts
problem_data_day2$counts = sapply(1:length(problem_data_day2$pwd), FUN = function(X) {str_count(string = problem_data_day2$pwd[X], pattern = problem_data_day2$symbol[X])})

# Then which ones are valid
problem_data_day2$valid = (problem_data_day2$counts>=problem_data_day2$range_start) & (problem_data_day2$counts<=problem_data_day2$range_end)

# Generate requested output
nr_valid_pwds = sum(problem_data_day2$valid)
print(paste0('The number of valid passwords is: ', nr_valid_pwds))

############################################################
# Part B: find valid passwords according to constraints 2

# Note that "range_start" and "range_end" now play the role 
# of "position 1" and "position 2"
problem_data_day2$pos1 = problem_data_day2$range_start
problem_data_day2$pos2 = problem_data_day2$range_end

# Determine validity
problem_data_day2$valid_B = sapply(1:length(problem_data_day2$pwd), FUN = 
        function(X) {
            1==
                (str_sub(string = problem_data_day2$pwd[X], start = problem_data_day2$pos1[X], end = problem_data_day2$pos1[X])==problem_data_day2$symbol[X])+
                (str_sub(string = problem_data_day2$pwd[X], start = problem_data_day2$pos2[X], end = problem_data_day2$pos2[X])==problem_data_day2$symbol[X])
            }
    )

# Return answer
nr_valid_pwds_B = sum(problem_data_day2$valid_B)
print(paste0('The number of valid passwords is: ', nr_valid_pwds_B))



############################################################
# Alternative for part A 

count=0
for (idx in 1:length(problem_data_day2$pwd)) {
    
    the_count = str_count(problem_data_day2$pwd[idx], problem_data_day2$symbol[idx])
    if (the_count >= problem_data_day2$range_start[idx] & the_count <= problem_data_day2$range_end[idx]) {
        count=count+1   
    }
    
}
count



