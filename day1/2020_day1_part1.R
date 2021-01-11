# Advent of code 2020, problem 1

problem_data = read.table('/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/input_problem1.txt')[[1]]

# I'm guessing there's no way to know which pair 
# is more likely to add to 2020.
#
# So we'll just have to try all
# Note looping is such that all pairs are considered once
#
# Note: I'm using simple loops, more advanced
# iteration might be more clean.
# 
found=F
for (idx1 in 1:(length(problem_data)-1)) {
 
    for (idx2 in (1+idx1):length(problem_data)) {
        
        if ((problem_data[[idx1]]+problem_data[[idx2]]) == 2020) {
            found = T
            break   
        }
        
    }
    
    if (found) { break }
       
}

# Report
if (found) {
    print(paste0('The two numbers are: ',problem_data[[idx1]], ' and ', problem_data[[idx2]]))
    print(paste0('The two indices are: ',idx1, ' and ', idx2))
    print(paste0('Output: ', problem_data[[idx1]]*problem_data[[idx2]]))
} else {
    print('No pair adds to 2020')    
    print(paste0('Output',NA))
}


