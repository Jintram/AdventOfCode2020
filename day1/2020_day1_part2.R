# Advent of code 2020, problem 2

problem_data = read.table('/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/input_problem1.txt')[[1]]

# Now, the question is which three numbers add to 2020

# Again, there's no way around just trying everything

# adjusted looping to go over sets of three
found=F
for (idx1 in 1:(length(problem_data)-2)) {
 
    for (idx2 in (1+idx1):(length(problem_data)-1)) {
        
        partial_sum = (problem_data[[idx1]]+problem_data[[idx2]])
        
        if (partial_sum < 2020) { # assuming all numbers >0
            
            for (idx3 in (2+idx1):length(problem_data)) {
                
                if ((partial_sum + problem_data[[idx3]])==2020) {
                
                    found = T
                    break   
                    
                }
            }
                
            if (found) {break}
        }
        
    }
    
    if (found) { break }
       
}

# Report
if (found) {
    print(paste0('The 3 numbers are: ',problem_data[[idx1]], ' and ', problem_data[[idx2]], ' and ', problem_data[[idx3]]))
    print(paste0('The 3 indices are: ',idx1, ' and ', idx2, ' and ', idx3))
    print(paste0('Output: ', problem_data[[idx1]]*problem_data[[idx2]]*problem_data[[idx3]]))
} else {
    print('No triplet adds to 2020')    
    print(paste0('Output',NA))
}
