# Advent of code 2020, problem 1a and 1b

# Read in data
problem_data = read.table('/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day1/input_problem1.txt')[[1]]

# In case we want to check all combination per se, 
# this is more optimal and concise:

# (Part A) For the sets of two
all_combinations = combn(problem_data, 2)
relevant_sets = all_combinations[,apply(all_combinations, 2, sum)==2020, drop=F]
requested_output = apply(relevant_sets, 2, prod)
print(paste0('The answer is ', requested_output))

# (Part B) For the sets of three
all_3way_combinations = combn(problem_data, 3)
relevant_3way_sets = all_3way_combinations[,apply(all_3way_combinations, 2, sum)==2020, drop=F]
requested_output_3way = apply(relevant_3way_sets, 2, prod)
print(paste0('The answer for threeway sets is ', requested_output_3way))
