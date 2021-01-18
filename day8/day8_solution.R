
# Read in file
filePath ='/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day8/day8_input.txt'
data_instructions = read.table(filePath, sep = ' ')
names(data_instructions) = c('instruction','value')

# Non-jump instructions can be viewed as +1 jump instructions
data_instructions$jump = data_instructions$value
data_instructions$jump[!(data_instructions$instruction=='jmp')] = 1
# Convenient to separate accumulator values also
data_instructions$value_acc = 0
data_instructions$value_acc[data_instructions$instruction=='acc'] = data_instructions$value[data_instructions$instruction=='acc']

# Function to determine path along instructions
get_path = function(jumps, return_what = 'locations') {
    
    # Set up some stuff
    nr_instructions = length(jumps)
    counter=0 # just to prevent real inf loops
    current_location=1; location_history = c() # initial values
    visited = rep(FALSE, length(jumps)) # Create tracking of already visited
    
    # go over the locations
    while (!visited[current_location] & current_location < nr_instructions & counter <= 1e6) {
        
        counter=counter+1 
        
        location_history = c(location_history, current_location)
        
        visited[current_location] = T # track where we've been
        
        current_location = current_location + jumps[current_location] # update location
    
        
    }
    
    if (counter == 1e6) {
        stop('Infinite loop protection kicked in.')
    }
    
    # Return either location history (ie path) or whether we've reached
    # the end with these jump instructions
    if (return_what == 'locations') {
        return(location_history) 
    } else if (return_what == 'end_reached') {
        return(current_location == nr_instructions)
    } else {
        stop('improper \'return_what\' value')   
    }
}

# Get path
location_history = get_path(data_instructions$jump, return_what = 'locations')

# Calculate acc value given that path
output_value_instructions = sum(data_instructions$value_acc[location_history])

################################################################################
# PART B
# I wonder whether there's a smart way to do this (i.e. deduce in one step
# what the correct answer is).
#
# The problem is that the path is not so trivial, since if there's a jump, 
# the future path can change drastically
#
# Could we go in the reverse direction? No, because it's impossible
# to know the past path with only knowledge of the current location.

# I guess we need to make a matrix with all permutations and just try
# Columns are the jump instructions
# Rows are attempted fixes
jump_matrix = matrix(rep(data_instructions$jump, length(data_instructions$jump)), nrow=length(data_instructions$jump))
jump_matrix[col(jump_matrix)==row(jump_matrix)] = 1

# Now find which one will reach the end
corrected_jumps_idx = which(apply(jump_matrix, 2, get_path, return_what='end_reached'))

# And get location path for that one to calculate acc values
location_history_corrected = get_path(jump_matrix[,corrected_jumps_idx], return_what = 'locations')
output_value_instructions_corrected = sum(data_instructions$value_acc[location_history_corrected])


