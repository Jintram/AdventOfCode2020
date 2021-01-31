################################################################################
# Advent of Code, day 11
# Filling seats 

# Import the data and convert to matrix
day11_data_raw = read.table('/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day11/day11_input.txt')$V1
#day11_data_raw = read.table('/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day11/day11_input_test.txt')$V1
seat_layout_matrix = t(sapply(day11_data_raw, function(X) {  t(sapply(strsplit(X, ''), function(Y) {'L' == Y}))  }))#*-1
seat_layout_matrix_padded = matrix(rep(0, prod(dim(seat_layout_matrix)+2)), nrow=dim(seat_layout_matrix)[1]+2)
seat_layout_matrix_padded[-c(1,nrow(seat_layout_matrix_padded)),][,-c(1,ncol(seat_layout_matrix_padded))] = seat_layout_matrix

# Rules:
# empty and 8 surrounding empty --> becomes occupied
# occupied and >4/8 surrounding occupied --> becomes empty
# otherwise, no change

# Note:
# array padding seems non-standard in R
# solution: expand "seat matrix" with ground tiles
# on each side (anyways perhaps even more elegant)

# We need some library that allows convolution
#library(OpenImageR) # convolution(image, kernel, mode = "same")
#library(magick) # image_convolve
library(imagine) # convolution2D



# Proposed strategy:
# - Apply kernel until convergence
# - Ignore empty seats, but ..
#   After each application, make empty seats empty again 


kernel_surrounding_seats = matrix(c(1,1,1,1,0,1,1,1,1), nrow=3)

# Initially, all can be filled, all zeroes
current_configuration = matrix(rep(0, prod(dim(seat_layout_matrix_padded))), nrow=dim(seat_layout_matrix_padded)[1])
#current_configuration = seat_layout_matrix_padded

flag_converged=F
for(idx in 1:1e6) {
 
    # print(paste0('loop ',idx))
    
    # Save to check convergence
    previous_configuration = current_configuration
    
    # Then, apply kernel for filling
    surr_ppl_count = convolution2D((current_configuration), kernel=kernel_surrounding_seats)
    # Which squares contain no people aruond it, and are currently empty
    seats_to_fill  = surr_ppl_count==0 & current_configuration==0 & seat_layout_matrix_padded
    # Which squares have 4 people around it, and are currently filled
    seats_to_empty = surr_ppl_count>=4 & current_configuration==1 & seat_layout_matrix_padded # (last term is redundant)
    
    # Execute
    current_configuration[seats_to_fill]  = 1
    current_configuration[seats_to_empty] = 0
    
    # check convergence
    if (all(previous_configuration == current_configuration)) {
        print('Yay! Conversion..')
        flag_converged=T
        break
    }
       
}

total_occupied = sum(current_configuration)
if (flag_converged) {
    print(paste0('Total occupied seats currently: ',total_occupied))
} else {
    print('Convergence didn\'t occur..')   
}

################################################################################
# PART B

# Load the data again
# Import the data and convert to matrix
day11_data_raw = read.table('/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day11/day11_input.txt')$V1
#day11_data_raw = read.table('/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day11/day11_input_test.txt')$V1
seat_layout_matrix = t(sapply(day11_data_raw, function(X) {  t(sapply(strsplit(X, ''), function(Y) {'L' == Y}))  }))#*-1
seat_layout_matrix_padded = matrix(rep(0, prod(dim(seat_layout_matrix)+2)), nrow=dim(seat_layout_matrix)[1]+2)
seat_layout_matrix_padded[-c(1,nrow(seat_layout_matrix_padded)),][,-c(1,ncol(seat_layout_matrix_padded))] = seat_layout_matrix


# I think this involves a bit more custom code, since it's not trivial 
# which other seats can be seen from each seat

current_index = c(2,2)
#seat_list=matrix(rep(NA,16),nrow=2)
seat_list=list()

directions = matrix(c(-1,0, -1,1, 0,1, 1,1, 1,0, 1,-1, 0,-1, -1,-1), nrow=2)

# return a list of indices that represent which seats can be seen from there
seats_seen_from_here = function(input_seat) {#input_seat=c(2,2)
    
    seat_list=matrix(rep(NA,16),nrow=8)
    
    # for 8 directions
    for (idx in 1:8) {
        
        current_location=input_seat
        
        # move from location in given direction until seat found or border is hit (note: padded)
        while (T) {
            
            current_location = current_location  + directions[, idx]
            
            if (seat_layout_matrix_padded[t(current_location)]==1 | 
                    current_location[1]==1|current_location[1]==nrow(seat_layout_matrix_padded)|
                    current_location[2]==1|current_location[2]==ncol(seat_layout_matrix_padded)) {
                seat_list[idx,] = current_location
                break
            }
            
        }
    }
    return(seat_list)
}

# Initially, no seat is occupied
current_configuration = matrix(rep(0, prod(dim(seat_layout_matrix_padded))), nrow=dim(seat_layout_matrix_padded)[1])

# get indices of seat locations
seat_locations_x_y = arrayInd(which(seat_layout_matrix_padded==1), .dim=dim(seat_layout_matrix_padded))

# get list of which seats can be seen from those location, per seat
seats_to_check_per_seat = lapply(as.data.frame(t(seat_locations_x_y)), seats_seen_from_here)

for(i in 1:1e3){
    
    old_configuration = current_configuration
    
    # now do as before, but 'manually' per seat, using locations, and known "related chairs"
    to_fill_bool = sapply(1:sum(seat_layout_matrix_padded), function(X) {
            # if seat is empty, and surrounding ones to, mark for filling
            if (  current_configuration[t(seat_locations_x_y[X,])]==0
                & sum(current_configuration[seats_to_check_per_seat[[X]]])==0 ) {T} else {F}
        })
    too_empty_bool = sapply(1:sum(seat_layout_matrix_padded), function(X) {
            # if seat is filled, and >=5 surrounding seats are filled, mark for emptying
            if (  current_configuration[t(seat_locations_x_y[X,])]==1
                & sum(current_configuration[seats_to_check_per_seat[[X]]])>=5 ) {T} else {F}
        })
    
    # execute
    current_configuration[seat_locations_x_y[to_fill_bool,]]   = 1 
    current_configuration[seat_locations_x_y[too_empty_bool,]] = 0 

    if (all(old_configuration==current_configuration)) {print('convergence!'); break}
    
}

if(i==1:1e6) {print('Loop maxed out!')}

sum(current_configuration)


