
################################################################################
# Read in data
problem_data_day3_raw = read.table('/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day3/day3_input.txt', comment.char = "")
problem_data_day3 = problem_data_day3_raw
# convert symbols to zeroes/ones
problem_data_day3[,1] = sapply(problem_data_day3[,1], function(X) {str_replace_all(X, '\\.', '0')})
problem_data_day3[,1] = sapply(problem_data_day3[,1], function(X) {str_replace_all(X, '#', '1')})
# convert strings to matrix
problem_data_day3_matrix = as.matrix(t(as.data.frame(lapply(str_split(problem_data_day3[,1],''), as.numeric))))
rownames(problem_data_day3_matrix) = NULL

################################################################################
# now count for the (dx, dy) = (3, 1) case

# dimensions of matrix
nrow_matrix = nrow(problem_data_day3_matrix)
ncol_matrix = ncol(problem_data_day3_matrix)
# get all coordinates
y_coordinates = 1:nrow_matrix
x_coordinates = cumsum(c(1,rep(3, nrow_matrix-1)))
# convert coordinates to keep them within matrix, using periodic boundaries
x_coordinates_converted = x_coordinates %% ncol_matrix
x_coordinates_converted[x_coordinates_converted==0] = ncol_matrix # fix zeroes

# How many trees did we hit?
tile_hit_vector = problem_data_day3_matrix[cbind(y_coordinates, x_coordinates_converted)]
trees_hit = sum(tile_hit_vector)

# show result
print(paste0('Nr of trees hit: ', trees_hit))


################################################################################
# For multiple angles 

count_trees_fn = function(step) {
    # i'm a bit sloppy with not making the other stuff input arguments too
    # step = dx, dy
        
    dx = step[1]
    dy = step[2]
    
    # dimensions of matrix
    nrow_matrix = nrow(problem_data_day3_matrix)
    ncol_matrix = ncol(problem_data_day3_matrix)
    # get all coordinates
    y_coordinates = seq(1, nrow_matrix, dy)
    x_coordinates = cumsum(c(1,rep(dx, length(y_coordinates)-1)))
    # convert coordinates to keep them within matrix, using periodic boundaries
    x_coordinates_converted = x_coordinates %% ncol_matrix
    x_coordinates_converted[x_coordinates_converted==0] = ncol_matrix # fix zeroes
    
    # How many trees did we hit?
    tile_hit_vector = problem_data_day3_matrix[cbind(y_coordinates, x_coordinates_converted)]
    trees_hit = sum(tile_hit_vector)
    
    # show result
    #print(paste0('Nr of trees hit: ', trees_hit))
    
    return(trees_hit)

}

# Now for 
#Right 1, down 1.
count_trees_fn(c(1,1))
#Right 3, down 1. (This is the slope you already checked.)
count_trees_fn(c(3,1))
#Right 5, down 1.
count_trees_fn(c(5,1))
#Right 7, down 1.
count_trees_fn(c(7,1))
#Right 1, down 2.
count_trees_fn(c(1,2))

# or, requested output is
input_list = list(c(1,1), c(3,1), c(5,1), c(7,1), c(1,2))
requested_output = prod(sapply(input_list, FUN = count_trees_fn))
requested_output













