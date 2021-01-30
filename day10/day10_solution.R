
################################################################################
# Advent of code 2012, day 10, part A

day10_input = read.table('/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day10/day10_input.txt')$V1
#day10_input = read.table('/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day10/day10_example2_input.txt')$V1
day10_input = c(0, day10_input, max(day10_input)+3) # add the outlet and your built-in adapter 

# the device has a built-in adapter w/ 3 j higher output than the highest adapter available from the input list

# the final question for A is what are the differences if we chain all
# so now the furst question is: can we make a chain that connects all?

# seems we need to connect them in ascending order
day10_input_ordered = day10_input[order(day10_input, decreasing = F)]

# let's see if we can make array
differences = day10_input_ordered[2:length(day10_input_ordered)]-day10_input_ordered[1:(length(day10_input_ordered)-1)]
if (!all(differences<=3)) {
    stop('we can\'t make an array of adapters, differences too large.')
} else { print('Proceeding') }

# this seems pretty simple
frequency_table = table(differences)

# Final answer:
print(paste0('The final answer is: ', prod(frequency_table)))

################################################################################
# Part B

# Now we need to find all possible chains
# 
# Perhaps easiest to simply expand the range of possibilities by
# taking out possiblilities one at a time

# initial allowed chain
allowed_chains = list(day10_input_ordered)
latest_chains = allowed_chains

while(any(allowed_out)) {
    
    # MAKE A FOR LOOP HERE OVER LATEST CHAINS
    
    # stats on current chain
    current_chain = latest_chains[[idx]]
    differences = current_chain[2:length(current_chain)]-current_chain[1:(length(current_chain)-1)]
    
    # let's identify what can be taken out
    # these are new differences for element i if element i would be taken out
    differences_new = c(Inf,differences[1:(length(differences)-1)]+differences[2:(length(differences))],Inf)
    allowed_out = which(differences_new<=3)
    
    latest_chains = lapply(allowed_out, function(X) {current_chain[-X]})
    allowed_chains = c(allowed_chains, latest_chains)
    
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # So it's not so super straightforward, because we'll easily create redundant lists..
    # it's probably better to just 
    # a) decide which adapters can connect to which higher ones
    # b) start building the tree using those rules
    # Perhaps this can even be done with Noreen's matrix approach
    # also applied in the previous tree-building challenge..
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
}

# 

################################################################################
# PART B
# Solution inspired by Noreen's solution to a previous problem
# (The other one with creating trees)

# Create a connectivity matrix
# (Note that each "joltage" is unique, ie there are no adapters double, which is necessary for this approach)
connectivity_matrix = 1*sapply(day10_input_ordered, function(X) { day10_input_ordered <= X+3 & day10_input_ordered > X})

# just visualize this system for sanity check purposes
names(day10_input_ordered) = 1:length(day10_input_ordered) # for convenience, give adapters names
day10_input_ordered
library(pheatmap)
rownames(connectivity_matrix) = 1:dim(connectivity_matrix)[1]
colnames(connectivity_matrix) = 1:dim(connectivity_matrix)[2]
pheatmap(connectivity_matrix, cluster_cols = F, cluster_rows = F, )

# Now go over possibilities
# 
# Each loop (vector=Matrix*vector) updates
# the vector such that it represents all connections
# that are possible for the "next step" in the chain.
#
# If multiple connections were made to that 
# adapter, because matrix multiplication involves
# sum in one dimension, those are added.
#
current_state = c(1, rep(0, length(day10_input_ordered)-1)) # End-points of growing adapter-chains
total_arrangements = 0
for (idx in 1:(dim(connectivity_matrix)[2])) {
    
    # Update vector with end-points of adapter-chains
    current_state = connectivity_matrix%*%current_state #; current_state
    
    # Collect the # of chains that made it to the end
    total_arrangements = total_arrangements + current_state[length(current_state)]#; total_arrangements
    
}

print(paste0('Total arrangements possible are: ',total_arrangements))

















