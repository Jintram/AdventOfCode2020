

# Load data
Data_Day9 = read.table('/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day9/day9_input.txt')
Data_day9_array = Data_Day9$V1

# Now the first question to solve is: what is the first number that is NOT the sum of
# one of the 25 previous numbers
# 
# We can of course just generate all combinations of the previous 25 numbers
# for each number, and loop over those asking that question.
# 
# Can it be done slightly smarter?
# Yes, I guess when shifting to the next number to test, 
# this removes all pairs with the n-26th number, and 
# introduces new pairs with the n-1 number.
# 
# Also, the number of pairs is (N*(N-1))/2
# so it would be nice to reduce the duplicity 
# of pairs
# 
# This could be done by creating a list of the relevant sums, 
# and then each iteration only calculating the new sums,
# and removing the sums that are no longer relevant
# from a list that keeps getting updated.

# OK, let's just first do it brute-force

# Create the combinations of interest
indices_of_interest = -25:-1
#pairs_of_interest   = expand.grid(indices_of_interest,indices_of_interest) # this leaves in the redundant ones
pairs_of_interest_as_dn = combn(indices_of_interest, 2)

brute_force_solve = function(input) {
    for (idx in 26:length(input)) {
        
        # calculate the sums (note idx+X[1] has a plus since X[i] is negative already)
        #all_pairs = apply(pairs_of_interest_as_dn, 2, function(X) {input[c(idx+X[1], idx+X[2])]})
        all_sums = apply(pairs_of_interest_as_dn, 2, function(X) {sum(input[c(idx+X[1], idx+X[2])])}) 
        
        if (!any(all_sums==input[idx])) {
            print(paste0('The ', idx, 'st number doesnt comply (',input[idx],')'))
            break
        }
        
    }
}

brute_force_solve(Data_day9_array)
# Output: "The 512st number doesnt comply (18272118)"

# Part A solved :)

################################################################################
# Now a speedier version for part A

# new pairs are any involving the -1 index
new_pairs = pairs_of_interest_as_dn[,apply(pairs_of_interest_as_dn, 2, function(X) {any(X==-1)})]
# old pairs are any involving the -25 index
new_pairs = pairs_of_interest_as_dn[,apply(pairs_of_interest_as_dn, 2, function(X) {any(X==-25)})]
# The new pairs are simple, that's just the new number combined 
# with any of the other n-25 numbers.
# The pairs to remove will be scathered a bit more
# The pattern can be predicted tough...
# 
# If I add all new combinations in a certain pattern, that's 
# -25 w/ -24:-1
# Given I've removed the appropariate numbers already before, that means I'll have
# sequences that have shrunken a bit each iterations, so

# quickly make a simple example / find out relevant indices
MAX=25
indices_of_interest2 = 1:MAX
new_to_add = matrix(c(rep(1, MAX-1), 2:MAX), nrow=2, byrow = T)
pairs_of_interest_as_dn2 = combn(indices_of_interest2, 2)
idx_to_keep = !apply((pairs_of_interest_as_dn2+1)==MAX+1,2,any)
# sanity check
updated_pairs = cbind(new_to_add, pairs_of_interest_as_dn2[,idx_to_keep]+1)

# Great, this should work
check_m_quick = function(input) {
    
    # first loop manually to initialize
    idx=26
    all_sums = apply(pairs_of_interest_as_dn2, 2, function(X) {sum(input[c(idx-X[1], idx-X[2])])}) 
    
    if (!any(all_sums==input[idx])) {
        print(paste0('The ', idx, 'st number doesnt comply (',input[idx],')'))
        return(idx)
    }
    
    # then continue
    for (idx in 27:length(input)) {
        
        # calculate the sums 
        all_sums = c(apply(new_to_add, 2, function(X) {sum(Data_day9_array[c(idx-X[1], idx-X[2])])}),
                     all_sums[idx_to_keep])
        
        if (!any(all_sums==input[idx])) {
            print(paste0('The ', idx, 'st number doesnt comply (',input[idx],')'))
            return(idx)
        }
        
    }
  
}
check_m_quick(Data_day9_array)


# Now compare whether it's indeed faster
timer_brute = system.time(brute_force_solve(Data_day9_array))
timer_quick = system.time(check_m_quick(Data_day9_array))
timer_brute[3]/timer_quick[3]
# 8.5x as fast for this particular run
# we spare (24*23)/2=276 pairs of 300 each run, so 300/(300-276), so theoretically could 've been 12.5x faster.

################################################################################
# Now for part B

# Now we take the output of the previous number, 
# 18272118
# 
# and we need to find a series of subsequent numbers
# in the data input that sums to this number

# My naive strategy would be to set a search window starting at n1:n2, n1=1, n2=2,
# increasing n2 until either sum(n1:n2) == 18272118 or sum(n1:n2) > 18272118
# at which point the search window is re-evaluated by:
# a) n1 = n1+1
# b) then, if still to large (because of new nr) n2 is looped back such that sum falls below 18272118,
# c) if sum(n1:n2) still doesn't equal 18272118, increase n1 and go back to increasing n2
find_series = function(input=Data_day9_array) {
    # I explicitly update the sum as done here (as opposed to calculating sum(input[n1:n2]) all the time
    # to make the code quicker
    
    count=0
    n1=1
    n2=2
    current_sum = sum(input[n1:n2])
    while(T) {
        count=count+1; if (count>1e6) {stop('Infinite error.')}
        
        # if this is the correct series, stop
        if (current_sum==18272118) { return(c(n1,n2)) }
        
        # if sum is too high, increase n1
        if (current_sum>18272118) {
            
            current_sum = current_sum-input[n1]
            n1=n1+1
            if (current_sum==18272118) { return(c(n1,n2)) }
            
            # drop down if necessary
            if(current_sum>18272118) {
                while(n2>n1) {
                    current_sum = current_sum-input[n2]
                    n2=n2-1
                    if (current_sum==18272118) { return(c(n1,n2)) }
                } 
                # if series isn't here, it won't exist with this n1
                current_sum = current_sum-input[n1]
                n1=n1+1
                if (current_sum==18272118) { return(c(n1,n2)) }
            }
            
            }
        
        # sum is still too low, expand search window
        n2=n2+1
        current_sum = current_sum+input[n2]
        
    }
}

# find the solution
solution_indices = find_series(Data_day9_array)
solution_indices # = 395 411

# check outcome
sum(Data_day9_array[395:411])==18272118 # TRUE
# add smallest and largest number in this range
min(Data_day9_array[395:411])+max(Data_day9_array[395:411])
# final solution = 2186361 (for my puzzle)







