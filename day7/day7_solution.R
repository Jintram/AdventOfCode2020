##################
# Advent of Code, day 7

library(stringr)

# First read in the rules
filePath = '/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day7/day7_input.txt'
#filePath = '/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day7/day7_input_test_A.txt'
#filePath = '/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day7/day7_input_test_B.txt'
con = file(filePath, "r")
rules_raw = readLines(con)
close(con)
# Create more convenient format
the_rules = do.call(c, lapply(rules_raw, function(X) { #for (X in rules_raw) {
    
    # split into (1) which bag type (2) what it may contain
    split1=str_split(X, pattern=' bags contain ')[[1]]
    
    # if (2) is "no other" create NA
    if (split1[2] == 'no other bags.') {
        
        out_list = list(c('none'=NA))
        names(out_list) = split1[1]
        return(out_list)
        
    # else process further, use regex to extract both the # and the colors and put in list output
    } else {
    
        bag_types  = str_extract_all(split1[2], ' [a-z ]+')
        bag_types = sapply(bag_types, function(Y) {sub(Y,pattern = '^ ',replacement = '')})
        bag_types = sapply(bag_types, function(Y) {sub(Y,pattern = ' bag(s|)$',replacement = '')})
        quantities = as.numeric(str_extract_all(split1[2], '[0-9]+')[[1]])
        
        names(quantities) = bag_types
        
        out_list = list(quantities)
        names(out_list) = split1[1]
        return(out_list)
    }
    
}))
# Also create simplified rules w/o the counts
simplified_rules = lapply(the_rules, function(X) {names(X)})

# This is again a combinatorial problem, with the caveat
# that there might be ever-repeating loops.
# 
# So we'll just write a function that creates
# this tree for each bag.

# function to generate a "tree of bags"
# outputs list, where each element
# consists of vector of colors that represents unique branch
calculate_tree_for_bag = function(start_bag) {
    
    finished_tree = list()
    the_tree = c(start_bag) # redundant but for clarity
    
    overflow_counter=0
    while(length(the_tree) > 0) {
        
        overflow_counter=overflow_counter+1
        
        new_tree=list()
        for (branch in the_tree) {
            
            # for this branch, which elements can last item hold?
            new_elements = simplified_rules[[branch[length(branch)]]]
            
            # if we see a repetition, move the branch to "finished tree"
            # if we see a 'none', indicates end of branch, also move to "finished tree"
            repeating_and_end_elements_idx = new_elements %in% branch | new_elements == 'none'
            if (any(repeating_and_end_elements_idx)) {
                finished_tree = c(finished_tree, lapply(new_elements[repeating_and_end_elements_idx], function(X){ c(branch, X) }))
            }
            
            # expand the tree with those elements that should still be expanding
            new_tree = c(new_tree, lapply(new_elements[!repeating_and_end_elements_idx], function(X){ c(branch, X) }))
        
        }
        the_tree = new_tree
    
        if (overflow_counter > 1000) {
           stop('Reached max nr of levels, aborted to prevent infinite loop.') 
        }
        
    }
    
    return(finished_tree)
    
}

# Just to test the function:
my_tree = calculate_tree_for_bag('shiny gold')

# Now solve the problem using the above ability to calculate tree
# ===

# First calculate all trees
my_forest = lapply(names(simplified_rules), calculate_tree_for_bag)

# We don't want to include the shiny gold bag itself
shiny_gold_branch_idx = which(names(simplified_rules)=='shiny gold')

# Then determine for each of the trees whether they contain a shiny gold bag.
# (If I had tailored above function better, I could have done this
# immediately in the above function, not storing this whole "forest", saving some memory)
can_hold = sapply(my_forest[-shiny_gold_branch_idx], function(tree) {
                any(sapply(tree, function(branch) {'shiny gold' %in% branch}))
            })

sum(can_hold)




################################################################################
# Part B

# This just requires slight modifications

# Same function as above, but now also tracking the duplicity of each branch
calculate_tree_and_duplicity_for_bag = function(start_bag) {
    
    # Where we'll collect final output
    finished_tree = list()
    finished_duplicity_tree = list()
    
    # These will hold branches that are still growing
    the_tree = c(start_bag) # redundant but for clarity
    duplicity_tree = c(1) # will track the duplicities
    
    overflow_counter=0
    while(length(the_tree) > 0) {
        
        overflow_counter=overflow_counter+1
        
        new_tree=list()
        new_duplicity_tree=list()
        for (branch_idx in 1:length(the_tree)) {
            
            branch = the_tree[[branch_idx]]
            duplicity_branch = duplicity_tree[[branch_idx]]
            
            # for this branch, which elements can last item hold?
            new_elements = simplified_rules[[branch[length(branch)]]]
            new_duplicities = the_rules[[branch[length(branch)]]] 
            
            # if we see a repetition, move the branch to "finished tree"
            # if we see a 'none', indicates end of branch, also move to "finished tree"
            end_elements_idx = new_elements == 'none'
            if (any(repeating_and_end_elements_idx)) {
                finished_tree = c(finished_tree, lapply(new_elements[end_elements_idx], function(X){ c(branch, X) }))
                finished_duplicity_tree = c(finished_duplicity_tree, lapply(new_duplicities[end_elements_idx], function(X){ c(duplicity_branch, X) }))
            }
            if (any(new_elements %in% branch)) {
                stop('Infinite branching detected.')
            }
            
            # expand the tree with those elements that should still be expanding
            new_tree = c(new_tree, lapply(new_elements[!end_elements_idx], function(X){ c(branch, X) }))
            new_duplicity_tree = c(new_duplicity_tree, lapply(new_duplicities[!end_elements_idx], function(X){ c(duplicity_branch, X) }))
        
        }
        the_tree = new_tree
        duplicity_tree = new_duplicity_tree
    
        if (overflow_counter > 1000) {
           stop('Reached max nr of levels, aborted to prevent infinite loop.') 
        }
        
    }
    
    return(list(tree=finished_tree, duplicity=finished_duplicity_tree))
    
}


# NEED TO FIND A SOLUTION HERE TO NOT JUST COUNT THE BAGS AT THE END OF THE BRANCH!!
# This probably requires some modification in how I track the branches..

my_tree_and_duplicity = calculate_tree_and_duplicity_for_bag('shiny gold')

# Now calculate the amount of bags that are in a shiny gold bag:
final_nr_of_bags = lapply(1:length(my_tree_and_duplicity$duplicity), 
    
    prod, na.rm=T)

print(paste0('Final nr of bags required: ',final_nr_of_bags))










