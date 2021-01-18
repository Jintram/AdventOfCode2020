

# I was thoroughly on the wrong track here
#
# should have drawn it out on paper beforehand :)







# Now the question is which bags can hold a shiny gold bag, 
# but the problem is nested. 
#
# (And I guess we should hope that there aren't any infinite 
# loops in there?)

# So maybe write a function that will also call itself to
# delve into the nested structure..

# Also note that the numbers don't matter, the question
# is about "at least 1", so we can simplify the rules structure

simplified_rules = lapply(the_rules, function(X) {names(X)})
simplified_rules_endpoints = sapply(simplified_rules, function(X) {if (X[1]=='none') {return(1)} else {return(0)}})

# maybe we need to inverse actually, in which bags is 
# a bag allowed to travel?

# Let's make a table first
# (Knowing the assingment better, I could have 
# immediately generated this table from the
# input file instead of converting here.)
bag_allowed_in_df = data.frame(bag_inner = c(), bag_outer = c())
bags_outer = names(simplified_rules)
counter=0
for (idx in 1:length(simplified_rules)) {
    counter=counter+1
    
    bag_allowed_in_df = rbind(bag_allowed_in_df, data.frame(bag_inner = simplified_rules[[idx]], bag_outer=bags_outer[idx]))
}

# Get all colors that are defined
all_possible_colors = unique(c(bag_allowed_in_df$bag_inner, bag_allowed_in_df$bag_outer))

MAX_ALLOWED = 1000
give_allowed_bags = function(input_bags, level) {
    collection=c()
    
    level = level+1
    if (level>MAX_ALLOWED) { print(input_bags); stop('inf loop error')  } 
    print( paste0('Current level: ', level) )
    
    
    for (current_bag_color in input_bags) {
        
        collection = c(collection, bag_allowed_in_df[bag_allowed_in_df$bag_inner == current_bag_color,]$bag_outer)
        
    }
    
    collection_next_level = give_allowed_bags(collection, level)[[1]]
    
    return(list(collection,collection_next_level))
    
}

test_out = give_allowed_bags("dark silver",0)
test_out = give_allowed_bags("shiny gold",0)

allowed_in = function(bag_color) {
    bag_allowed_in_df[bag_allowed_in_df$bag_inner == bag_color,]$bag_outer   
}

total_collection = c()
level=0
old_collection = "dark silver"
while (level < 1000) {
    
    level=level+1
    collection = c()
    
    for (current_bag_color in old_collection) {
        
        collection = c(collection, allowed_in(current_bag_color))
        
    }
    
    total_collection[[level]] = collection
    old_collection = collection
    
}
print(level)

allowed_in('pale plum')
allowed_in('dim green')
allowed_in('muted gold')
allowed_in('bright gray')
allowed_in('shiny lime')