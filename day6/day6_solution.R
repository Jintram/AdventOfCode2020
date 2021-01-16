##################
# Advent of Code, day 6

# So this can be done in a very simple way:
# Per group, just loop over the lines, 
# and collect yesses in a 26x1 vector representing the 26 questions,
# then calculate sum.

filePath = '/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day6/day6_input.txt'

# create a symbol to index dict
empty_qvec = rep(F,26) # to use later as answer vector template
names(empty_qvec) = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')

con = file(filePath, "r")
current_qvec = empty_qvec
collected_output=c()
while ( T ) {
    
    counter = counter + 1
    
    # Go over each group
    line = readLines(con, n = 1)
    
    if (length(line)>0) { # ie we haven't reached eof
        
        if (line[[1]] == "") { # indicating end of group
            
            # process this group
            all_yes_count = sum(current_qvec)
            
            # collect output
            collected_output = c(collected_output, all_yes_count)
            
            # start new group
            current_qvec = empty_qvec
            
        } else { # otherwise process the line
            
            # Mark the ones answered yes, keeping previous yesses from this group too
            current_qvec[str_split(line,'', simplify = T)] = T
            
        }
        
    } else { # eof reached
        
        # Process final group
        all_yes_count = sum(current_qvec)
        
        # collect output
        collected_output = c(collected_output, all_yes_count)
    
        break 
        
    }
}

close(con)

# The output:
print(paste0('Sum of all-yesses for the groups: ',sum(collected_output)))


###################

##################
# Advent of Code, day 6
# Part B

# So this can be done in a very simple way:
# For each group, 
# we just need to create a matrix which is 26xN, 
# N being the size of the group, where 
# positive questions are marked.
# Then we count the number of columns that have
# any ones
#
# Actually, I guess this could be done as 
# was done for part A, but just looking
# which answers are 
# both yes for line 1 & 2, store the result,
# make same comparison for that result and line 3,
# and so on.
# Not sure which solution is computationally more
# efficient (memory/flops tradeoff?)

# create a symbol to index dict
empty_qvec = rep(F,26) # to use later as answer vector template
names(empty_qvec) = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')

con = file(filePath, "r")
counter=0; current_group = list()
collected_output=c()
while ( T ) {
    
    counter = counter + 1
    
    # Go over each group
    line = readLines(con, n = 1)
    
    if (length(line)>0) { # ie we haven't reached eof
        
        if (line[[1]] == "") { # indicating end of group

            # process this group
            all_yes_count = sum(
                apply(as.data.frame(current_group, col.names = 1:length(current_group)), 1, all))
            
            # collect output
            collected_output = c(collected_output, all_yes_count)
            
            # start new group
            counter=0; current_group = list()
            
        } else { # otherwise process the line
            
            # convert line to vector with 26 Qs
            current_qvec = empty_qvec
            current_qvec[str_split(line,'', simplify = T)] = T
            
            current_group[[counter]] = current_qvec
            
        }
        
    } else { 
        
        # Process final group
        all_yes_count = sum(
            apply(as.data.frame(current_group, col.names = 1:length(current_group)), 1, all))
        
        # collect output
        collected_output = c(collected_output, all_yes_count)
        
        break 
        
    } # if end of file, quit reading
}

close(con)

# The output:
print(paste0('Sum of all-yesses for the groups: ',sum(collected_output)))




