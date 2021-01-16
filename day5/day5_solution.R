

##################
# Advent of Code, day 5

library(stringr)

##################
# PART A

# Since there's only very few rows (128), I think it's easier to just map the
# rows to their respective codes

# More specifically, it seems to be a binomial combination problem.
# 
# So we can easily generate a dictionary that maps a seat
# code to it's location, because it's a pattern like a decision tree.


# Create row mappings
row_symbols = c('F', 'B')
TOTAL_ROWS = 128
row_mappings_list = sapply(2^((log2(TOTAL_ROWS)-1):0), function(X) {rep(row_symbols, each=X, length.out=TOTAL_ROWS)})
# convert to strings and put in dictionary to look up the codes
row_dict = 0:(TOTAL_ROWS-1)
names(row_dict) = apply(row_mappings_list, 1, paste, collapse='')

# Now do the same for columns
# Create mappings
column_symbols = c('L', 'R')
TOTAL_COLS = 8
col_mappings_list = sapply(2^((log2(TOTAL_COLS)-1):0), function(X) {rep(column_symbols, each=X, length.out=TOTAL_COLS)})
# convert to strings and put in dictionary to look up the codes
col_dict = 0:(TOTAL_COLS-1)
names(col_dict) = apply(col_mappings_list, 1, paste, collapse='')

# Now read the data
data = read.table('/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day5/day5_input.txt')
# Process the data
data$rownumber = sapply(data[,1], function(X) {row_dict[substr(X,1,7)]}, USE.NAMES = F)
data$colnumber = sapply(data[,1], function(X) {col_dict[substr(X,8,10)]}, USE.NAMES = F)
data$seatID    = data$rownumber*8 + data$colnumber


# Produce desired output
max(data$seatID)

##################
# Part B

# OK, so we first need to check which seats are taken, and which ones are not:
# Note that I'm using indexing starting from 1, airline company starts at 0
seat_ID_occupency = rep(0, 128*8)
seat_ID_occupency[data$seatID+1] = 1
# let's mark seats that are at the edge, they won't have two neighbors anyways,
# and they would interfere with my later strategy to check -1 and +1 seats.
seat_ID_occupency[1]=NA
seat_ID_occupency[length(seat_ID_occupency)] = NA

# Identify empty seats (excl. first and last because of aformentioned NAs)
empty_seats = which(seat_ID_occupency==0) # empty seats

# Find out my seat number if indexing would be starting from 1
seat_nr_raw = empty_seats[seat_ID_occupency[empty_seats+1]==1 & seat_ID_occupency[empty_seats-1]==1]
# But it starts at 0, so we need to subtract 1
my_seat_number = seat_nr_raw-1

print(paste('My seat number is:',my_seat_number))



##################


# For debugging purposes, allow manual testing specific cases
test_calculation = function(input='BFFFBBFRRR') {
    print(paste('Rownumber: ',row_dict[substr(input,1,7)]))
    print(paste('Colnumber: ',col_dict[substr(input,8,10)]))
    print(paste('ID: ',col_dict[substr(input,8,10)]+row_dict[substr(input,1,7)]*8))
}
test_calculation('BFFFBBFRRR') # expected: BFFFBBFRRR: row 70, column 7, seat ID 567.
test_calculation('FFFBBBFRRR') # expected: FFFBBBFRRR: row 14, column 7, seat ID 119.
test_calculation('BBFFBBFRLL') # expected: BBFFBBFRLL: row 102, column 4, seat ID 820.



    
    


