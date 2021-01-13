
# Advent of Code, day 4
# https://adventofcode.com/2020/day/4

library(stringr)

filePath <- "/Users/m.wehrens/Documents/git_repos/AdventOfCode2020/day4/day4_input.txt"

required_fields = c('byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid')

# determine whether passport is valid based on passport text (as one line)
# for part A
determine_pp_valid = function(passport_line) {
            passport_matrix = str_split(
                str_split(string = passport_line, pattern = ' ', simplify = T), 
                ':', simplify = T)
            
            passport_fieldnames = passport_matrix[-1,1]
            passport_values     = passport_matrix[-1,2]
            
            all(required_fields %in% passport_fieldnames)
            
}

# determine more stringent validity for 
# Part B
determine_pp_valid_stringent = function(passport_line) {
            passport_matrix = str_split(
                str_split(string = passport_line, pattern = ' ', simplify = T), 
                ':', simplify = T)
            
            passport_fieldnames = passport_matrix[-1,1]
            passport_values     = passport_matrix[-1,2]
            
            names(passport_values) = passport_fieldnames
            
            # reject if not all required field names still
            if (!all(required_fields %in% passport_fieldnames)) {return(F)}
            
            # check field value validity
            tests_passed=0
            
            # 1 byr (Birth Year) - four digits; at least 1920 and at most 2002.
            if (str_detect(string = passport_values['byr'], pattern = '^[0-9]{4}$')) {
                if (as.numeric(passport_values['byr'])>=1920 & as.numeric(passport_values['byr']) <= 2020) {
                    tests_passed=tests_passed+1 }
            }
            
            # 2 iyr (Issue Year) - four digits; at least 2010 and at most 2020.
            if (str_detect(string = passport_values['iyr'], pattern = '^[0-9]{4}$')) {
                if (as.numeric(passport_values['iyr'])>=2010 & as.numeric(passport_values['iyr']) <= 2020) {
                    tests_passed=tests_passed+1 }
            }
            
            # 3 eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
            if (str_detect(string = passport_values['eyr'], pattern = '^[0-9]{4}$')) {
                if (as.numeric(passport_values['eyr'])>=2020 & as.numeric(passport_values['eyr']) <= 2030) {
                    tests_passed=tests_passed+1 }
            }
            
            # 4 hgt (Height) - a number followed by either cm or in:
            hgt=passport_values['hgt']
            # check general pattern
            if (str_detect(string = hgt, pattern = '^[0-9]+cm$|^[0-9]+in$')) {
                
                units = substr(hgt, nchar(hgt)-1, nchar(hgt))
                value = as.numeric(substr(hgt, 1, nchar(hgt)-2))
                
                # then check values
                if (units=='cm') {if (value >= 150 & value <= 193) {tests_passed=tests_passed+1}}
                if (units=='in') {if (value >= 59 & value <= 76) {tests_passed=tests_passed+1}}
                
            }
            
            # 5 hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
            if (str_detect(string = passport_values['hcl'], pattern = '^#[0-9a-f]{6}$')) {
                tests_passed=tests_passed+1
            }
            
            # 6 ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
            if (str_detect(string = passport_values['ecl'], pattern = '^amb$|^blu$|^brn$|^gry$|^grn$|^hzl$|^oth$')) {
                tests_passed=tests_passed+1
            }
            
            # 7 pid (Passport ID) - a nine-digit number, including leading zeroes.
            if (str_detect(string = passport_values['pid'], pattern = '^[0-9]{9}$')) {
                tests_passed=tests_passed+1
            }
            
            return(tests_passed==7)
            
}
            
# Read in file, put all info to list
con = file(filePath, "r")
stop_reading = F
passport_line = ''
valid_count = 0; invalid_count = 0
valid_count_stringent = 0; invalid_count_stringent = 0
while ( !stop_reading ) {
    
    # first collect all info into one line
    line = readLines(con, n = 1)
    
    if (length(line)>0) {
        
        if (line[[1]] == "") {
            
            # process this line for part A
            if (determine_pp_valid(passport_line)) {
                valid_count = valid_count + 1
            } else {invalid_count = invalid_count + 1}

            # now for the answer of B
            if (determine_pp_valid_stringent(passport_line)) {
                valid_count_stringent = valid_count_stringent + 1
            } else {invalid_count_stringent = invalid_count_stringent + 1}
            
            # reset
            passport_line = ''
            
        } else {
            
            passport_line = paste(passport_line, line)
            
        } 
            
    } else {
        
        # process this line for part A
        if (determine_pp_valid(passport_line)) {
            valid_count = valid_count + 1
        } else {invalid_count = invalid_count + 1}

        # now for the answer of B
        if (determine_pp_valid_stringent(passport_line)) {
            valid_count_stringent = valid_count_stringent + 1
        } else {invalid_count_stringent = invalid_count_stringent + 1}
        
        stop_reading = T
        
    }
        
}

close(con)

print(paste0('Nr of valid passports: ', valid_count))
print(paste0('Nr of invalid passports: ', invalid_count))

print(paste0('B. Nr of stringently valid passports: ', valid_count_stringent))
print(paste0('B. Nr of stringently invalid passports: ', invalid_count_stringent))
