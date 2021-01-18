################################################################################
# Advent of Code
# Personal notes w/ day 8 solution

# Note that in R, you can't access the looping parameter, even though
# it appears the value is changed; the following will print "0" 10x.
counter=0
for (idx in 1:10) {
    idx=0
    counter=counter+1
    if (counter>100) {break}
    print(paste0(idx))
}

# let's just take a look how the path looks
library(ggplot2)
ggplot(data.frame(n=1:length(location_history), location=location_history)) + 
    geom_line(aes(x=n, y=location))+theme_bw()

data_instructions$value[location_history]