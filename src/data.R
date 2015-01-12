## Data ######
X <- data.frame( 
    age =        c("a16.30", "a16.30", "au30", "a16.30", "au30", "au30",
                   "au30", "a16.30", "au30", "a16.30", "au30", "a16.30",
                   "a16.30", "au30", "au30", "au30", "a16.30", "au30", "au30",
                   "a16.30", "au30", "au30", "a16.30", "au30", "a16.30"), 
    sex =        c("female", "male", "male", "female", "female", "male",
                   "male", "male", "female", "male", "female", "female",
                   "male", "male", "female", "male", "male", "female", "male",
                   "male", "male", "male", "female", "female", "male"), 
    employment = c("employed", "unemployed", "unemployed", "unemployed",
                   "employed", "unemployed", "employed", "unemployed",
                   "employed", "employed", "unemployed", "unemployed",
                   "unemployed", "unemployed", "unemployed", "employed",
                   "unemployed", "employed", "unemployed", "unemployed",
                   "employed", "unemployed", "employed", "unemployed",
                   "employed"), 
    income =     c(0,3,2,5,0,1,0,4,0,0,1,3,2,5,4,0,3,0,2,4,0,5,0,1,0),
    location =   c("urban", "rural", "rural", "urban", "rural", "urban",
                   "rural", "urban", "rural", "rural", "urban", "rural",
                   "rural", "rural", "urban", "urban", "rural", "urban",
                   "rural", "urban", "rural", "rural", "urban", "urban",
                   "rural"))

# Initial weights
dx <- c(4,5,6,5,3,4,6,4,5,3,5,4,3,6,4,5,6,3,6,4,5,3,5,4,3)

# True population totals
Tx <- data.frame(a16.30=50,
                 au30=50,
                 female=45,
                 male=55,
                 unemployed=70,
                 employed=30,
                 income=200,
                 rural=65,
                 urban=35)
