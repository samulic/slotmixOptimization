## ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(caret)
library(linprog)
options(scipen = 999)
slot <- readxl::read_excel("data/Lucky Duck Entertainment revenue 2013.xls", na = c(".", "NA", "NaN"))
str(slot)

## ------------------------------------------------------------------------
df <- slot %>%
  group_by(Month, Casino, Section, MachineType, Denomination) %>%
  summarise(numero_macchine    = sum(NoMachines),
            ricavi_totali      = sum(GrossRevenue),
            ricavo_unitario    = ricavi_totali/numero_macchine,
            giocate_totali     = sum(Plays),
            giocate_unitarie   = round(giocate_totali/numero_macchine),
            ricavo_per_giocata = ricavi_totali/giocate_totali) %>%
  arrange(Month, Section, MachineType, Denomination) %>%
  mutate(tipo = paste0(MachineType, "_", Denomination)) 
head(df)


## ------------------------------------------------------------------------
# function to optimize winnings for the given month
solver <- function(data, month) {
  df <- filter(data, Month == ymd(month))
  numslots.section <- df %>%
    group_by(Month, Casino, Section) %>%
    summarise(sum = sum(numero_macchine),
              ricavo = sum(ricavi_totali)) %>%
    #il mutate perde un livello dopo il summarise
    mutate(sum_casino = sum(sum), prop_per_mese = round(sum / sum(sum), 3))
  
  
  # ggplot(data = numslots.section, aes(x = sum, y = prop_per_mese)) +
  #   geom_point(aes(color = Casino, size = ricavo)) +
  #   ggtitle("Distribuzione della proporzione del numero di slot sul totale del casino per ogni mese") +
  #   xlab("Numero slot per sezione (per mese)") +
  #   ylab("Proporzione di slot in ogni mese") #+ facet_wrap(~Section)
  
  #Ottieni numero di slot presenti in ogni casino per mese da usare come upper bound
  numslots.casino <- NULL #a cosa serve ? 
  nslots <- df %>%
    group_by(Month, Casino) %>%
    summarise(sum = sum(numero_macchine)) %>%
    arrange(Month) #ensure order
  
  nslots <- nslots$sum #numero di slot per casino e mese
  
  ## ------------------------------------------------------------------------
  f.obj <- round(df$ricavo_unitario)
  
  ## ------------------------------------------------------------------------
  #First create dummy var for Casino and month, used to activate each constraint depending on the casino and month
  f.A1 <- t(predict(dummyVars(~ Casino, data = df), newdata = df))
  #View(rbind(t(df[, "Casino"]), t(df$Month), f.A)) #Check results
  
  #with one month don't require " : I(factor(Month)" after Casino
  
  
  #Coefficients
  b1 <- nslots #month1-> casino1, month1 ->casino2, month2 -> casino1, ...
  f.b1 <-  b1# vector of right-hand side values - upper bounds of total number of slots per month
  f.dir1 <- rep("<=", length(f.b1)) # direction of the inequalities
  
  sol1 <- solveLP(f.obj, f.b1, f.A1, maximum = TRUE, const.dir = f.dir1) #solver 
  sol1bis <- lp(direction = "max", objective.in = f.obj, const.mat = f.A1, const.dir = f.dir1, const.rhs = f.b1)
  
  sol1$con
  sol1$solution
  
  ## ------------------------------------------------------------------------
  f.A2 <- t(predict(dummyVars(~ Section : Casino, data = df), newdata = df))
  #with one month don't require " : factor(Month)" after Casino
  
  # Duplicate every row, needed because for each section/month/casino we need a lower and upper bound. 
  # Retain order --> ob1-low, obs1-up, obs2-low, ...)
  f.A2 <- as.tibble(f.A2) %>% slice(rep(1:n(), each = 2)) %>% as.matrix()
  
  #I vincoli sono attivati nel seguente modo 
  # mese1-casino1-sez1-low,
  # mese1-casino1-sez1-up,
  # mese1-casino1-sez2;
  # mese1-casino2;
  # mese2;...
  # quindi i primi 8 (4 sez * 2 up/low) vincoli sono associato ad un medesimo upper e lower bound, legato al primo valore di nslots.
  
  #nslots contiene il numero di slot per ogni casino e mese, alternato per casino (mese1 casino1, mese1 casino2, mese2 casino1, ...)
  # combine nslots in two columns, where rows are months and columns are casino (first for Aries)
  (numslots <- matrix(nslots, ncol = 2, byrow = T))
  f.b2 <- c()
  
  
  month_prop <- numslots.section %>%
    group_by(Month, Casino) %>% summarise(prop_min = min(prop_per_mese),
                                          prop_max = max(prop_per_mese))
  
  b2 <- cbind(nslots, month_prop$prop_min, month_prop$prop_max)
  colnames(b2) <- NULL #altrimenti errore nel solver 
  # to check correctness, compare with 
  #View(numslots.section)
  f.b2 <- c()
  # probabilmente si puo' fare senza il ciclo for, ma come??
  for (i in 1:nrow(b2)) {
    f.b2 <- append(f.b2, rep(round(c(b2[i,1]*b2[i,2], b2[i,1]*b2[i,3])), 4))
  }
  
  f.dir2 <- rep(c(">=", "<="), length(f.b2)/2) # direction of the inequalities
  
  sol2 <- solveLP(f.obj, f.b2, f.A2, maximum = TRUE, const.dir = f.dir2) #solver 
  sol2bis <- lp(direction = "max", objective.in = f.obj, const.mat = f.A2, const.dir = f.dir2, const.rhs = f.b2)
  
  summary(sol2)
  sol2$con
  
  ## ------------------------------------------------------------------------
  #To see solution on the original dataset
  # df[which(sol2$solution != 0),] %>%
  #   arrange(Casino, tipo) %>%
  #   View()
  
  # Vincolo 3 - preprocessing
  ## ------------------------------------------------------------------------
  #Calcola i bin per le giocate totali del casino Libra, per differenziare dai bin di Aries
  quartili.aries <- c(quantile(df[df$Casino == "Aries", ]$"giocate_totali", probs = seq(0, 1, by = 0.25)))
  quartili.libra <- c(quantile(df[df$Casino == "Libra", ]$"giocate_totali", probs = seq(0, 1, by = 0.25)))
  #Check dataset ordering --> arrange(Month, Section, MachineType, Denomination, Casino)
  head(df, n = 100)
  head(arrange(df, Month, Section, MachineType, Denomination, Casino), n = 100) #ok, e' cosi'
  # split dataset based on casino, to differentiate bins
  aries.idx <- which(df$Casino == "Aries")
  aries <- df[aries.idx,]
  libra <- df[-aries.idx,]
  # Create bins based on total plays, include lowest value, and display digits instead of scientific notation
  aries$bin_giocate <- cut(aries$giocate_totali, breaks = quartili.aries, include.lowest = T, dig.lab=10)
  libra$bin_giocate <- cut(libra$giocate_totali, breaks = quartili.libra, include.lowest = T, dig.lab=10)
  # merge the two casino into the original dataset, with the additional column bin_giocate
  df <- rbind(aries, libra) %>% arrange(Month, Section, MachineType, Denomination, Casino)
  # reorder columns, set bin_giocate next to giocate_totali
  df[c(1:9, 13, 10:12)]
  # Confirm ordering is the same as before
  all(which(df$Casino == "Aries") == aries.idx)
  
  bin_stats <- df %>% 
    group_by(bin_giocate, Casino) %>%
    summarise(minimo_macchine = min(numero_macchine),
              massimo_macchine = max(numero_macchine),
              media_macchine = mean(numero_macchine),
              mediana_macchine = median(numero_macchine),
              totale_macchine = sum(numero_macchine)) %>%
    #arrange(Casino, desc(bin_giocate)) #non funziona bene come ordinamento
    arrange(Casino, media_macchine) #meglio
  
  # Vincolo 3 - vincola il numero di slot di ogni categoria tra un min e max calcolato per gruppi di slot che condividono uno stesso range di numero di giocate.
  # create all combinations of ...
  f.A3 <- diag(nrow(df))
  f.A3 <- as.tibble(f.A3) %>% slice(rep(1:n(), each = 2)) %>% as.matrix()
  
  b <- c()
  for (i in 1:nrow(df)) {
    #get the correct row index to identify the bin reference
    idx <- match(df[i,"bin_giocate"], bin_stats$bin_giocate)
    # create vector of constraint coefficient, lower then upper bounds
    b <- append(b, bin_stats[idx, "minimo_macchine"])
    b <- append(b, bin_stats[idx, "massimo_macchine"])
  }
  
  f.b3 <- unlist(b, use.names=FALSE)
  f.dir3 <- rep(c(">=", "<="), dim(df)[1]) # direction of the inequalities
  
  sol3 <- solveLP(f.obj, f.b3, f.A3, maximum = TRUE, const.dir = f.dir3) # Va in loop, quello sotto no. 
  sol3bis <- lp(direction = "max", objective.in = f.obj, const.mat = f.A3, const.dir = f.dir3, const.rhs = f.b3)
  
  #sol3$con
  summary(sol3)
  sol3bis
  
  #check if A3 is equal to f.A3 -> all(A3 == f.A3) FALSE
  
  ## ------------------------------------------------------------------------
  f.A <- rbind(f.A1, f.A2, f.A3)
  f.b <- c(f.b1, f.b2, f.b3)
  f.dir <- c(f.dir1, f.dir2, f.dir3)
  
  sol <- solveLP(f.obj, f.b, f.A, maximum = TRUE, const.dir = f.dir) #solver 
  #sol.bis <- lp(direction = "max", objective.in = f.obj, const.mat = f.A, const.dir = f.dir, const.rhs = f.b)
  
  #sol$solution
  #sol.bis
  #summary(sol)
  #shadow_price <- sol$con
  
  return(sol)
}

# create empty list to store each month's result
sols <- alist()
# choose which months to use (all months present in the dataset)
months <- unique(df$Month)
# apply foreach month, the function solver to the original dataframe df (solver will filter for each month)
sols <- lapply(months, FUN = solver, data = df)
# give names to the list of solutions
names(sols) <- months

sols$`2011-09-01`$con

save(file = "solutions.rdata", solver, bin_stats, df, sols)

sols[["2012-03-01"]]
# get the optimum found for each month
sapply(sols, `[[`, "opt")

