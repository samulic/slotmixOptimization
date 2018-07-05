################################################
# STRATEGIA 1: MODELLO MACRO DIVISO PER CASINO
###############################################
options(scipen = 999)

library(rstudioapi)
#Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print(getwd())

library(readxl)
slot = read_excel("Lucky Duck Entertainment revenue 2013.xls", na = c(".", "NA", "NaN"))

library(funModeling)
library(dplyr)
library(lubridate)

#status
status = df_status(d, print_results = F)
summary(d$Month) #12 mesi

#aries
d_aries = slot %>%
  filter(Casino == "Aries")

#libra
d_libra = slot %>%
  filter(Casino == "Libra")


#keep calm: first exploration -> only aries
df = d_aries %>%
  group_by(Denomination, MachineType, Section, Month) %>%
  summarise(numero_macchine = sum(NoMachines),
            ricavi_totali = sum(GrossRevenue),
            ricavo_unitario = ricavi_totali/numero_macchine,
            giocate_totali = sum(Plays),
            giocate_unitarie = round(giocate_totali/numero_macchine),
            ricavo_per_giocata = ricavi_totali/giocate_totali) %>%
  arrange(Month, Section, MachineType, Denomination) %>%
  mutate(tipo = paste0(MachineType, "_", Denomination))

library(ggplot2)
ggplot(data=slot, aes(x=slot$Month, y = slot$GrossRevenuePerMachine)) +
  geom_point(alpha=.4, size=4, color="#880011") +
  labs(x="Month", y="Gross Revenue per Machine") +
  theme_classic()

ricavi_mese = df %>% 
  group_by(Month) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario), 
            numero_macchine_medie = mean(numero_macchine),
            varianza_macchine = sqrt(var(numero_macchine)))

ggplot(data=ricavi_mese, aes(x=ricavi_mese$Month, y = ricavi_mese$ricavo_medio_unitario)) +
  geom_line(alpha=.5, size=1, color="#880011") +
  ggtitle("Ricavi per Mese") +
  labs(x="Mese", y="Ricavo medio unitario") +
  theme_classic()

ricavi_mese$Month = as.factor(ricavi_mese$Month)

ggplot(data=ricavi_mese, aes(x=ricavi_mese$numero_macchine_medie,
                             y=ricavi_mese$ricavo_medio_unitario,
                             colour = Month)) +
  geom_point(alpha=.4, size=4) +
  ggtitle("Ricavi e Numero macchine mensili") +
  labs(x="Numero medio macchine", y="Ricavi medi unitari") + 
  theme_minimal()

ricavi_categoria = df %>%
  group_by(Denomination, MachineType) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario)) %>%
  mutate(type = paste0(Denomination,sep = "_", MachineType))


ggplot(data=ricavi_categoria, aes(x=ricavi_categoria$type, y = ricavi_categoria$ricavo_medio_unitario)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Ricavi per Categoria") +
  labs(x="Categoria", y="Ricavo medio unitario") +
  theme_classic()

ricavi_sezione = df %>% 
  group_by(Section) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario))

ggplot(data=ricavi_sezione, aes(x=ricavi_sezione$Section, y = ricavi_sezione$ricavo_medio_unitario)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Ricavi per Sezione") +
  labs(x="Sezione", y="Ricavo medio unitario") +
  theme_classic()

df %>%
  group_by(Denomination, MachineType) %>%
  summarise(n = n()) %>%
  View()#13 categorie al massimo

categorie_mese = 
  df %>% 
  group_by(Month, Denomination, MachineType) %>% 
  summarise(n =n()) %>% 
  group_by(Month) %>%
  summarise(numero_categorie = n()) #alla fine restano pi? o meno 13 = raggruppamento per categoria ha senso [per aries]

df %>% 
  group_by(Section, Month) %>% 
  summarise(n =n()) %>%
  View()

df %>% group_by(tipo, Section) %>%
  summarise(n = n()) %>%
  arrange(tipo, Section) %>%
  group_by(Section) %>%
  summarise(n = n()) %>%
  View() #Nelle sezioni le categorie sono pi? o meno le stesse per tutto l'anno  = Ha senso questo raggruppamento 

df %>% group_by(tipo, Section, Month) %>%
  summarise(n = n()) %>%
  arrange(tipo, Section) %>%
  group_by(Section) %>%
  summarise(n = n()) %>%
  View() 

df %>% group_by(Section, Month) %>%
  summarise(n = n()) %>%
  arrange(Section) %>%
  View()

df %>% group_by(Section, Month) %>%
  summarise(n = n()) %>%
  arrange(Section) %>%
  group_by(Section) %>%
  summarise(Numero_medio_categorie = mean(n), Varianza_categorie = sqrt(var(n))) %>%
  View()

df %>% group_by(Section, Month) %>%
  summarise(n = n()) %>%
  arrange(Section) %>%
  group_by(Month) %>%
  summarise(Numero_medio_categorie = mean(n), Varianza_categorie = sqrt(var(n))) %>%
  View()


#"Modello semplice"  = Lavoriamo solamente su Aries [per iniziare]

if(require(linprog)==FALSE) install.packages("linprog")
library(linprog)

#Modello con vincolo semplice legato solo al quantitativo mensile 

f_obj = df$ricavo_unitario #ricavi unitari = da MAX

a =  c(rep(1,473),
  ifelse(df$Month == ymd("2011-09-01"), 1, 0),
  ifelse(df$Month == ymd("2011-09-01"), 1, 0),
  ifelse(df$Month == ymd("2011-10-01"), 1, 0),
  ifelse(df$Month == ymd("2011-10-01"), 1, 0),
  ifelse(df$Month == ymd("2011-11-01"), 1, 0),
  ifelse(df$Month == ymd("2011-11-01"), 1, 0),
  ifelse(df$Month == ymd("2011-12-01"), 1, 0),
  ifelse(df$Month == ymd("2011-12-01"), 1, 0),
  ifelse(df$Month == ymd("2012-01-01"), 1, 0),
  ifelse(df$Month == ymd("2012-01-01"), 1, 0),
  ifelse(df$Month == ymd("2012-02-01"), 1, 0),
  ifelse(df$Month == ymd("2012-02-01"), 1, 0),
  ifelse(df$Month == ymd("2012-03-01"), 1, 0),
  ifelse(df$Month == ymd("2012-03-01"), 1, 0),
  ifelse(df$Month == ymd("2012-04-01"), 1, 0),
  ifelse(df$Month == ymd("2012-04-01"), 1, 0),
  ifelse(df$Month == ymd("2012-05-01"), 1, 0),
  ifelse(df$Month == ymd("2012-05-01"), 1, 0),
  ifelse(df$Month == ymd("2012-06-01"), 1, 0),
  ifelse(df$Month == ymd("2012-06-01"), 1, 0),
  ifelse(df$Month == ymd("2012-07-01"), 1, 0),
  ifelse(df$Month == ymd("2012-07-01"), 1, 0),
  ifelse(df$Month == ymd("2012-08-01"), 1, 0),
  ifelse(df$Month == ymd("2012-08-01"), 1, 0))

A = matrix(a,25,473, byrow = T) #Matrice A del modello, 1 se valore attivo, 0 altrimenti 

b = c(5896, rep(c(404, 849), 12)) #upper and lower bound
constraints = c("<=", rep(c(">=", "<="), 12)) 

sol <- solveLP(f_obj, b, A, maximum = TRUE, constraints)
summary(sol)

shadow_price = sol$con

#proviamo ad aggiungere qualche vincolo

#proporzioni delle sezioni per mese 
df %>%
  group_by(Month, Section) %>%
  summarise(Num_macchine = sum(numero_macchine)) %>%
  mutate(prop_per_mese = round(Num_macchine / sum(Num_macchine), 2)) %>%
  View()#il mutate perde un livello dopo il summarise

#Cerco di capire come individuare le variabili per un mese...poi estendo a tutti e 12 i mesi

#numeratore proporzione
# ifelse(df$Month == ymd("2011-09-01")  & df$Section == "Boundary", 1, 0) #ok

#denominatore 
# ifelse(df$Month == ymd("2011-09-01"), 1, 0) #ok

#vincolo base

#0.2 <= numeratore/denominatore <= 0.3
#ma si pu?? forse occorre scriverlo in maniera diversa? 
#No non ? lineare!

x = df %>%
  group_by(Month) %>%
  summarise(Num_macchine = sum(numero_macchine)) 

variazione = round(sqrt(var(x$Num_macchine))) #potremmo usare questo per definire gli upper e lower 

d = unique(ymd(df$Month))
s = unique(df$Section)
a1 = c()
for (i in 1:12) {
  for (k in 1:4) {
    for (t in 1:2) {
      a1 = append(a1, ifelse(df$Month == d[i] & df$Section ==s[k], 1, 0))
    }
  }
}

A1 = matrix(a1,96,473, byrow = T)
A = rbind(A, A1)

b = c(5896, rep(c(404, 849), 12)) #upper and lower bound

inferiore = c()
superiore = c()
for (i in 1:length(x$Num_macchine)) {
  inferiore[i] = x$Num_macchine[i] - variazione
  superiore[i] = x$Num_macchine[i] + variazione
}

v = c(rbind(inferiore,superiore)) #unire 2 vettori in modo alternato 

b = c(5896, rep(c(404, 849), 12), rep(c(round(0.2*404),round(0.3*849)), 48))
constraints = c("<=", rep(c(">=", "<="), 12), rep(c(">=", "<="), 48))

sol <- solveLP(f_obj, b, A, maximum = TRUE, constraints)
summary(sol)

shadow_price = sol$con

#Conclusione: il modello potrebbe essere pi? completo ed elegante, ma l'interpretabilit? e l'inserimento dei vincoli
#? particolarmente oneroso.

################################################################
# STRATEGIA 2: OTTIMIZZO MESE PER MESE MA PER ENTRAMBI I CASINO
###############################################################
options(scipen = 999)

library(rstudioapi)
#Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print(getwd())

library(readxl)
slot = read_excel("../Decision_Models/Lucky Duck Entertainment revenue 2013.xls", na = c(".", "NA", "NaN"))

library(funModeling)
library(dplyr)
library(lubridate)

#status
status = df_status(slot, print_results = F)
summary(slot$Month) #12 mesi

tot = slot %>%
  group_by(Casino, Denomination, MachineType, Section, Month) %>%
  summarise(numero_macchine = sum(NoMachines),
            ricavi_totali = sum(GrossRevenue),
            ricavo_unitario = ricavi_totali/numero_macchine,
            giocate_totali = sum(Plays),
            giocate_unitarie = round(giocate_totali/numero_macchine),
            ricavo_per_giocata = ricavi_totali/giocate_totali) %>%
  arrange(Month, Section, MachineType, Denomination) %>%
  mutate(tipo = paste0(MachineType, "_", Denomination))

tot = as.data.frame(tot)

ricavi_mese = tot %>% 
  group_by(Month) %>%
  summarise(ricavo_medio_totale = mean(ricavi_totali),
            ricavo_medio_unitario = mean(ricavo_unitario),
            numero_macchine_medie = mean(numero_macchine),
            varianza_macchine = sqrt(var(numero_macchine)))

ggplot(data=ricavi_mese, aes(x=ricavi_mese$Month, y = ricavi_mese$ricavo_medio_unitario)) +
  geom_line(alpha=.5, size=1, color="#880011") +
  ggtitle("Ricavi per Mese di LDE") +
  labs(x="Mese", y="Ricavo medio unitario") +
  theme_classic()
#non ? un andamento lineare: le linee servono per sottolineare il fenomeno altalenante. Non esiste rilevazione infra mese

ricavi_mese$Month = as.factor(ricavi_mese$Month)

#NOTA: non ho idea se abbia pi? senso mettere i ricavi medi unitari (grafico 1) o i ricavi medi totali (grafico 2)
ggplot(data=ricavi_mese, aes(x=ricavi_mese$numero_macchine_medie,
                             y=ricavi_mese$ricavo_medio_totale,
                             colour = Month)) +
  geom_point(alpha=.4, size=4) +
  ggtitle("Pi? macchine hai pi? guadagni? In linea di massima SI!") +
  labs(x="Numero medio macchine", y="Ricavi medi totali") + 
  theme_minimal()

ggplot(data=ricavi_mese, aes(x=ricavi_mese$numero_macchine_medie,
                             y=ricavi_mese$ricavo_medio_unitario,
                             colour = Month)) +
  geom_point(alpha=.4, size=4) +
  ggtitle("Tante macchine non significa ricavi unitari elevati!") +
  labs(x="Numero medio macchine", y="Ricavi medi unitari") + 
  theme_minimal()

#io dai grafici deduco questo: a settembre ho tante macchine con ricavi unitari bassi che nel complesso mi fanno guadagnare molto;
#negli altri mesi la situazione ? altalenante: marzo per esempio ha poche macchine ma con ricavo unitario elevatissimo

ricavi_categoria = tot %>%
  group_by(Denomination, MachineType) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario)) %>%
  mutate(type = paste0(Denomination,sep = "_", MachineType))

ggplot(data=ricavi_categoria, aes(x=ricavi_categoria$type, y = ricavi_categoria$ricavo_medio_unitario)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Ricavi per Categoria di LDE") +
  labs(x="Categoria", y="Ricavo medio unitario") +
  theme_classic()

ricavi_sezione = tot %>% 
  group_by(Section) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario))

ggplot(data=ricavi_sezione, aes(x=ricavi_sezione$Section, y = ricavi_sezione$ricavo_medio_unitario)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Ricavi per Sezione di LDE") +
  labs(x="Sezione", y="Ricavo medio unitario") +
  theme_classic()

ricavi_casino = tot %>% 
  group_by(Casino) %>%
  summarise(ricavo_medio_unitario = mean(ricavo_unitario))

ggplot(data=ricavi_casino, aes(x=ricavi_casino$Casino, y = ricavi_casino$ricavo_medio_unitario)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Ricavi per Casino di LDE") +
  labs(x="Casino", y="Ricavo medio unitario") +
  theme_classic()

tot %>%
  group_by(tipo) %>%
  summarise(n = n()) %>%
  View()#14 categorie al massimo 

tot %>% 
  group_by(Casino,Section, Month) %>% 
  summarise(n =n()) %>%
  View()

tot %>% group_by(Casino, tipo, Section, Month) %>%
  summarise(n = n()) %>%
  arrange(tipo, Section) %>%
  group_by(Section) %>%
  summarise(n = n()) %>%
  View()#numero categorie per sezione di LDE

tot %>% group_by(Casino, Section, Month) %>%
  summarise(n = n()) %>%
  arrange(Casino, Section) %>%
  group_by(Casino, Section) %>%
  summarise(Numero_medio_categorie = mean(n), Varianza_numero_categorie = sqrt(var(n))) %>%
  View()

tot %>% group_by(Casino, Section, Month) %>%
  summarise(n = n()) %>%
  arrange(Casino, Section) %>%
  group_by(Month) %>%
  summarise(Numero_medio_categorie = mean(n), Varianza_numero_categorie = sqrt(var(n))) %>%
  View()

#MODELLO PER SETTEMBRE: verr? poi esteso a tutti i mesi

set = tot %>%
  filter(Month == ymd("2011-09-01")) %>%
  arrange(Casino)

set = as.data.frame(set)

f_obj = round(set$ricavo_unitario) #ricavi unitari = da MAX

#quante macchine ci sono ogni mese nei 2 casino? 
tot %>% 
  group_by(Casino, Month) %>%
  summarise(n_macchine = sum(numero_macchine)) %>%
  arrange(Month) %>%
  View()

#costruzione vincolo 1: numero massimo di macchine per casino nel mese di settembre 

library(caret)
Vincolo1 =  function(df){
  "dummy + trasposizione"
  A = predict(dummyVars(~ Casino, data = df), newdata = df)
  A = t(A)
  return(A)
} #approccio valido se ho una sola colonna

A = Vincolo1(set)
#Vettore B del modello lineare
b = c(849, 230) #upper bound
constraints = c("<=", "<=") 

library(linprog)
sol <- solveLP(f_obj, b, A, maximum = TRUE, constraints) #solver 

#risultato 1
summary(sol)
shadow_price = sol$con

#risultato sul dataset
set[which(sol$solution != 0),] %>%
  arrange(Casino, tipo) %>%
  View()
#Avendo solo questo vincolo le macchine vengo piazzate dove il ricavo unitario ? massimo. Mi sembra ragionevole
#E' chiaro che occorre tenere in considerazione altri vincoli.

#Vincolo 2: proporzione macchine in ciascuna sezione dei 2 Casino

set %>%
  group_by(Casino, Section) %>%
  summarise(Num_macchine = sum(numero_macchine)) %>%
  mutate(prop_per_mese = round(Num_macchine / sum(Num_macchine), 2)) %>%
  View()

#Per Aries ciascuna sezione deve avere un numero di macchine: 0.2 >= x <= 0.3
#Per Libra ciascuna sezione deve avere un numero di macchine: 0.15 >= x <= 0.35
#Si tratta di valori arbitrari dedotti dai dati


#Matrice A:
#1) creo un vettore di 1 e di 0 -> dove le variabilid decisionali sono attive varr? 1 altrimenti 0;
#2) popolo la matrice A (modello lineare) attraverso il vettore creato
Vincolo2 = function(df){
  "funzione per costruire il vincolo 2"
  x = unique(df$Section)
  y = unique(df$Casino)
  a = c()
  for (t in 1:length(y)) {
    for (i in 1:length(x)) {
      for (k in 1:2) {
        a = append(a, ifelse(df$Section == x[i] & df$Casino == y[t], 1, 0))
      }
    }
  }
  A = matrix(a,16,78, byrow = T)
  return(A)
}

#Vincolo 2  
A1 = Vincolo2(set)

#Vincolo 1 e Vincolo 2 (Matrice A)
A = rbind(A, A1)

#Vettore B aggiornato 
b = c(b,
      rep(c(round(0.2*849), round(0.3*849)), 4), rep(c(round(0.15*230), round(0.35*230)), 4))

constraints = c(constraints,
                rep(c(">=", "<="), 8))

#Risultato 2
sol <- solveLP(f_obj, b, A, maximum = TRUE, constraints)
summary(sol)
shadow_price = sol$con

#risultato sul dataset
set[which(sol$solution != 0),] %>%
  arrange(Casino, tipo) %>%
  View()

######################################################################################################################
#SE SI RISOLVE IL VINCOLO 4 QUESTO SECONDO ME DIVENTA SUPERFLUO
# #Vincolo 3: categorie di macchine non nulle [questo vincolo potrebbe essere superfluo dopo l'inserimento del Vincolo 4]
# 
# set %>%
#   group_by(Casino, tipo) %>%
#   summarise(n = n(), macchine = sum(numero_macchine)) %>%
#   View()
# 
# #almeno una macchina per categoria in ciascun casino! onesto -> questione di gusti dei clienti!
# 
# Vincolo3 = function(df){
#   "funzione per costruire il vincolo 3"
#   x = unique(df$tipo)
#   y = unique(df$Casino)
#   a = c()
#   for (t in 1:length(y)) {
#     for (i in 1:length(x)) {
#       a = append(a, ifelse(df$tipo == x[i] & df$Casino == y[t], 1, 0))
#     }
#   }
#   A = matrix(a,28,78, byrow = T)
#   A = A[c(1:13,15:28),] #la riga 14 ? vuota siccome nel casino Aries ci sono 13 categorie e non 14 -> si rimuove
#   return(A)
# }
# 
# #Vincolo 3
# A2 = Vincolo3(set)
# 
# #Vincolo 1 e Vincolo 2 (Matrice A)
# A = rbind(A, A2)
# 
# #Vettore B
# b = c(b,
#       rep(1,27)) #mettiamo 1 simbolicamente [ha poco senso avere una categoria con una sola macchina
# 
# constraints = c(constraints,
#                 rep(">=", 27))
# 
# #Risultato 3
# sol <- solveLP(f_obj, b, A, maximum = TRUE, constraints)
# summary(sol)
# 
# shadow_price = sol$con
# 
# #risultato sul dataset
# set[which(sol$solution != 0),] %>%
#   arrange(Casino, tipo) %>%
#   View()
#########################################################################################################################
#noi per? stiamo ottimizzando sul numero di macchine per categoria perci? forse avrebbe pi? senso avere:
#tutte le categorie attive in settembre;
#pi? macchine per categoria altrimenti come fanno a giocare i tizi?

#forse le macchine con pi? giocate sono quelle pi? utilizzate?
ggplot(data=set, aes(x=set$giocate_totali, y = set$numero_macchine)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("a settembre, pi? macchine ci sono pi? si gioca? Si") +
  labs(x="Giocate Totali", y="Numero di Macchine") +
  theme_classic() #si

cor(set$giocate_totali,set$numero_macchine) #0.97 ho scoperto l'acqua calda 

##################################
#mistero misterioso
 ggplot(data=set, aes(x=set$giocate_unitarie, y = set$numero_macchine)) +
   geom_point(alpha=.5, size=3, color="#880011") +
   ggtitle("a settembre, pi? macchine ci sono pi? si gioca? No") +
   labs(x="Giocate unitarie", y="Numero di Macchine") +
   theme_classic() #no
##################################

#ma ? vero per per tutto l'anno?
ggplot(data=tot, aes(x=tot$giocate_totali, y = tot$numero_macchine)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Pi? macchine ci sono pi? si gioca? Si") +
  labs(x="Giocate Totali", y="Numero di Macchine") +
  theme_classic() #si

cor(tot$giocate_totali, tot$numero_macchine) #0.93 ho scoperto l'acqua calda 

#forse il numero di giocate dipende dal wage minimo
ggplot(data=set, aes(x= set$Denomination, y=set$giocate_totali)) +
  geom_point(alpha=.5, size=3, color="#880011") +
  ggtitle("Pi? monete piccole puoi inserire pi? giochi? No!") +
  labs(x="Wage minimo", y="giocate totali") +
  theme_classic() #ni 

cor(set$Denomination, set$giocate_totali) #bassa dipendenza lineare [non vale la pena]

##################################################################################
#lanciare per conferma. Dopo aver fatto la discretizzazione delle giocate ho potuto fare un chi test pi? veritiero [codice dopo]
# #forse il numero di giocate dipende dal tipo di macchina
# ggplot(data=set, aes(x= set$MachineType, y=set$giocate_unitarie)) +
#   geom_point(alpha=.5, size=3, color="#880011") +
#   ggtitle("I tipi di macchina influenzano le giocate? No!") +
#   labs(x="Wage minimo", y="giocate unitarie") +
#   theme_classic() #mmm no
# 
# #Volevo testare la dipendenza: ora non so se il Chi quadro sia adatto (non ho 2 categoriali)
# #ho usato il modello lineare per testare la significativit? di un tipo rispetto all'altro
# table(set$MachineType, set$giocate_unitarie)
# chisq.test(set$MachineType, set$giocate_unitarie)
# m = lm (giocate_unitarie ~ MachineType, data = set)
# summary(m) #no significativo
# 
# #forse il numero di giocate dipende dalla categoria di macchina
# ggplot(data=set, aes(x= set$tipo, y=set$giocate_unitarie)) +
#   geom_point(alpha=.5, size=3, color="#880011") +
#   ggtitle("La categoria della macchina influenza la giocate? No!") +
#   labs(x="Wage minimo", y="giocate unitarie") +
#   theme_classic() #mmm no

# #stesso discorso di sopra
# chisq.test(set$tipo, set$giocate_unitarie)
# m1 = lm (giocate_unitarie ~ tipo, data = set)
# summary(m1) #difficile interpretazione 
#################################################################################
corrplot::corrplot.mixed(cor(set[,c("Denomination", "numero_macchine", "ricavi_totali", "giocate_totali")]))
#potremmo mettere un vincolo che si basi sulle giocate unitarie;
#se le giocate unitarie sono alte -> preferenza dei consumatori -> numero macchine deve essere >= di una certa soglia;
#se le giocate unitarie sono basse -> numero macchine <= di una certa soglia 

set$Casino = as.factor(set$Casino)

p = ggplot(set, aes(x=Casino, y=giocate_totali, fill = Casino)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  labs(y = "Giocate toali") +
  ggtitle("Distribuzione giocate totali settembre") +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme_dark()

data_summary <- function(x) {
  "funzione per mettere intervallo di confidenza attorno alla media"
  m = mean(x)
  ymin = t.test(x)$conf.int[1]
  ymax = t.test(x)$conf.int[2]
  return(c(y=m,ymin=ymin,ymax=ymax))
}

p + stat_summary(fun.data = data_summary, 
                 geom="pointrange",
                 color="red", 
                 size = 0.7)

t.test(set$giocate_totali ~ set$Casino)
#distribuzioni significativamente diverse: non posso usare la congiunta
  
summary(set[set$Casino == "Aries", "giocate_totali"])
summary(set[set$Casino == "Libra", "giocate_totali"])

help("discretize_df")

#dataframe in input per la discretizzaztione;
#si pu? usare il comdando "discretize_get_bins" ma non permette il settaggio con i quantili
#il risultato sarebbe lo stesso daframe con valori chiaramenti diversi

d_bins = data_frame(variable = "giocate_totali",
                cuts = paste0("122810","|","601962","|","1665180","|","inf")) #da summary precedente
aries_discrettizato = discretize_df(set %>% filter(Casino=="Aries"),
                                  data_bins = d_bins,
                                  stringsAsFactors = T)

d_bins1 = data_frame(variable = "giocate_totali",
                    cuts = paste0("80424","|","209740","|","617728","|","inf")) #da summary precedente
libra_discrettizato = discretize_df(set %>% filter(Casino=="Libra"),
                                    data_bins = d_bins1,
                                    stringsAsFactors = T)
set_discretizzato = rbind(aries_discrettizato, libra_discrettizato)

set$giocate_totali_discr = set_discretizzato$giocate_totali
set = set[c(1:9,13,10:12)] #riordino colonne 

#ora raggruppo per giocate_unitarie che sono state discretizzate e calcolo alcune statistiche per decidere la soglia
set %>% 
  group_by(giocate_totali_discr, Casino) %>%
  summarise(media_macchine = mean(numero_macchine),
            massimo_macchine = max(numero_macchine),
            mediana_macchine = median(numero_macchine)) %>%
  arrange(Casino) %>%
  View()
#utilizziamo il massimo numero di macchine

#PROBLEMONE ENORME: SE USO LA BINARIZZAZIONE SULLE NUOVE VARIABILI CATEGORICHE [QUELLE BINARIZZATE] PURTROPPO MI SEGNA 1
#LADDOVE C'E' LA CATEGORIA: NOI VORREMMO AVERE 1 SE QUELLA VARIABILE APPARTIENE ALLA CATEGORIA E ZERO ALTROVE, IN MODO
#DA AVERE 78 VARIABILI DOVE SOLO IN UN CAMPO ? 1 E NEI RESTANTI ? 0: se non hai capito chiamami
#se lanci il codice sottostante lo capisci meglio comunque [il codice non ritorna quello che vogliamo ma lo lascio acceso
#per far capire]
Vincolo4 = function(df) {
  A = predict(dummyVars(~ giocate_totali_disc, data = df), newdata = df)
  A = t(A)
  return(A)
}

#Vincolo 4
A2 = Vincolo4(set)

#########
#PROVA COMO PER RISOLVERE IL PROBLEMA
########

#1 creo una mtrice diagonale 78 x 78: si adatta alla selezione delle nostre variabili
prova = diag(78)
prova = cbind(prova, as.character(set$giocate_totali_discr))
#2replico ciascuna riga a priori
prova = prova[rep(seq_len(nrow(prova)), each= 2),]
#3 elimino le righe che non dovrebbero essere doppie poich? hanno solamente un lower bound

#Altro problema: siccome vogliamo fissare dei lower e dei upper bound, la matrice deve essere sdobbiata ricordi? bene teoricamente
#non tutte le righe andrebbero raddoppiate ma solo quelle che hanno vincoli sia upper che lower, ergo quelle tutte ad esclusione
#dell'ultima classe (valore, inf) sia per LIBRA che per ARIES

#####################################
# Non va bene perch? ? sbagliato quello sopra 
# #Sistemo la matrice in accordo con la predisposizione del vincolo [raddoppio le righe che hanno sia lower che gli upper]
# A2 = A2[rep(seq_len(nrow(A2)), each= 2),]
# A2 = A2[c(1:7,9:15),]
# A = rbind(A, A2)
# #Vettore B: aggiungo i valori massimi di macchine per categoria di giocate
# b = c(b,
#       1,9,9,22,22,26,101,
#       1,3,3,5,5,9,34)
# constraints = c(constraints,
#                 ">=","<", ">=", "<", ">=", "<", ">=",
#                 ">=","<", ">=", "<", ">=", "<", ">="
#                 )
# sol <- solveLP(f_obj, b, A, maximum = TRUE, constraints)
# summary(sol)
# 
# shadow_price = sol$con
# 
# #risultato sul dataset
# set[which(sol$solution != 0),] %>%
#   arrange(Casino, tipo) %>%
#   View()
################################################################################################


#CONSIDERAZIONE SUCCESSIVA
#ora che abbiamo raggruppato in categorie le giocate, possiamo valutare se quest'ultilme dipendono dalla categoria
#si potrebbere dedurre una certa preferenza dei consumatori

###########################################################################
#come potrebbe essermi utile per i vincoli?
#numero di macchine diverso in funzione della categoria ma che rispecchi le giocate totali 
# #ora che abbiamo le categorie vediamo il chi test
chisq.test(set_discretizzato$giocate_totali, set_discretizzato$tipo)
#p_value bassisimo, si rifiuta l'ipotesi nulla di indipendenza: la categoria di macchina influenza le giocate totali
#che vincolo potrei mettere?
###########################################################################