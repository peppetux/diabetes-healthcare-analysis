#CODICE PROGETTO
diabetes_dataset=read.csv('diabetes_dataset.csv',header=TRUE)
attach(diabetes_dataset)
str(diabetes_dataset)
# Creazione dataset filtrato
dataset_filtrato = subset(
  diabetes_dataset,
  diabetes_stage %in% c("No Diabetes", "Pre-Diabetes", "Type 2")
)

#distribuzione sesso
frequenze_sesso=table(gender)
freq_rel_sesso=prop.table(frequenze_sesso)
pct<-round(freq_rel_sesso*100,1)
lbls<-paste(pct,"%",sep="")
pie(freq_rel_sesso, main="Genere", labels = lbls, col=c("mediumorchid1", "dodgerblue","grey"))
labs<-c("Donne", "Uomini","Altro")
legend(1.0, 1.0, cex = 0.8, legend=labs, fill = c("mediumorchid1", "dodgerblue","grey"))

#distribuzione età
eta=cut(age,seq(1,100,by=10))
# Creazione classi di età per analisi Chi-quadro
eta2 = cut(age, breaks = seq(1, 91, by = 10))
frequenze_eta=table(eta)
freq_rel_eta=prop.table(frequenze_eta)
barplot(frequenze_eta, xlab="Età", ylab="Freq.assoluta",main="Diagramma a barre dell'età",col="red")

#distribuzione reddito
reddito=factor(income_level,levels = c("Low","Lower-Middle","Middle","Upper-Middle","High"))
frequenze_reddito=table(reddito)
freq_rel_reddito=prop.table(frequenze_reddito)
colore_reddito=c("darkolivegreen","darkorange4","darkgoldenrod","burlywood3","antiquewhite")
pct<-round(freq_rel_reddito*100,1)
lbls<-paste(pct,"%",sep="")
pie(freq_rel_reddito, main="Reddito", labels = lbls, col =colore_reddito )
labs<-c("Low","Lower-Middle","Middle","Upper-Middle","High")
legend(1.1, 1.05, cex = 0.6, legend=labs, fill =colore_reddito)

#analisi descrittiva variabili quantitative

#hba1c
hist(hba1c,col='purple2', main ='hba1c',xlab='Emoglobina glicata',ylab='Frequenza')
summary(hba1c)
sd(hba1c)

#glucosio a digiuno
hist(glucose_fasting,col='royalblue3', main ='Glucose fasting',xlab='Glicemia a digiuno',ylab='Frequenza')
summary(glucose_fasting)
sd(glucose_fasting)

#indice di massa corporea
hist(bmi,col='cyan', main ='BMI',xlab='Indice di massa corporea',ylab='Frequenza')
summary(bmi)
sd(bmi)

#trigliceridi
hist(triglycerides,col='yellow1', main ='Triglycerides',xlab='Trigliceridi',ylab='Frequenza')
summary(triglycerides)
sd(triglycerides)

#attività fisica 
hist(physical_activity_minutes_per_week,col='green',main='Attività fisica',xlab='Attività fisica(minuti/settimana) ',ylab='Frequenza')
summary(physical_activity_minutes_per_week)
sd(physical_activity_minutes_per_week)

#colesterolo
hist(cholesterol_total,col='magenta1', main ='Cholesterol total',xlab='Colesterolo',ylab='Frequenza')
summary(cholesterol_total)
sd(cholesterol_total)

#variabili target
#distribuzione stadi del diabete
frequenze_stadi=table(diabetes_stage)
freq_rel_stadi=prop.table(frequenze_stadi)
barplot(frequenze_stadi, ylab="Freq.Assoluta",main="Stadi del diabete",col="grey")

#distribuzione diabete
freq_diabete=table(diagnosed_diabetes)
freq_rel_diab=prop.table(freq_diabete)
barplot(freq_diabete, xlab="Diabete", ylab="Freq.Assoluta",main="Diabete",col="grey")

#analisi bivariata

#quantitative-qualitative  

#glucosio a digiuno-diagnosi del diabete
tapply(glucose_fasting,diagnosed_diabetes,mean)
#       0        1 
#102.6101 116.7889 
model_anova1=anova(lm(formula = glucose_fasting ~ diagnosed_diabetes ))
model_anova1
#significativo 
#grafico diabete e glucosio a digiuno
library(ggplot2)
nomi_diagnosed_diabetes = factor(diagnosed_diabetes,levels = c(0, 1),labels = c("No diabete", "Diabete"))

ggplot(diabetes_dataset, aes(x = nomi_diagnosed_diabetes, y = glucose_fasting, fill = nomi_diagnosed_diabetes)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = c("green", "red")) +
  theme_minimal() +
  labs(
    title = "                             ",
    x = "Diagnosi di diabete",
    y = "Glucosio a digiuno (mg/dL)"
  ) +
  guides(fill = "none")

#glucosio postprandial-diabete
tapply(glucose_postprandial,diagnosed_diabetes,mean)
#0.         1
#136.1730 175.9444 
model_anova13=anova(lm(formula= glucose_postprandial ~ diagnosed_diabetes))
#significativo
#grafico diabete  e glucosio postprandiale
ggplot(diabetes_dataset, aes(x = nomi_diagnosed_diabetes, y = glucose_postprandial, fill = nomi_diagnosed_diabetes)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = c("green", "red")) +
  theme_minimal() +
  labs(
    title = "                             ",
    x = "Diagnosi di diabete",
    y = "Glucosio post-prandiale (mg/dL)"
  ) +
  guides(fill = "none")

#hba1c e diabete
tapply(hba1c,diagnosed_diabetes, mean)
#0        1 
#5.843554 6.972295 
model_anova2=anova(lm(formula = hba1c ~ diagnosed_diabetes))
model_anova2
#significativo

#grafico hba1c e diabete
ggplot(diabetes_dataset, aes(x = nomi_diagnosed_diabetes, y = hba1c, fill = nomi_diagnosed_diabetes)) +
  geom_boxplot( outlier.alpha = 0.3) +
  scale_fill_manual(values = c("green", "red")) +
  theme_minimal() +
  labs(
    title = "",
    subtitle = "",
    x = "Diagnosi di diabete",
    y = "HbA1c (%)"
  ) +
  guides(fill = "none")

#glucosio a digiuno-stage
tapply(glucose_fasting,diabetes_stage,mean)
#Gestational  No Diabetes Pre-Diabetes       Type 1       Type 2 
#108.56835     90.05613    105.76897    106.68033    116.79934
model_anova11=anova(lm(formula= glucose_fasting ~ diabetes_stage))
#significativo 
#grafico glucosio a digiuno-stage
ggplot(diabetes_dataset, aes(x = diabetes_stage, y = glucose_fasting, fill = diabetes_stage)) +
  geom_boxplot(outlier.alpha = 0.3) +
  theme_minimal() +
  labs(
    title = "                             ",
    x = "Stadi del diabete",
    y = "Glucosio a digiuno (mg/dL)"
  ) +
  guides(fill = "none")

#glucosio postprandiale-stage
tapply(glucose_postprandial,diabetes_stage,mean)
# Gestational  No Diabetes Pre-Diabetes       Type 1       Type 2 
#159.3058     114.5722     141.5799     153.9180     175.9532 
model_anova14=anova(lm(formula= glucose_postprandial ~ diabetes_stage))
#significativo 

#grafico glucosio postprandiale-stage
ggplot(diabetes_dataset, aes(x = diabetes_stage, y = glucose_postprandial, fill = diabetes_stage)) +
  geom_boxplot(outlier.alpha = 0.3) +
  theme_minimal() +
  labs(
    title = "                             ",
    x = "Stadi del diabete",
    y = "Glucosio postprandiale(mg/dL)"
  ) +
  guides(fill = "none")

#hba1c e stage
tapply(hba1c,diabetes_stage, mean)
#Gestational  No Diabetes Pre-Diabetes       Type 1       Type 2 
#6.465935     5.197990     6.005346     6.339836     6.972617 
model_anova8=anova(lm(formula = hba1c ~ diabetes_stage ))
model_anova8
#significativo

#grafico glucosio a digiuno-stage
ggplot(diabetes_dataset, aes(x = diabetes_stage, y = hba1c, fill = diabetes_stage)) +
  geom_boxplot(outlier.alpha = 0.3) +
  theme_minimal() +
  labs(
    title = "                             ",
    x = "Stadi del diabete",
    y = "Hba1c (mg/dL)"
  ) +
  guides(fill = "none")


#distribuzioni doppie qualitativa-qualitativa
#età-diagnosi diabete
prob_diab=tapply(diagnosed_diabetes,eta2,mean)
#(11,21]   (21,31]   (31,41]   (41,51]   (51,61]   (61,71]   (71,81]   (81,91] 
#0.4537210 0.5003471 0.5458149 0.5843379 0.6263444 0.6708990 0.7159890 0.7335513
chisq.test(diagnosed_diabetes, eta2, correct = FALSE)
#significativo 
df_prob <- data.frame(
  Classe_eta = names(prob_diab),
  Prob_diabete = as.numeric(prob_diab)
)

ggplot(df_prob, aes(x = Classe_eta, y = Prob_diabete)) +
  geom_col(fill = "red") +
  labs(
    title = "                        Probabilità di diabete per classe di età",
    x = "Classe di età",
    y = "Probabilità di diabete"
  )

#tipo di diabete-genere
tdiab_gen=table(dataset_filtrato$gender, dataset_filtrato$diabetes_stage)
prop.table(tdiab_gen)

ggplot(dataset_filtrato, aes(dataset_filtrato$diabetes_stage)) + 
  geom_bar(width = 0.4, aes(fill = gender),position="dodge") + 
  scale_fill_manual(values = c(
    "Female" = "mediumorchid1", 
    "Male" = "dodgerblue",     
    "Other" = "grey"      
  )
  )
#distribuzione abbastanza equa

#tipo di diabete-reddito
reddito_filtrato= factor(dataset_filtrato$income_level, levels = c("Low","Lower-Middle","Middle","Upper-Middle","High"))
tdiab_red=table(reddito_filtrato, dataset_filtrato$diabetes_stage)
prop.table(tdiab_red)
summary(tdiab_red)
#p-value 0.08 non significativo
ggplot(dataset_filtrato, aes(dataset_filtrato$diabetes_stage)) + 
  geom_bar(width = 0.4, aes(fill = reddito_filtrato),position="dodge")+ 
  scale_fill_manual(values = c(
    "Low" = "darkolivegreen", 
    "Lower-Middle" = "darkorange4",     
    "Middle" = "darkgoldenrod" ,
    "Upper-Middle" = "burlywood3",
    "High" = "antiquewhite"
  )
  )
#distribuzione regolare 

#analisi esplorativa
library(dplyr)
library(NbClust)

#creo un dataset con solo variabili quantitative e ne prendo un campione
diabete_quantitativo = select_if(diabetes_dataset, is.numeric)
diab_quant_ridotto =sample_frac(diabete_quantitativo,0.01,replace=TRUE)

#legame singolo
ResClusteringsingolo = NbClust(data = scale(diab_quant_ridotto),  distance = "euclidean", 
                               min.nc = 2, max.nc = 5, method = "single", index = "all", alphaBeale = 0.0)
ResClusteringsingolo$Best.nc
table(ResClusteringsingolo$Best.nc[1,])
g_bestsingolo=ResClusteringsingolo$Best.partition
table(g_bestsingolo)
#g_bestsingolo
#1   2 
#999   1 
#questa è la distribuzione del legame singolo, distribuzione sbilanciata

#legame completo 
ResClusteringcompleto = NbClust(data = scale(diab_quant_ridotto),  distance = "euclidean", 
                                min.nc = 2, max.nc = 5, method = "complete", index = "all", alphaBeale = 0.1)
ResClusteringcompleto$Best.nc
table(ResClusteringcompleto$Best.nc[1,])
g_bestcompleto=ResClusteringcompleto$Best.partition
table(g_bestcompleto)
#la maggior parte degli indicatori segnano come numero ottimale di gruppi 3
#il risultato viene anche confermato dalla regola del gomito 
#la proporzione dei gruppi è bilanciata 
#g_bestcompleto
#1   2   3 
#317 481 202 

#legame medio 
ResClusteringmedio = NbClust(data = scale(diab_quant_ridotto),  distance = "euclidean", 
                             min.nc = 2, max.nc = 5, method = "average", index = "all", alphaBeale = 0.0)
ResClusteringmedio$Best.nc
table(ResClusteringmedio$Best.nc[1,])
g_bestmedio=ResClusteringmedio$Best.partition
table(g_bestmedio)
#distribuzione estremamente sbilanciata
# 1   2 
#998  2 

#l'unica risposta valida è quella che da 3 cluster come numero ottimale 

#analisi confermativa-predittiva (spark)

library(sparklyr)

sc = spark_connect(master="local")
diabete_stand=mutate_if(dataset_filtrato,is.numeric, scale)
str(diabete_stand)
diabete_spark = copy_to(sc, diabete_stand, "diabete_spark", overwrite = TRUE)
kmeans_model = diabete_spark %>%
  ml_kmeans(k=3, features = c("glucose_postprandial","bmi","cholesterol_total","triglycerides","glucose_fasting","hba1c"))

output1 = ml_summary(kmeans_model)
output1$predictions%>%
  print(n=150)
count(output1$predictions)
output1$cluster_sizes()
# 35990 34280 29330
# 7981        31845        59774-->dimensioni reali dei gruppi 

partitions_diabete <- diabete_spark %>%
  sdf_random_split(training = 0.7, test = 0.3, seed = 1111)
diabete_training <- partitions_diabete$training
diabete_test <- partitions_diabete$test

count(diabete_test) 
#29983

diabete_group= diabete_training %>%
  group_by(diabetes_stage) %>%
  count()%>%
  collect() %>%
  print()
#1 Type 2         41803
#2 Pre-Diabetes   22193
#3 No Diabetes     5621

diabete_group2= diabete_test %>%
  group_by(diabetes_stage) %>%
  count()%>%
  collect() %>%
  print()
#Type 2         17971
# Pre-Diabetes    9652
# No Diabetes     2360

kmeans_model2 = ml_kmeans(diabete_training, k = 3, diabetes_stage ~ .) 

output2 = ml_summary(kmeans_model2)
output2$predictions%>%
  print(n=100)
output2$cluster_sizes()
#28341 22442 18834

tidy(kmeans_model2)

predicted2 <- ml_predict(kmeans_model2, diabete_test) %>%
  collect()

table(predicted2$diabetes_stage)
table(predicted2$label)
table(predicted2$prediction)

#0=tipo 2, 1=pre-diabete, 2=no diabete

tabellaConfusione =table(predicted2$label, predicted2$prediction)
colnames(tabellaConfusione) <- c("tipo 2", "prediabete", "sano")
rownames(tabellaConfusione) <- c("tipo 2", "prediabete", "sano")
tabellaConfusione

#.          tipo 2 prediabete  sano
#tipo 2      12092       4638  1241
#prediabete    116       4064  5472
#sano            0       1073  1287

#questo modello ne sbaglia molti 

#proviamo a ridurre il numero di variabili, considerando solo quelle più significative 

# metodo delle k-medie usando due variabili 
kmeans_model3 = diabete_training %>%
  ml_kmeans(k = 3, diabetes_stage ~ hba1c + glucose_fasting)
output3 = ml_summary(kmeans_model3)

predicted3 <- ml_predict(kmeans_model3, diabete_test) %>%
  collect()
table(predicted3$diabetes_stage)
table(predicted3$label)
table(predicted3$prediction)

tabConfusione2 = table(predicted3$label, predicted3$prediction)
colnames(tabConfusione2) <- c("tipo 2", "prediabete", "sano")
rownames(tabConfusione2) <- c("tipo 2", "prediabete", "sano")
tabConfusione2

#           tipo 2 prediabete sano
#tipo 2       8716       7884 1371
#prediabete   5089          0 4563
#sano            0          0 2360

library(ggplot2)
predicted3 %>%
  ggplot(aes(hba1c, glucose_fasting)) +
  geom_point(aes(hba1c, glucose_fasting, col = factor(prediction + 1)),
             size = 2, alpha = 0.5
  ) +
  geom_point(
    data = kmeans_model3$centers, aes(hba1c, glucose_fasting),
    pch = "x", size = 12
  ) +
  scale_color_discrete(
    name = "Gruppi previsti",
    labels = c("Prediabete","Diabete","No Diabete")
  ) +
  labs(
    x = "hba1c",
    y = "glucosio a digiuno"
  )
