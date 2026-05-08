################################################################################
################## ADB Final Project: Enzyme Kinetics Inhibitors ###############
################################################################################


# R packages used throughout our code (Each was installed beforehand)

library(rstatix)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(patchwork)
library(dplyr)
library(ggpubr)
library(caret)
library(gtsummary)
library(broom)
library(knitr)
library(kableExtra)
library(report)

# Setting a Working Directory to later load the .csv file.

setwd("~/Documents/MBC/1st_Year/1st Semester/Biological Big Data Analytics/ADB_Project")

# or (How each group member set up their working directory in their computer)

#setwd("C:/Users/anaan/OneDrive - Universidade de Aveiro/Attachments/Desktop/OneDrive - Universidade de Coimbra/ADB/Projeto")

# Importing the data into RStudio

Enzyme_KI = read.csv('enzyme_kinetics_inhibitors.csv'); Enzyme_KI

# Finding out Enzyme_KI columns, dimensions, and some statistical data:-

head(Enzyme_KI)
dim(Enzyme_KI)
str(Enzyme_KI) 
summary(Enzyme_KI)

# Finding out the different inhibitor types in the data:- 

unique(Enzyme_KI$inhibitor_type) 

# There are "Competitive","None","Uncompetitive" and "Noncompetitive" inhibitors.


# Performing some statistical analysis for kinetic parameters per inhibitor type. 

kinetic_parameters <- Enzyme_KI %>%
  group_by(inhibitor_type) %>%
  summarise(
    Vmax = mean(vmax_app),
    Km = mean(km_app),
    Vmax_sd = sd(vmax_app),
    Km_sd = sd(km_app),
    count = n()) %>%
  as.data.frame(); kinetic_parameters

# Separating relevant parameters per inhibitor_type into various data frames for plotting.

CI <- Enzyme_KI %>%
  filter(inhibitor_type == "Competitive") %>%
  select(substrate_mM,velocity_uM_min,vmax_app,km_app) %>%
  as.data.frame(); CI

UCI <- Enzyme_KI %>%
  filter(inhibitor_type == "Uncompetitive") %>%
  select(substrate_mM,velocity_uM_min, vmax_app, km_app)%>%
  as.data.frame(); UCI

NCI <- Enzyme_KI %>%
  filter(inhibitor_type == "Noncompetitive") %>%
  select(substrate_mM,velocity_uM_min, vmax_app, km_app)%>%
  as.data.frame(); NCI

NI <- Enzyme_KI %>%
  filter(inhibitor_type == "None") %>%
  select(substrate_mM,velocity_uM_min,vmax_app,km_app)%>%
  as.data.frame(); NI

################## Michaelis-Menten Plots per Inhibitor Type ###################

#1- No Inhibition Michaelis-Menten Plot

NI_Plot <- ggplot(NI, aes(x=substrate_mM, y= velocity_uM_min))+ geom_point(size = 1 , color = "gray57") + 
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  geom_hline(aes(yintercept = mean(vmax_app) ), color = "purple", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = mean(km_app) ), color = "#4169E1", linetype = "dashed", linewidth = 1) +
  labs(title = "No Inhibition", x = "Substrate Concentration (mM)", y = "Velocity (μM/min)") +
  coord_cartesian(xlim = c(0,17), ylim= c(0,100)) + theme_minimal() + 
  annotate("text", x = 16, y = mean(NI$vmax_app) , 
           label = "Mean Vmax", color = "purple", size = 3.5) +
  annotate("text", y = mean(NI$km_app) ,  x = 4.7, 
           label = "Mean Km", color = "#4169E1", size = 3.5) +
  theme(plot.title = element_text(size = 17, face = "plain",
                                  family = "sans" , color = "mediumorchid4", hjust = 0.5))
NI_Plot

#2- Competitive Inhibition Michaelis-Menten Plot

CI_Plot <- ggplot(CI, aes(x=substrate_mM, y= velocity_uM_min))+ geom_point(size = 1 , color = "gray57") + 
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  geom_hline(aes(yintercept = mean(vmax_app)), color = "purple", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = mean(km_app)), color = "#4169E1", linetype = "dashed", linewidth = 1) +
  labs(title = "Competitive inhibition", x = "Substrate Concentration (mM)", y = "Velocity (μM/min)") +
  coord_cartesian(xlim = c(0,17), ylim= c(0,100)) + theme_minimal() + 
  annotate("text", x = 16, y = mean(CI$vmax_app) - 5, 
           label = "Mean Vmax", color = "purple", size = 3.5) +
  annotate("text", y = mean(CI$km_app) + 1,  x = 4.7, 
           label = "Mean Km", color = "#4169E1", size = 3.5) +
  theme(plot.title = element_text(size = 17, face = "plain",
                                  family = "sans" , color = "mediumorchid4", hjust = 0.5))
CI_Plot

#3- Uncompetitive Inhibition Michaelis-Menten Plot

UCI_Plot <- ggplot(UCI, aes(x=substrate_mM, y= velocity_uM_min))+ geom_point(size = 1 , color = "gray57") + 
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  geom_hline(aes(yintercept = mean(vmax_app)), color = "purple", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = mean(km_app)), color = "#4169E1", linetype = "dashed", linewidth = 1) +
  labs(title = "Uncompetitive Inhibition", x = "Substrate Concentration (mM)", y = "Velocity (μM/min)") +
  coord_cartesian(xlim = c(0,17), ylim= c(0,100)) + theme_minimal() + 
  annotate("text", x = 16, y = mean(UCI$vmax_app) - 5, 
           label = "Mean Vmax", color = "purple", size = 3.5) +
  annotate("text", y = mean(UCI$km_app) + 1,  x = 4.7, 
           label = "Mean Km", color = "#4169E1", size = 3.5) +
  theme(plot.title = element_text(size = 17, face = "plain",
                                  family = "sans" , color = "mediumorchid4", hjust = 0.5))
UCI_Plot

#4- Noncompetitive Inhibition Michaelis-Menten Plot

NCI_Plot <- ggplot(NCI, aes(x=substrate_mM, y= velocity_uM_min))+ geom_point(size = 1 , color = "gray57") + 
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  geom_hline(aes(yintercept = mean(vmax_app)), color = "purple", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = mean(km_app)), color = "#4169E1", linetype = "dashed", linewidth = 1) +
  labs(title = "Noncompetitive Inhibition", x = "Substrate Concentration (mM)", y = "Velocity (μM/min)") +
  coord_cartesian(xlim = c(0,17), ylim= c(0,100)) + theme_minimal() + 
  annotate("text", x = 16, y = mean(NCI$vmax_app) - 5, 
           label = "Mean Vmax", color = "purple", size = 3.5) +
  annotate("text", y = mean(NCI$km_app) + 1,  x = 4.7, 
           label = "Mean Km", color = "#4169E1", size = 3.5) +
  theme(plot.title = element_text(size = 17, face = "plain",
                                  family = "sans" , color = "mediumorchid4", hjust = 0.5))
NCI_Plot

# Comparing different inhibitors against no inhibition (or the "None" data)

NI_CI_Comparison <- (NI_Plot | CI_Plot); NI_CI_Comparison
NI_NCI_Comparison <- (NI_Plot | NCI_Plot); NI_NCI_Comparison
NI_UCI_Comparison <- (NI_Plot | UCI_Plot); NI_UCI_Comparison

# Plots are shown separately rather than combined, as overlapping points from 
# different inhibitor types would have made visual interpretation more difficult.



# *Extra* Normalized Michaelis–Menten Plot

# Normalizing No Inhibition Data
NI_Vel_Mean <- mean(NI$velocity_uM_min);NI_Vel_Mean
NI_Sub_Mean <- mean(NI$substrate_mM);NI_Sub_Mean
NI_norm <- NI %>%
  mutate(velocity_uM_min = velocity_uM_min / NI_Vel_Mean, substrate_mM = substrate_mM / NI_Sub_Mean)
# Normalizing Competitive Inhibition Data
CI_norm <- CI %>%
  mutate(velocity_uM_min = velocity_uM_min / NI_Vel_Mean, substrate_mM = substrate_mM / NI_Sub_Mean)
# Normalizing Noncompetitive Inhibition Data
NCI_norm <- NCI %>%
  mutate(velocity_uM_min = velocity_uM_min /NI_Vel_Mean, substrate_mM = substrate_mM / NI_Sub_Mean)
# Normalizing Uncompetitive Inhibition Data
UCI_norm <- UCI %>%
  mutate(velocity_uM_min = velocity_uM_min /NI_Vel_Mean, substrate_mM = substrate_mM / NI_Sub_Mean)

Normalized_Data <- bind_rows(
  NI_norm  %>% mutate(Inhibition = "None"),
  CI_norm  %>% mutate(Inhibition = "Competitive"),
  NCI_norm %>% mutate(Inhibition = "Noncompetitive"),
  UCI_norm %>% mutate(Inhibition = "Uncompetitive"))

# Normalized Michaelis–Menten Plot

Normalized_MM_Plot <- ggplot(Normalized_Data, aes(x = substrate_mM, y = velocity_uM_min, color = Inhibition)) +
  geom_smooth(method = "loess", se = F) +
  labs(x = "Substrate Concentration (mM)",y = "Velocity (μM/min)",title = "Normalized Michaelis–Menten Plot") +
  theme_minimal() + coord_cartesian(xlim = c(0,4), ylim= c(0,2)) + theme_minimal() + 
  theme(plot.title = element_text(size = 17, face = "plain",
                                  family = "sans", color = "mediumorchid4", hjust = 0.5))

Normalized_MM_Plot

########## Visualising Kinetic Parameters by Inhibitor Type: Boxplots ##########

#1- Temperature (°C) Boxplot



ANOVA_Temp <- aov(temperature_C ~ inhibitor_type, data = Enzyme_KI)


report(ANOVA_Temp)


# 3. Check if residuals are independent and come from a normal distribution.


ANOVA_Temp_res <- residuals(object = ANOVA_Temp)
ANOVA_Temp_res

shapiro.test(x = ANOVA_Temp_res) 

#Histogram, boxplot and qqplot


b1 <- ggplot(aov_residuals, aes(y = residuals.object...ANOVA_Temp.)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = 8, 
               outlier.size = 2) +  
  labs(title = "Box Plot", y = NULL) +
  theme(plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))
b1
# we want to look for symmetry; if boxplot is symmetric maybe the sample comes from a normal distribution

# Histogram:
h1 <- ggplot(data = aov_residuals, aes(x = residuals.object...ANOVA_Temp.)) +
  geom_histogram(fill = "gray", color = "black", position = 'identity') + 
  labs(title = "Histogram", y = "Frequency", x = "Temperature (°C)") +
  theme(plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))
h1
# Q-Q Plot (Quartis):
q1 <- ggplot(aov_residuals, aes(sample = residuals.object...ANOVA_Temp.)) +  
  geom_qq(fill = "gray", color = "black") + stat_qq() +
  geom_qq_line(color = "black") + labs(title = "Normal Q-Q Plot", 
                                       y = "Sample Quartiles", x = "Theoretical Quartiles") +
  theme(plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))
q1

t = h1 | b1 | q1
t
par(mfrow = c(1, 3))
hist(aov_residuals)
boxplot(aov_residuals)
qqnorm(aov_residuals)
qqline(aov_residuals)


shapiro.test(x = aov_residuals) 

TukeyHSD(ANOVA_Temp)


anova_result_Temp <- Enzyme_KI %>% 
  anova_test(temperature_C ~ inhibitor_type)
anova_result_Temp


Enzyme_KI %>% 
  rstatix::levene_test(temperature_C ~ inhibitor_type) %>%
  as.data.frame() 



stats_Temp <- Enzyme_KI %>%
  tukey_hsd(temperature_C ~ inhibitor_type) %>%
  add_y_position(step.increase = 0.05 * max(Enzyme_KI$temperature_C, na.rm = TRUE))
stats_Temp

Temp_Boxplot <- ggplot(Enzyme_KI) + 
  aes(x = factor(inhibitor_type, 
                 levels = c("None","Competitive", "Noncompetitive", "Uncompetitive")), 
      y = temperature_C, fill = inhibitor_type) + geom_boxplot(show.legend = F) +
  labs(title = "Temperature Comparison Between Inhibitor Types", y = "Temperature (°C)", 
       x = "Inhibitor Type") + theme_minimal() +
  theme(plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))

Temp_Boxplot


#2- Maximum Reaction Rate (Vmax (μM/min)) Boxplot 

predictor <- Enzyme_KI %>%
  dplyr::select(-inhibitor_type) 

pp <- preProcess(predictor, method = c("BoxCox"))

x_trans <- predict(pp, predictor)

transformed <- cbind(x_trans, inhibitor_type = Enzyme_KI$inhibitor_type)

head(Enzyme_KI,n=2)
head(transformed,n=2)

str(transformed)

Enzyme_KI %>% 
  rstatix::levene_test(vmax_app ~ inhibitor_type) %>%
  as.data.frame() 

ANOVA_Vmax <- aov(vmax_app ~ inhibitor_type, data = Enzyme_KI)


report(ANOVA_Vmax)


# 3. Check if residuals are independent and come from a normal distribution.

ANOVA_Vmax_res <-residuals(object = ANOVA_Vmax)
ANOVA_Vmax_res
colnames(ANOVA_Vmax_res)
shapiro.test(x = ANOVA_Vmax_res)


b1 <- ggplot(ANOVA_Vmax_res, aes(y = residuals.object...ANOVA_Vmax.)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = 8, 
               outlier.size = 2) +  
  labs(title = "Box Plot", y = NULL) +
  theme(plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))
b1
# we want to look for symmetry; if boxplot is symmetric maybe the sample comes from a normal distribution

# Histogram:
h1 <- ggplot(data = ANOVA_Vmax_res, aes(x = residuals.object...ANOVA_Vmax.)) +
  geom_histogram(fill = "gray", color = "black", position = 'identity') + 
  labs(title = "Histogram", y = "Frequency", x = "Temperature (°C)") +
  theme(plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))
h1
# Q-Q Plot (Quartis):
q1 <- ggplot(ANOVA_Vmax_res, aes(sample = residuals.object...ANOVA_Vmax.)) +  
  geom_qq(fill = "gray", color = "black") + stat_qq() +
  geom_qq_line(color = "black") + labs(title = "Normal Q-Q Plot", 
                                       y = "Sample Quartiles", x = "Theoretical Quartiles") +
  theme(plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))
q1

t = h1 | b1 | q1
t

#Histogram, boxplot and qqplot
par(mfrow = c(1, 3))
hist(ANOVA_Vmax_res)
boxplot(ANOVA_Vmax_res)
qqnorm(ANOVA_Vmax_res)
qqline(ANOVA_Vmax_res)

# Run Shapiro-Wilk test - normality test - different way of checking normality
shapiro.test(x = ANOVA_Vmax_res) # Normally distributed data

tukey_result <- TukeyHSD(ANOVA_Vmax)

# Extract the table
tukey_table <- as.data.frame(tukey_result$inhibitor_type)

# Add a column with p-values formatted in scientific notation
tukey_table$p_adj_sci <- sprintf("%.2e", tukey_table$`p adj`)

# View table
tukey_table


anova_result_Vmax <- Enzyme_KI %>% 
  anova_test(vmax_app ~ inhibitor_type)
anova_result_Vmax

stats_Vmax <- Enzyme_KI %>%
  tukey_hsd(vmax_app ~ inhibitor_type) %>%
  add_y_position(step.increase = 0.01 * max(Enzyme_KI$temperature_C, na.rm = TRUE))   
stats_Vmax

Vmax_Boxplot <- ggplot(Enzyme_KI) + 
  aes(x = factor(inhibitor_type, 
                 levels = c("None","Competitive", "Noncompetitive", "Uncompetitive")), 
      y = vmax_app, fill = inhibitor_type) + geom_boxplot(show.legend = F) +
  labs(title = "Maximum Reaction Rate", y = "Vmax (μM/min)", 
       x = "Inhibitor Type") + theme_minimal() +
  stat_pvalue_manual(stats_Vmax, label = "p.adj.signif", tip.length = 0.01) +
  theme(plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))

Vmax_Boxplot

#3- Enzyme-substrate Affinity Boxplot (Km (mM))

##########################################################


predictor <- Enzyme_KI %>%
  dplyr::select(-inhibitor_type) 

pp <- preProcess(predictor, method = c("scale","center"))

x_trans <- predict(pp, predictor)

transformed <- cbind(x_trans, inhibitor_type = Enzyme_KI$inhibitor_type)

Enzyme_KI2 <- Enzyme_KI %>%
  dplyr::select(km_app, vmax_app, inhibitor_type) %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::select(id, everything())


Enzyme_KI2 %>%
  dplyr::select(-id) %>%
  reshape2::melt(id="inhibitor_type")%>% 
  ggplot(aes(x = inhibitor_type, y = value, fill = variable)) + 
  geom_boxplot() +
  facet_grid(~variable)+
  theme(legend.position = 'bottom')  +
  labs(title = "Distribution of Km and Vmax by Inhibitor Type") 


Enzyme_KI2 %>%
  dplyr::select(-id) %>%
  reshape2::melt(id = "inhibitor_type") %>%
  # Change the variable names for nicer labels
  dplyr::mutate(variable = dplyr::recode(variable,
                                         km_app = "Km",
                                         vmax_app = "Vmax")) %>%
  ggplot(aes(x = inhibitor_type, y = km_app, fill = variable)) + 
  geom_boxplot() +
  facet_grid(~variable, labeller = label_value) +  # facet labels now Km and Vmax
  theme(legend.position = 'bottom') +
  labs(title = "Distribution of Km and Vmax by Inhibitor Type",
       x = "Inhibitor Type",
       y = "Value",
       fill = "Variable")


#Ex C- Compute means and standard deviations for each variable by species.

Enzyme_KI2 %>%
  group_by(inhibitor_type) %>%
  # dplyr::summarise(bal bla bla)
  get_summary_stats(km_app, vmax_app, type = "mean_sd") %>%
  as.data.frame()


#EX D- Identify statistical test to apply

#We have two continuous dependent variables (Sepal.Length and Petal.Length)
#and one categorical independent variable (Species).
#→ Therefore, the appropriate test is a one-way MANOVA.

#EX E- Assumptions

# Adequate sample size

Enzyme_KI %>%
  group_by(inhibitor_type) %>%
  dplyr::summarise((N = n())) %>%
  as.data.frame()

#- A2 - Absense of univariate or multivariate outliers.

# Identify the existence of outliers in the two continuous variables

Enzyme_KI2 %>% 
  group_by(inhibitor_type) %>%
  identify_outliers(km_app)

Enzyme_KI2 %>% 
  group_by(inhibitor_type) %>%
  identify_outliers(vmax_app) 


Enzyme_KI2 %>%
  group_by(inhibitor_type) %>% 
  mahalanobis_distance(-id) %>%  
  dplyr::filter(is.outlier == TRUE) %>%
  as.data.frame()


## A3 - Normality
#- A3 - Univariate and multivariate nornality assumption.

Enzyme_KI2 %>% 
  group_by(inhibitor_type) %>%
  shapiro_test(km_app, vmax_app) %>%
  as.data.frame()

Enzyme_KI2 %>%
  dplyr::select(km_app, vmax_app)%>%
  mshapiro_test() %>%
  as.data.frame()

Enzyme_KI %>% 
  cor_test(km_app, vmax_app) %>%
  as.data.frame() 


#There was no multicollinearity, as assessed by Pearson correlation (r = 0.87, p < 0.0001).

#In the situation, where you have multicollinearity, you could consider removing one of the outcome variables that is highly correlated.
## A5 - Linearity
#- A5 - Linearity between all outcome variables for each group.

Enzyme_KI2 %>%
  ggplot(aes(km_app, vmax_app, color = inhibitor_type)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal(base_size = 13) +
  labs(title = "Linearity between Km and Vmax by Inhibitor Type")+
  facet_grid(~inhibitor_type)

Enzyme_KI2 %>%
  ggplot(aes(x = km_app, y = vmax_app, color = inhibitor_type)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal(base_size = 13) +
  facet_grid(~inhibitor_type, labeller = labeller(inhibitor_type = 
                                                    c(NoneT = "None", 
                                                      CompT = "Competitive", 
                                                      NoncompT = "Non-Competitive", 
                                                      UncompT = "Uncompetitive"))) +
  labs(title = "Linearity Between Km and Vmax by Inhibitor Type",
       x = expression(paste("Km (", mu, "M)")),        # Add units if known, e.g., µM
       y = expression(paste("Vmax (", mu, "M/min)")), # Replace units with correct ones
       color = "Inhibitor Type")


#There was a linear relationship between Sepal.Length and Petal.Length in each Species group, as assessed by scatter plot.

## A6 - Homogeneity of Covariance Matrices 
# This can be evaluated using the Box’s M-test implemented in the rstatix package.

box_m(Enzyme_KI2[, c("km_app", "vmax_app")], Enzyme_KI2$inhibitor_type)
head(Enzyme_KI2, n = 2)


#- A7- Check the homogeneity of variance assumption

# you only new to change the name of the factor variable of interest- Species in this case

Enzyme_KI2%>%
  dplyr::select(-id) %>%
  reshape2::melt(id = "inhibitor_type") %>%
  group_by(variable) %>%
  levene_test(value ~ inhibitor_type) %>%
  as.data.frame()

#Levene’s test was significant (p < 0.05) → the assumption of equal variances across groups was not met.
#Again, Pillai’s Trace is the most robust choice in this scenario.

#Ex F - Apply MANOVA.

#The most commonly recommended multivariate statistic to use is Wilks’ Lambda.

library(broom)


model <- lm(cbind(km_app, vmax_app) ~ inhibitor_type, Enzyme_KI2) 
Manova(model, test.statistic = "Pillai")

manova_model <- manova(cbind(km_app, vmax_app) ~ inhibitor_type, data = Enzyme_KI)
res <- residuals(manova_model)

# Check residual correlation patterns
plot(res)

#Ex G- Post-Hoc Tests
#We follow a statistical significant one-way MANOVA by a univariate one-way ANOVA.

Enzyme_KI2 %>%
  dplyr::select(-id) %>%
  reshape2::melt(id = "inhibitor_type")  %>% 
  group_by(variable) %>%
  anova_test(value ~ inhibitor_type) %>%
  as.data.frame()


Enzyme_KI2 %>%
  dplyr::select(-id) %>%
  reshape2::melt(id = "inhibitor_type")  %>% 
  group_by(variable) %>%
  games_howell_test(value ~ (inhibitor_type))


############################################################

ANOVA_Km <- aov(km_app ~ inhibitor_type, data = Enzyme_KI)


report(ANOVA_Km)

format.pval(your_p_values, scientific = TRUE, digits = 3)


stats_Km <- Enzyme_KI %>%
  tukey_hsd(km_app ~ inhibitor_type) %>%
  add_y_position()  

tukey_hsd(ANOVA_Km)
tukey <- tukey_hsd(ANOVA_Km)
tukey %>%
  mutate(p.adj = format.pval(p.adj, scientific = TRUE, digits = 3))

Km_Boxplot <- ggplot(Enzyme_KI) + 
  aes(x = factor(inhibitor_type, 
                 levels = c("None","Competitive", "Noncompetitive", "Uncompetitive")), 
      y = km_app, fill = inhibitor_type) + geom_boxplot(show.legend = F) +
  labs(title = "Enzyme-substrate Affinity", y = "Km (mM)", 
       x = "Inhibitor Type") + theme_minimal() +
  stat_pvalue_manual(stats_Km, label = "p.adj.signif", tip.length = 0.01) +
  theme(plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))

Km_Boxplot

# Comparing the different Kinetic Parameters by joining them into one plot.

All_Boxplots <- ((Km_Boxplot) | (Vmax_Boxplot) | (Temp_Boxplot)) + 
  plot_layout(ncol = 3, widths = rep(1, 3)); All_Boxplots


# *Extra* Substrate Concentration 

anova_result_substrate_mM <- Enzyme_KI %>% 
  anova_test(substrate_mM ~ inhibitor_type)
anova_result_Km

stats_substrate_mM <- Enzyme_KI %>%
  tukey_hsd(substrate_mM ~ inhibitor_type) %>%
  add_y_position()  

substrate_mM_Boxplot <- ggplot(Enzyme_KI) + 
  aes(x = factor(inhibitor_type, 
                 levels = c("None","Competitive", "Noncompetitive", "Uncompetitive")), 
      y = substrate_mM, fill = inhibitor_type) + geom_boxplot(show.legend = F) +
  labs(title = "Substrate Concentration", y = "Substrate (mM)", 
       x = "Inhibitor Type") + theme_minimal() +
  stat_pvalue_manual(stats_substrate_mM, label = "p.adj.signif", tip.length = 0.01) +
  theme(plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))

substrate_mM_Boxplot

sub_Boxplot <- ggplot(Enzyme_KI, aes(y =  substrate_mM, fill = inhibitor_type)) + 
  labs(title= "Substrate Concentration ", y = "Substrate (mM)", fill = "Inhibitor Type") +
  geom_boxplot(show.legend = F) + facet_grid(~inhibitor_type) + theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "plain",family = "sans" , color = "mediumorchid4", hjust = 0.5)) +
  theme(strip.text = element_text(size = 14, face = "plain",family = "sans"))
sub_Boxplot

# *Extra* Velocity

vel_Boxplot <- ggplot(Enzyme_KI, aes(y =  velocity_uM_min, fill = inhibitor_type)) + 
  labs(title= "Velocity", y = "Velocity (uM/Min)", fill = "Inhibitor Type") +
  geom_boxplot(show.legend = F) + facet_grid(~inhibitor_type) + theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "plain",family = "sans" , color = "mediumorchid4", hjust = 0.5)) +
  theme(strip.text = element_text(size = 14, face = "plain",family = "sans"))
vel_Boxplot

############################## Linear Regression ###############################

#considerando que queremos estudar velocity em relação às outras variáveis

summary(Enzyme_KI)
str(Enzyme_KI)

g1 <- ggplot(Enzyme_KI, aes(x = vmax_app, y = velocity_uM_min)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) 

g1

#retirar as variaveis fatoriais
d1 = select(Enzyme_KI, -inhibitor_type, -id)
#retirar highly correleted

m_cor = cor(d1);m_cor
mt_corr =findCorrelation(m_cor,cutoff = 0.95); mt_corr #não há

#normalizar

#selecionar as variaveis continuas

predictors = select(d1, -velocity_uM_min) # retiro a variável de estudo


pp <- preProcess(predictors, method = c("center", "scale")) # center and scaling is normalized


X_trans <- predict(pp, predictors); X_trans
d_trans <- cbind(X_trans, 
                 id = Enzyme_KI$id, 
                 inhibitor_type = Enzyme_KI$inhibitor_type, 
                 velocity_uM_min = Enzyme_KI$velocity_uM_min)
head(d_trans, n=3)

#calcular a regressão linear para todas as variáveis

model <- lm(velocity_uM_min ~ . , data = d_trans)
summary(model)

##############


tidy(model) %>%
  mutate(
    p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)),
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 2)
  ) %>%
  kable(col.names = c("Variable", "Coefficient", "Std. Error", "t-value", "p-value"),
        caption = "Linear Regression Results: Velocity (μM/min)") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = FALSE,
                font_size = 14) %>%
  row_spec(0, bold = TRUE, color = "white", background = "darkgreen") %>%
  row_spec(c(2,4,5), bold = TRUE, background = "#E7F0FF")  # Highlight significant predictors

#5- Calculate residuals and check if they are normally distributed.
rstandard(model) # errors should follow a normal distribution
# normal distribution- Gaussian curve
# visual ways to check if a distribution is normal
par(mfrow = c(1, 3))
par(mar = c(5,4,2,1)) # I did this because R was complaining about margins
hist(rstandard(model)) # A- R base for histogram
boxplot(rstandard(model)) # B- R base for boxplot
qqnorm(rstandard(model)) # C- R base for qq-plot
qqline(rstandard(model))


res_model <- data.frame(rstandard(model))

b1 <- ggplot(res_model, aes(y = rstandard.model.)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = 8, 
               outlier.size = 2) +  
  labs(title = "BoxPlot", y = NULL) +
  theme(plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))
b1
# we want to look for symmetry; if boxplot is symmetric maybe the sample comes from a normal distribution

# Histogram:
h1 <- ggplot(data = res_model, aes(x = rstandard.model.)) +
  geom_histogram(fill = "gray", color = "black", position = 'identity') + 
  labs(title = "Histogram", y = "Frequency", x = "Velocity (uM/min)") +
  theme(plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))
h1
# Q-Q Plot (Quartis):
q1 <- ggplot(res_model, aes(sample = rstandard.model.)) +  
  geom_qq(fill = "gray", color = "black") + stat_qq() +
  geom_qq_line(color = "black") + labs(title = "Normal Q-Q Plot", 
                                       y = "Sample Quartiles", x = "Theoretical Quartiles") +
  theme(plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))
q1

t = h1 | b1 | q1
t


shapiro.test(rstandard(model)) # o valor de p é menor do que 0.05 logo a distribuição não é normal

#Os residuos não seguem uma distribuição normal


#6- Check if residuals are too high (maximum of 3.3 - this is an empirical rule).
which(rstandard(model) > 3.3)

#7- Check homoscedasticity by 2 ways (rstandard vs fitted values; fitted values vs real ones)

plot(model$fitted.values,rstandard(model)) # R base for a scatter plot

a1 <- ggplot(data = res_model, aes(x = model$fitted.values, y = rstandard(model))) +
  geom_point(color = "black", fill = "gray", shape = 21, size = 3, alpha = 0.7) + 
  labs(
    title = "Fitted vs Observed Values",
    x = "Fitted Values",
    y = "Observed Velocity (uM/min)"
  ) +
  theme(
    plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )
a1

# should gives all the points within a rectangle
#Não segue um padrão retangular, logo não podemos considerar que o a variancia dos valores 
#previstos não é homogénea

# Behavior between the fitted values and the real one (the one that us being predicted weight)
plot(model$fitted.values, d_trans$velocity_uM_min)

a2 <- ggplot(data = res_model, aes(x = model$fitted.values, y = d_trans$velocity_uM_min)) +
  geom_point(color = "black", fill = "gray", shape = 21, size = 3, alpha = 0.7) + 
  labs(
    title = "Fitted vs Real Values",
    x = "Fitted Values",
    y = "Observed Velocity (uM/min)"
  ) +
  theme(
    plot.title = element_text(size = 18, color = "mediumorchid4", hjust = 0.5),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )
a2

p <- a1|a2; p
#Should be closer to y = x so integer(0) is guaranteed.
# OK

#8- Calculate leverages and check if they are higher than leverage > 2(p+1)/n; n is number of rows and p the number of features.

leverages <- lm.influence(model) 
leverages# leverage values; looking which value are influencing the regression
# Leverage is high - empirical rule/rule of thumb - if leverage > 2(p+1)/n
dim(d_trans) # 236 by  16 colums; p = number of variables -1 (the predicted ones); n is the amount of rows (number of observations)
cutoff = 2*(6+1)/1000 # p=6 e n = 1000; leverage cut-off above which the observations are probably important


plot(leverages$hat)
abline(2*(6+1)/1000, 0) # draw a horizontal line 
# Existem muitas observações cuja leverage está acima do cut off

#9- Calculate their influence with Cook´s distance and check if higher than 1.
cooks.distance(model)
plot(cooks.distance(model))
#All observations have a cook´s distance value lower than 1. 
#Although many observations are above leverage cutoff. 
#all have a cook distance lower than 1 should be OK.


#Uma vez que o pressuposto da normalidade dos residuos e da homogeneidade de variancias foram violados, 
#o modelo não é muito fiavel

#no entanto, na fase da normalização pode aplicar-se a transformação de box cox
#de modo a diminuir a skewness dos residuos, e a tornar os dados mais normais

p1 <- preProcess(predictors, method =c("BoxCox"))


#o r ^2: do novo modelo é Adjusted R-squared:  0.9469 
summary(model)
#substrate_mM , vmax_app ,km_app influenciam mais o modelo

#Apesar de os pressupostos da normalidade serem violados, a amostra é muito grande, logo podemos 
#aceitar a violação da normalidade

model_regression_3 <- step(model, direction = "both")
summary(model_regression_3)

#################

# Fit weighted nonlinear model using Michaelis-Menten equation

model_weighted = nls(velocity_uM_min ~ (Vmax * substrate_mM) / (Km + substrate_mM),
                     data = NI,
                     start = list(Vmax = 100, Km = 2))
model_weighted


########################### Lineweaver-Burk Plots ##############################

#1- Lineweaver-Burk Plot for Competitive Inhibition

CI_LBP <- ggplot(CI, mapping = aes(x = 1/substrate_mM, y = 1/velocity_uM_min)) +
  geom_point(size=1, color = "gray57") + geom_smooth(method = "lm", se = F, color = "black") +
  labs(title= "Competitive Inhibition", x = "1 / Substrate (mM)", y = " 1 / Velocity (uM/Min)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17, face = "plain",family = "sans" , color = "mediumorchid4", hjust = 0.5))
CI_LBP

#2- Lineweaver-Burk Plot for Uncompetitive Inhibition

UCI_LBP <- ggplot(UCI, mapping = aes(x = 1/substrate_mM, y = 1/velocity_uM_min)) +
  geom_point(size=1, color = "gray57") + geom_smooth(method = "lm", se = F, color = "black") +
  labs(title= "Uncompetitive Inhibition", x = "1 / Substrate (mM)", y = " 1 / Velocity (uM/Min)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17, face = "plain",family = "sans" , color = "mediumorchid4", hjust = 0.5))
UCI_LBP

#3- Lineweaver-Burk Plot for Noncompetitive Inhibition

NCI_LBP <- ggplot(NCI, mapping = aes(x = 1/substrate_mM, y = 1/velocity_uM_min)) +
  geom_point(size=1, color = "gray57") + geom_smooth(method = "lm", se = F, color = "black") +
  labs(title= "Noncompetitive Inhibition", x = "1 / Substrate (mM)", y = " 1 / Velocity (uM/Min)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17, face = "plain",family = "sans" , color = "mediumorchid4", hjust = 0.5))
NCI_LBP

#4- Lineweaver-Burk Plot for No Inhibition (None)

NI_LBP <- ggplot(NI, mapping = aes(x = 1/substrate_mM, y = 1/velocity_uM_min)) +
  geom_point(size=1, color = "gray57") + geom_smooth(method = "lm", se = F, color = "black") +
  labs(title= "No Inhibition", x = "1 / Substrate (mM)", y = " 1 / Velocity (uM/Min)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17, face = "plain",family = "sans" , color = "mediumorchid4", hjust = 0.5))
NI_LBP


Final_LBPs <- ((CI_LBP|UCI_LBP)/(NCI_LBP|NI_LBP)); Final_LBPs



