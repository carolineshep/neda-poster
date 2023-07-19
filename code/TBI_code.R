library (vegan)
library (ggplot2)


######DECOMPOSITION TEA BAG DATA
##LOAD DATA
data<- TBI_Jun_2023_Final

###ANOVA-decomp by treatments
TBI.model <- lm (data$"k" ~ data$"Treatment", data = data)
model.aov<- aov (TBI.model)
summary (model.aov)

###boxplot of decomp by treatments
p <- ggplot(data, aes(y=k, x=Treatment, fill = Treatment))+ geom_boxplot()
p <- p+ theme_bw() +  scale_fill_manual(values=c("#C2D6D4", "#F2E80A"))+ theme(text = element_text(colour = 'black', size = 30)) + xlab ("Treatment") + ylab ("Decomposition Rate (k)") +  theme(legend.position="none")
p

###ANOVA-decomp by treatments and round
TBI.model.Round <- lm (data$"k" ~ data$"Treatment"* data$"Round", data = data)
model.aov.round<- aov (TBI.model.Round)
summary (model.aov.round)

###boxplot of decomp by treatments and round
ggplot(data, aes(x=factor(Treatment), y=k, fill = factor(Round)))+ xlab ("Treatment") + ylab ("Decomposition Rate (k)") +
  geom_boxplot()+theme_bw() + theme(text = element_text(colour = 'black', size = 30)) +  scale_fill_manual(values=c("#C2D6D4", "#F2E80A", "#A7A09A", "#3B3B3D")) + labs(fill = "Round")
