# Loss functions

L1_loss = function(x){abs(x)}
L2_loss = function(x){x*x}
Huber_loss = function(x){(0.24^2)*(sqrt(1 + (x/0.24)^2) - 1)}

x_init <- (-5:5)/5
x_full <- rep(-5:5,3)/5

# Plot

dummy_data <- cbind(x_full, rbind(cbind(L1_loss(x_init)), cbind(L2_loss(x_init)), cbind(Huber_loss(x_init))))

dummy_data <- cbind(dummy_data, rbind(cbind(rep(1:1,11)), cbind(rep(2:2,11)), cbind(rep(3:3,11))))

dummy_data <- as.data.frame(dummy_data)

colnames(dummy_data)[3] <- "Loss Function"

dummy_data$`Loss Function` <- as.factor(dummy_data$`Loss Function`)

library(ggplot2)

pdf("LossFunctions.pdf",10,10)
ggplot(data=dummy_data, aes(x=x_full, y=V2, group = `Loss Function`, colour = `Loss Function`)) + 
  geom_line(size = 1) + xlab("") + ylab(" ") + 
  scale_color_manual(labels = c(expression(L[1]), expression(L[2]), "Huber"), values = c("royalblue1", "tomato", "springgreen4")) +
  theme_bw() + theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )
dev.off()


