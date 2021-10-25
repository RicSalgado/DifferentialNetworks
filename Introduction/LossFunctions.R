# Loss functions

L1_loss = function(x){abs(x)}
L2_loss = function(x){x*x}
Huber_loss = function(x){(0.24^2)*(sqrt(1 + (x/0.24)^2) - 1)}

x_init <- (-100:100)/100
x_full <- rep(-100:100,3)/100

# Plot

dummy_data <- cbind(x_full, rbind(cbind(L1_loss(x_init)), cbind(L2_loss(x_init)), cbind(Huber_loss(x_init))))

dummy_data <- cbind(dummy_data, rbind(cbind(rep(1:1,201)), cbind(rep(2:2,201)), cbind(rep(3:3,201))))

dummy_data <- as.data.frame(dummy_data)

colnames(dummy_data)[3] <- "Loss Function"

dummy_data$`Loss Function` <- as.factor(dummy_data$`Loss Function`)

library(ggplot2)

pdf("LossFunctions.pdf",15,10)
ggplot(data=dummy_data, aes(x=x_full, y=V2, group = `Loss Function`, colour = `Loss Function`)) + 
  geom_line(size = 1) + xlab("y") + ylab("Loss Function") + 
  scale_color_manual(labels = c("Mean Absolute Error", "Mean Square Error", "Huber"), values = c("royalblue1", "tomato", "springgreen4")) +
  theme_bw() + theme(
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25), legend.position=c(0.5, 0.8),
    axis.title.y=element_text(size=25),
    axis.title.x=element_text(size=25))
dev.off()


