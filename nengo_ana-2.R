library(data.table)

thalamus = read.table("th-output-2.txt")

names(thalamus) = c("Time","Choice","Value")

choice_name = c("V1MEM1","MEM2AC1","V1WM1","WM1MEM1","MEM2WM1","WM1AC1","Wander")

thalamus$t = (thalamus$Time - 1) / 100
thalamus$Trial = floor(thalamus$t / 3.4)
thalamus$Condition = ifelse(thalamus$Trial < 40, "Training", "Test")
thalamus$TimeInTrial = thalamus$Time - thalamus$Trial * 340 
thalamus$Task = ifelse(thalamus$TimeInTrial < 171, "WM", "CRT")



thal_sub <- aggregate(Value ~ Choice * TimeInTrial, data = thalamus[thalamus$Condition == "Test",], FUN=mean)
# thal_sub <- aggregate(Value ~ Choice * TimeInTrial, data = thalamus, FUN=mean)
thal_sub$t <- thal_sub$TimeInTrial / 100

plot(thal_sub[thal_sub$Choice == 7,]$t,thal_sub[thal_sub$Choice == 7,]$Value, lwd=2, type = "l", col="red", xlab = "Time", ylab = "Activation", ylim = c(min(thal_sub$Value),max(thal_sub$Value)+0.4), main="Thalamus output")
lines(thal_sub[thal_sub$Choice == 1,]$t,thal_sub[thal_sub$Choice == 1,]$Value,lwd=2, type = "l", col = "blue")
lines(thal_sub[thal_sub$Choice == 2,]$t,thal_sub[thal_sub$Choice == 2,]$Value,lwd=2, type = "l", col = "orange")
lines(thal_sub[thal_sub$Choice == 3,]$t,thal_sub[thal_sub$Choice == 3,]$Value,lwd=2, type = "l", col = "black")
lines(thal_sub[thal_sub$Choice == 4,]$t,thal_sub[thal_sub$Choice == 4,]$Value,lwd=2, type = "l", col = "pink")
lines(thal_sub[thal_sub$Choice == 5,]$t,thal_sub[thal_sub$Choice == 5,]$Value,lwd=2, type = "l", col = "yellow")
lines(thal_sub[thal_sub$Choice == 6,]$t,thal_sub[thal_sub$Choice == 6,]$Value,lwd=2, type = "l", col = "purple")
legend("topright",legend = choice_name, col = c("blue","orange","black","pink","yellow","purple","red"),lwd = 2,  bty="n", cex=0.7)
abline(v=1.7, lty=2, lwd=2)
abline(v=0.3, lty=3)
abline(v=0.5, lty=3)
abline(v=0.8, lty=3)
abline(v=1.0, lty=3)
abline(v=1.6, lty=3)
abline(v=2.0, lty=3)
abline(v=2.2, lty=3)
abline(v=2.5, lty=3)
abline(v=2.7, lty=3)
abline(v=3.3, lty=3)
text(c(0.7,2.3),c(0.2,0.2),labels=c("WM","CRT"))
text(c(0.1, 0.6, 1.2, 1.8, 2.3, 2.8),1.1,labels = c("4","1","?","3","2","5"),col=c("black","black","red","black","black","red"), cex=1.2)

# What is the Choice belonging to the highest Value at a given t?

thal_h <- as.data.table(thalamus[thalamus$Condition == "Test",])
thal_h$TrialwithTime <- thal_h$Trial * 1000 + thal_h$TimeInTrial
result <- thal_h[thal_h[, .I[Value == max(Value)], by=TrialwithTime]$V1]

res <- aggregate(Trial ~ Choice * Task , data = result, FUN=length)


bg = read.table("bg-util-2.txt")

names(bg) = c("Time","Choice","Value")

bg$t = (bg$Time - 1) / 100
bg$Trial = floor(bg$t / 3.4)
bg$Task = ifelse(bg$Trial %%2 == 0, "WM", "CRT")
bg$Condition = ifelse(bg$Trial < 40, "Training", "Test")
bg$TimeInTrial = bg$Time - bg$Trial * 340 

bg_sub <- aggregate(Value ~ Choice * TimeInTrial, data = bg[bg$Condition == "Test",], FUN=mean)
#bg_sub <- aggregate(Value ~ Choice * TimeInTrial, data = bg, FUN=mean)
bg_sub$t <- bg_sub$TimeInTrial / 100

plot(bg_sub[bg_sub$Choice == 7,]$t,bg_sub[bg_sub$Choice == 7,]$Value, lwd=2, type = "l", col="red", xlab = "Time", ylab = "Activation", ylim = c(min(bg_sub$Value),max(bg_sub$Value)+0.4),main="Basal Ganglia input")
lines(bg_sub[bg_sub$Choice == 1,]$t,bg_sub[bg_sub$Choice == 1,]$Value,lwd=2, type = "l", col = "blue")
lines(bg_sub[bg_sub$Choice == 2,]$t,bg_sub[bg_sub$Choice == 2,]$Value,lwd=2, type = "l", col = "orange")
lines(bg_sub[bg_sub$Choice == 3,]$t,bg_sub[bg_sub$Choice == 3,]$Value,lwd=2, type = "l", col = "black")
lines(bg_sub[bg_sub$Choice == 4,]$t,bg_sub[bg_sub$Choice == 4,]$Value,lwd=2, type = "l", col = "pink")
lines(bg_sub[bg_sub$Choice == 5,]$t,bg_sub[bg_sub$Choice == 5,]$Value,lwd=2, type = "l", col = "yellow")
lines(bg_sub[bg_sub$Choice == 6,]$t,bg_sub[bg_sub$Choice == 6,]$Value,lwd=2, type = "l", col = "purple")
legend("topright",legend = choice_name, col = c("blue","orange","black","pink","yellow","purple","red"),lwd = 2,  bty="n", cex=0.7)
abline(v=1.7, lty=2, lwd=2)
text(c(0.7,2.3),c(0.4,0.4),labels=c("WM","CRT"))
abline(v=0.3, lty=3)
abline(v=0.5, lty=3)
abline(v=0.8, lty=3)
abline(v=1.0, lty=3)
abline(v=1.6, lty=3)
abline(v=2.0, lty=3)
abline(v=2.2, lty=3)
abline(v=2.5, lty=3)
abline(v=2.7, lty=3)
abline(v=3.3, lty=3)
text(c(0.1, 0.6, 1.2, 1.8, 2.3, 2.8),0.8,labels = c("4","1","?","3","2","5"),col=c("black","black","red","black","black","red"), cex=1.2)


mem <- read.table("mem-output-2.txt")
names(mem) <- c("Time","SP","Value")
mem$t = (mem$Time - 1) / 100
mem$Trial = floor(mem$t / 3.4)
mem$Condition = ifelse(mem$Trial < 40, "Training", "Test")
mem$TimeInTrial = mem$Time - mem$Trial * 340 
mem$Task = ifelse(mem$TimeInTrial < 171, "WM", "CRT")

mem_sub <- mem[mem$Trial == 43,]
mem_sub$t <- mem_sub$TimeInTrial / 100

plot(mem_sub[mem_sub$SP == 'ODD',]$t,mem_sub[mem_sub$SP == 'ODD',]$Value, lwd=2, type = "l", col="red", xlab = "Time", ylab = "Activation", ylim = c(min(mem_sub$Value),max(mem_sub$Value)+0.4),main="Memory output")
lines(mem_sub[mem_sub$SP == 'EVEN',]$t,mem_sub[mem_sub$SP == 'EVEN',]$Value, lwd=2, type = "l", col="orange")
lines(mem_sub[mem_sub$SP == 'EVEN',]$t,mem_sub[mem_sub$SP == 'CRY',]$Value, lwd=2, type = "l", col="green")
lines(mem_sub[mem_sub$SP == 'EVEN',]$t,mem_sub[mem_sub$SP == 'REDEEM',]$Value, lwd=2, type = "l", col="purple")
lines(mem_sub[mem_sub$SP == 'EVEN',]$t,mem_sub[mem_sub$SP == 'LAUGH',]$Value, lwd=2, type = "l", col="blue")
abline(v=1.7, lty=2, lwd=2)
text(c(0.5,2.3),-0.4,labels=c("WM","CRT"))
abline(v=0.3, lty=3)
abline(v=0.5, lty=3)
abline(v=0.8, lty=3)
abline(v=1.0, lty=3)
abline(v=1.6, lty=3)
abline(v=2.0, lty=3)
abline(v=2.2, lty=3)
abline(v=2.5, lty=3)
abline(v=2.7, lty=3)
abline(v=3.3, lty=3)
text(c(0.1, 0.6, 1.2, 1.8, 2.3, 2.8),-0.25,labels = c("4","1","?","3","2","5"),col=c("black","black","red","black","black","red"), cex=1.2)
legend("topright",legend = c("ODD","EVEN","CRY","REDEEM","LAUGH"), col = c("red","orange","green","purple","blue"),lwd = 2,  bty="n", cex=0.7)

mem_h <- as.data.table(mem[mem$Condition == "Test",])
mem_h$TrialwithTime <- mem_h$Trial * 1000 + mem_h$TimeInTrial



result <- mem_h[mem_h[, .I[Value == max(Value)], by=TrialwithTime]$V1]

res <- aggregate(Trial ~ SP * Task , data = result, FUN=length)



