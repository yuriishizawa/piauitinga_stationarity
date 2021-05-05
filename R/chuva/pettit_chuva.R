#Est?ncia
library(trend)

pettitt_analysis <- function(p,g_name,bp_name){
  bmp(filename = g_name, width = 400, height = 400)
  plot(p[,'ano'],p[,'prec'], type = 'p', xlab = 'Ano', ylab = 'Precipitação acumulada anual (mm)')
  points(p[,'ano'],p[,'prec'], col = 'black', pch = 16)
  p_res <- pettitt.test(p[,'prec'])
  n <- p_res$nobs
  i <- p_res$estimate
  p_value <- p_res$p.value
  data <- p_res&data
  s.1 <- mean(p[1:i,"prec"])
  s.2 <- mean(p[(i+1):n,"prec"])
  s.plot1 <- ts(c(rep(s.1,i)))
  s.plot2 <- ts(c(rep(s.2,(n-i))))
  lines(p[1:i,'ano'],s.plot1, lty=2, col = c('red'))
  lines(p[(i+1):n,'ano'],s.plot2, lty=2, col = 'blue')
  legend('topleft', legend=c(sprintf("M?dia (%s-%i)",p[1,1],p[i,1]),sprintf("M?dia (%s-%i)",p[i+1,1],p[n,1])),
         col=c("red", "blue"), lty=2, cex=0.8)
  dev.off()
  #boxplot
  bmp(filename = bp_name, width = 400, height = 400)
  b.plot1 <- ts(c(rep(sprintf("(%s-%i)",p[1,1],p[i,1]),i)))
  b.plot2 <- ts(c(rep(sprintf("(%s-%i)",p[i+1,1],p[n,1]),(n-i))))
  b = c(b.plot1,b.plot2)
  boxplot(p[,'prec']~b, col = 'grey', xlab = 'Período', ylab = 'Precipitação acumulada anual (mm)')
  dev.off()
  return(n,i)
}

pettitt_analysis(p,"estancia_test1.bmp", "box_test1.bmp")

p_res$p.value


p <- read.table('estancia_chuva.txt', header = T)
bmp(filename = "g_name.bmp", width = 400, height = 400)
plot(p[,'ano'],p[,'prec'], type = 'p', xlab = 'Ano', ylab = 'Precipitação acumulada anual (mm)')
points(p[,'ano'],p[,'prec'], col = 'black', pch = 16)
p_res <- pettitt.test(p[,'prec'])
n <- p.res$nobs
i <- p.res$estimate
s.1 <- mean(p[1:i,"prec"])
s.2 <- mean(p[(i+1):n,"prec"])
s.plot1 <- ts(c(rep(s.1,i)))
s.plot2 <- ts(c(rep(s.2,(n-i))))
lines(p[1:i,'ano'],s.plot1, lty=2, col = c('red'))
lines(p[(i+1):n,'ano'],s.plot2, lty=2, col = 'blue')
legend('topleft', legend=c(sprintf("Média (%s-%i)",p[1,1],p[i,1]),sprintf("Média (%s-%i)",p[i+1,1],p[n,1])),
       col=c("red", "blue"), lty=2, cex=0.8)
dev.off()

bmp(filename = "estancia.bmp", width = 400, height = 400)
p <- read.table('estancia_chuva.txt', header = T)
plot(p[,'ano'],p[,'prec'], type = 'p', xlab = 'Ano', ylab = 'Precipita??o acumulada anual (mm)')
points(p[,'ano'],p[,'prec'], col = 'black', pch = 16)
p.res <- pettitt.test(p[,'prec'])
n <- p.res$nobs
i <- p.res$estimate
s.1 <- mean(p[1:i,"prec"])
s.2 <- mean(p[(i+1):n,"prec"])
s.plot1 <- ts(c(rep(s.1,i)))
s.plot2 <- ts(c(rep(s.2,(n-i))))
lines(p[1:i,'ano'],s.plot1, lty=2, col = c('red'))
lines(p[(i+1):n,'ano'],s.plot2, lty=2, col = 'blue')
legend('topleft', legend=c(sprintf("M?dia (%s-%i)",p[1,1],p[i,1]),sprintf("M?dia (%s-%i)",p[i+1,1],p[n,1])),
       col=c("red", "blue"), lty=2, cex=0.8)
dev.off()
#boxplot
bmp(filename = "boxplot-estancia.bmp", width = 400, height = 400)
b.plot1 <- ts(c(rep(sprintf("(%s-%i)",p[1,1],p[i,1]),i)))
b.plot2 <- ts(c(rep(sprintf("(%s-%i)",p[i+1,1],p[n,1]),(n-i))))
b = c(b.plot1,b.plot2)
boxplot(p[,'prec']~b, col = 'grey', xlab = 'Per?odo', ylab = 'Precipita??o acumulada anual (mm)')
dev.off()

#----------------------------------------------------------
#Salgado
bmp(filename = "salgado.bmp", width = 400, height = 400)
p <- read.table('salgado_chuva.txt', header = T)
plot(p[,'ano'],p[,'prec'], type = 'p', xlab = 'Ano', ylab = 'Precipita??o acumulada anual (mm)')
points(p[,'ano'],p[,'prec'], col = 'black', pch = 16)
p.res <- pettitt.test(p[,'prec'])
n <- p.res$nobs
i <- p.res$estimate
s.1 <- mean(p[1:i,"prec"])
s.2 <- mean(p[(i+1):n,"prec"])
s.plot1 <- ts(c(rep(s.1,i)))
s.plot2 <- ts(c(rep(s.2,(n-i))))
lines(p[1:i,'ano'],s.plot1, lty=2, col = c('red'))
lines(p[(i+1):n,'ano'],s.plot2, lty=2, col = 'blue')
legend('topleft', legend=c(sprintf("M?dia (%s-%i)",p[1,1],p[i,1]),sprintf("M?dia (%s-%i)",p[i+1,1],p[n,1])),
       col=c("red", "blue"), lty=2, cex=0.8)
dev.off()
#boxplot
bmp(filename = "boxplot-salgado.bmp", width = 400, height = 400)
b.plot1 <- ts(c(rep(sprintf("(%s-%i)",p[1,1],p[i,1]),i)))
b.plot2 <- ts(c(rep(sprintf("(%s-%i)",p[i+1,1],p[n,1]),(n-i))))
b = c(b.plot1,b.plot2)
boxplot(p[,'prec']~b, col = 'grey', xlab = 'Per?odo', ylab = 'Precipita??o acumulada anual (mm)')
dev.off()

#----------------------------------------------------------
#Thiessen
library(trend)
tiff("thiessen.tiff",units = "in", width=5, height=5, res=300)
#bmp(filename = "thiessen.bmp", width = 400, height = 400)
est <- read.table('estancia_chuva.txt', header = T)
sal <- read.table('salgado_chuva.txt', header = T)
p[,'prec']= ((est[,'prec']*0.2284 + sal[,'prec']*(1-0.2284)))
plot(p[,'ano'],p[,'prec'], type = 'p', xlab = 'Ano', ylab = 'Precipitação acumulada anual (mm)',
     cex.axis = 1.25, cex.lab = 1.25)
points(p[,'ano'],p[,'prec'], col = 'black', pch = 16)
p.res <- pettitt.test(p[,'prec'])
n <- p.res$nobs
i <- p.res$estimate
s.1 <- mean(p[1:i,"prec"])
s.2 <- mean(p[(i+1):n,"prec"])
s.plot1 <- ts(c(rep(s.1,i)))
s.plot2 <- ts(c(rep(s.2,(n-i))))
lines(p[1:i,'ano'],s.plot1, lty=2, col = c('red'))
lines(p[(i+1):n,'ano'],s.plot2, lty=2, col = 'blue')
legend('topleft', legend=c(sprintf("Média (%s-%i)",p[1,1],p[i,1]),sprintf("Média (%s-%i)",p[i+1,1],p[n,1])),
       col=c("red", "blue"), lty=2, cex=1.25)
dev.off()
#valores de U
bmp(filename = "U-thiessen.bmp", width = 400, height = 400)
plot(p[,'ano'],p.res$data, type = 'p', xlab = 'Ano', ylab = 'U')
points(p[,'ano'],p.res$data, col = 'black', pch = 16)
lines(p[,'ano'],rep(0,n), lty=2, col = c('black'))
lines(p[,'ano'],p.res$data, col = 'black')
dev.off()
#boxplot
tiff("boxplot-thiessen.tiff",units = "in", width=5, height=5, res=300)
#bmp(filename = "boxplot-thiessen.bmp", width = 400, height = 400)
b.plot1 <- ts(c(rep(sprintf("(%s-%i)",p[1,1],p[i,1]),i)))
b.plot2 <- ts(c(rep(sprintf("(%s-%i)",p[i+1,1],p[n,1]),(n-i))))
b = c(b.plot1,b.plot2)
boxplot(p[,'prec']~b, col = 'grey', xlab = 'Período', ylab = 'Precipitação acumulada anual (mm)',cex.axis = 1.25, cex.lab = 1.25)
dev.off()