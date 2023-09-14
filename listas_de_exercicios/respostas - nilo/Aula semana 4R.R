#1
data.frame(Base_dados)
attach(Base_dados)
#2
Y=table(Base_dados$`5. Tipo de Residência`)
Y
#3
P=prop.table(table(Base_dados$`5. Tipo de Residência`))
P
#4
round(P, digits = 2)
P
#tipo de residencia versus genero
w=table(Base_dados$`5. Tipo de Residência`,Base_dados$`6. Sexo`)
w
prop.table(w)

z=round(prop.table(w),2)
z
#Extra
Total_linha<-margin.table(z,2)#O argumento 2 define a marginal da linha
Total_coluna<-margin.table(z,1)#O argumento 1 define a marginal da coluna
tab<-rbind(cbind(z,Total_coluna),c(Total_linha, sum(Total_coluna)))
dimnames(tab)[[1]][4]<-"Total_linha"
tab
#Tabela
barplot(table(Base_dados$`5. Tipo de Residência`),xlab = "Tipo Residencia", ylab = "Quantidade", ylim =c(0,80),col =c("green","yellow","red"),main = "Tipo de redidencia")
