#obtem e prepara banco para uso
-d'ata.frame(Enem_)
attach(Enem_)



#                 Questão 1
#   1.1 tabelas simples
#1.1.1 Estado civil
EC=table(Enem_$`Estado Civil`)
EC

#1.1.2 Dependencia administrativa(Escola)
Da=table(Enem_$`Dependência administrativa (Escola)`)
Da

#1.1.3 Renda mensal da familia 
Rm=table(Enem_$`Qual é a renda mensal de sua família?`)
Rm

#1.1.4 Sua residencia tem ascesso a internet
AI=table(Enem_$`Na sua residência tem acesso à Internet?`)
AI

#1.1.5 Lingua estrangeira
LI= table(Enem_$`Língua Estrangeira`)
LI

#1.1.6 Escola do ensino medio 
EM= table(Enem_$`Tipo de escola do Ensino Médio`)
EM


#   1.2Tabelas relativas (da questão 1)
#1.2.1 Estado civil relativo
ECR= prop.table(table(Enem_$`Estado Civil`))
round(ECR, digits = 4)

#1.2.3 Dependencia administrativa(Escola)
DAR= prop.table(table(Enem_$`Dependência administrativa (Escola)`))
round(DAR, digits = 4 )

#1.2.4 Renda familiar 
RMF= prop.table(table(Enem_$`Qual é a renda mensal de sua família?`))
round(RMF, digits = 4)

#1.2.5 Sua residencia tem acesso a internet 
AIR= prop.table(table(Enem_$`Na sua residência tem acesso à Internet?`))
round(AIR, digits = 4)

#1.2.6 lingua estrangeira
LIR= prop.table(table(Enem_$`Língua Estrangeira`))
round(LIR, digits = 4)

#1.2.7 escola do ensino medio 
EMR= prop.table(table(Enem_$`Tipo de escola do Ensino Médio`))
round(EMR, digits = 4)




#                   Questão 2 
#   Montar tabela cruzada com as frequências simples e relativas para as variáveis:
#2.2 Qual é a renda mensal de sua família versus na sua residência tem acesso à Internet.
X=table(Enem_$`Qual é a renda mensal de sua família?`, Enem_$ `Na sua residência tem acesso à Internet?`)
X

#2.3 Na sua residência tem TV por assinatura versus na sua residência tem acesso à Internet.
Y=table(Enem_$`Na sua residência tem TV por assinatura?`,Enem_$`Na sua residência tem acesso à Internet?`)
Y
#2.4 Tipo de escola do Ensino Médio versus Língua Estrangeira
Z=table(Enem_$`Tipo de escola do Ensino Médio`,Enem_$`Língua Estrangeira`)
Z



#                   Questão 3
#   Montar um gráfico de barra para a variável:
#3.1Qual é a renda mensal de sua família
barplot(table(Enem_$`Qual é a renda mensal de sua família?`),xlab = "Tipo de Renda", ylab = "Valor da Renda", ylim =c(0,10000),col =c("green","yellow","red"),main = "renda mensal de sua família")

#3.2 Na sua residência tem acesso à Internet?
barplot(table(Enem_$`Na sua residência tem acesso à Internet?`),xlab="Tem acesso a internet",ylab = "Quantidade", ylim = c(0,15000),col = c("red","green"), main= "Acesso a internet" )



#                   Questão 4
#   Montar um gráfico de setor para as variáveis:
#4.1 Na sua residência tem acesso à Internet.
x= table(Enem_$`Na sua residência tem acesso à Internet?`)
piepercent<- round(100*x/sum(x), 1)
piepercent

pie(table(Enem_$`Na sua residência tem acesso à Internet?`),labels = piepercent,col = c("red", "green"),main = "grafico de setor acesso a internet")

#4.2 Na sua residência tem telefone celular.
y= table(Enem_$`Na sua residência tem telefone celular?`)
piepercent<- round(100*y/sum(y),1)
piepercent

pie(table(Enem_$`Na sua residência tem telefone celular?`), labels = piepercent,col = c("red", "green","blue","orange","yellow"), main = "grafico de setores telefone celular" )





#                   Questão 5
#     Determinar as medidas de posição e dispersão para as variáveis:
# 5.1 Nota da prova de Ciências da Natureza
#MEDIA
media1 = mean(Enem_$`Nota da prova de Ciências da Natureza`)
round(media1, digits=3) #mostra a media arredondada pra 3 casas decimais
#MEDIANA
median(Enem_$`Nota da prova de Ciências da Natureza`)
#MODA
mo1 = table(Enem_$`Nota da prova de Ciências da Natureza`)
m1 =(moda=names(mo1)[mo1==max(mo1)])
m1 #printa
#AMPLITUDE
range(Enem_$`Nota da prova de Ciências da Natureza`)
#VARIANCIA
vari1 = var(Enem_$`Nota da prova de Ciências da Natureza`)
round(vari1, digits=3) #printa a variancia arredondada pra 3 digitos
#DESVIO PADRAO
dp1 = sd(Enem_$`Nota da prova de Ciências da Natureza`)
round(dp1, digits = 3) #printa o desvio padrao arredondada pra 3 digitos
#COEFICIENTE DE VARIACAO
cv1=dp1/media1
round(cv1,digits=3)

# 5.2 Nota da prova de Ciências Humanas 
media2 = mean(Enem_$`Nota da prova de Ciências Humanas`)
round(media2, digits=3) #mostra a media arredondada pra 3 casas decimais
#MEDIANA
median(Enem_$`Nota da prova de Ciências Humanas`)
#MODA
mo2 = table(Enem_$`Nota da prova de Ciências Humanas`)
m2 =(moda=names(mo2)[mo2==max(mo2)])
m2 #printa
#AMPLITUDE
range(Enem_$`Nota da prova de Ciências Humanas`)
#VARIANCIA
vari2 = var(Enem_$`Nota da prova de Ciências Humanas`)
round(vari2, digits=3) #printa a variancia arredondada pra 3 digitos
#DESVIO PADRAO
dp2 = sd(Enem_$`Nota da prova de Ciências Humanas`)
round(dp2, digits = 3) #printa o desvio padrao arredondada pra 3 digitos
#COEFICIENTE DE VARIACAO
cv2=dp2/media2
round(cv2,digits=3)

# 5.3 Nota da prova de Linguagens e Códigos 
media3 = mean(Enem_$`Nota da prova de Linguagens e Códigos`)
round(media3, digits=3) #mostra a media arredondada pra 3 casas decimais
#MEDIANA
median(Enem_$`Nota da prova de Linguagens e Códigos`)
#MODA
mo3 = table(Enem_$`Nota da prova de Linguagens e Códigos`)
m3 =(moda=names(mo3)[mo3==max(mo3)])
m3 #printa
#AMPLITUDE
range(Enem_$`Nota da prova de Linguagens e Códigos`)
#VARIANCIA
vari3 = var(Enem_$`Nota da prova de Linguagens e Códigos`)
round(vari3, digits=3) #printa a variancia arredondada pra 3 digitos
#DESVIO PADRAO
dp3 = sd(Enem_$`Nota da prova de Linguagens e Códigos`)
round(dp3, digits = 3) #printa o desvio padrao arredondada pra 3 digitos
#COEFICIENTE DE VARIACAO
cv3=dp3/media3
round(cv3,digits=3)

# 5.4 Nota da prova de Matemática
media4 = mean(Enem_$`Nota da prova de Matemática`)
round(media4, digits=3) #mostra a media arredondada pra 3 casas decimais
#MEDIANA
median(Enem_$`Nota da prova de Matemática`)
#MODA
mo4 = table(Enem_$`Nota da prova de Matemática`)
m4 =(moda=names(mo4)[mo4==max(mo4)])
m4 #printa
#AMPLITUDE
range(Enem_$`Nota da prova de Matemática`)
#VARIANCIA
vari4 = var(Enem_$`Nota da prova de Matemática`)
round(vari4, digits=3) #printa a variancia arredondada pra 3 digitos
#DESVIO PADRAO
dp4 = sd(Enem_$`Nota da prova de Matemática`)
round(dp4, digits = 3) #printa o desvio padrao arredondada pra 3 digitos
#COEFICIENTE DE VARIACAO
cv4=dp4/media4
round(cv4,digits=3)





#                 QUESTÃO 6
#     Determinar histograma para as variáveis: 
#6.1 Nota da prova de Matemática
hist(Enem_$`Nota da prova de Matemática`,
  nclass=5,
  xlab="nome de X aqui", ylab="nome de Y aqui",
  main="titulo aqui")

#6.2 Nota da prova de redação
hist(Enem_$`Nota da prova de redação`,
  nclass=5,
  xlab="nome de X aqui", ylab="nome de Y aqui",
  main="titulo aqui")




#                 QUESTÃO 7
#     Determinar Box Plot para as variáveis:
#7.1 Nota da prova de Matemática
boxplot(table(Enem_$`Nota da prova de Matemática`), 
        xlab="nome de X aqui", ylab="nome de Y aqui", 
        ylim=c(0,80), 
        col=c("green","yellow","red"),
        main="titulo aqui")


#7.2 Nota da prova de redação
boxplot(table(Enem_$`Nota da prova de redação`), 
        xlab="nome de X aqui", ylab="nome de Y aqui", 
        ylim=c(0,80), 
        col=c("green","yellow","red"),
        main="titulo aqui")

