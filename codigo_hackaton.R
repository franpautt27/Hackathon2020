setwd("C:/Users/Usuario/Desktop/analisis")
datos<-read.csv("datos.csv",sep=";")
str(datos)
library(lubridate)
library(tidyverse)
datos$FECHA.DE.INGRESO.AL.PAIS<-dmy(datos$FECHA.DE.INGRESO.AL.PAIS)
datos$FECHA.DE.REGISTRO<-dmy(datos$FECHA.DE.REGISTRO)
str(datos)
dat<-select(datos,OCUPACION.U.OFICIO,DEPARTAMENTO,MUNICIPIO,GENERO,EDAD,ESTADO.CIVIL,NIVEL.DE.ESCOLARIDAD,CONVALIDADO.,EXPERIENCIA.CERTIFICADA.,FECHA.DE.INGRESO.AL.PAIS,FECHA.DE.REGISTRO,SECTOR.DE.OCUPACION,AFILIADO.A.SEGUDAD.SOCIAL.,INTENCIÓN.DE.PERMANENCIA)#Seleccionamos las variables mas significativas
library(forcats)
dat<-filter(dat,EDAD>=18,EDAD<=60)
str(dat)
summary(dat)

#SEPARANDO EMPLEO FORMAL CON INFORMAL Y DESEMPLEADO
Ocupación<-fct_collapse(dat$OCUPACION.U.OFICIO,
                        "Desempleado"=c("Desempleado","No Aplica"),
                        "Empleado formal"=c("Empleado Formal (Contrato Laboral)"),
                        "Empleado Informal"=c("Empleado Informal (Sin Contrato Laboral)"))
fct_count(Ocupación)

datos2<-data.frame(Ocupación,dat)#agregamos el subconjunto creado a la base de datos
View(datos2)
datos2<-datos2[,-2]##no correr 2 veces (eliminamos la columna antigua de ocupacion)
datos2<-filter(datos2,Ocupación!="Estudiante")
summary(datos2)


NIVEL_ACADEMICO<-fct_collapse(datos2$NIVEL.DE.ESCOLARIDAD,
                            "Basico"=c("Preescolar","Primaria", "Secundaria"),
                            "Profesional" =c("Universitario","Doctorado - PSD","Especializacion","Maestria"),
                            "Tecnica"=c("Tecnica/tecnologica"),
                            "Ninguna"=c("Ninguno","No Aplica"))
datos2<-data.frame(datos2,NIVEL_ACADEMICO)
view(datos2)
datos2<- datos2[,-7]##no correr

datos3<-filter(datos2,CONVALIDADO.=="SI"|CONVALIDADO.=="NO")
datos3<-filter(datos3,EXPERIENCIA.CERTIFICADA.=="NO"|EXPERIENCIA.CERTIFICADA.=="SI")
view(datos3)
summary(datos3)

which.max(datos3$FECHA.DE.REGISTRO)
datos$FECHA.DE.REGISTRO[44985]

k<-dmy("08/06/2018")#FECHA MAXIMA DE REGISTRO
z<-dmy("01/01/2017")#FECHA EN QUE INICIO LA CRISIS MIGRATORIA
h<-k-z#restamos las 2 fechas para hallar el limite de dias a analizar
h<-as.numeric(h)#pasamos a numerico


DIAS_EN_COLOMBIA<-datos3$FECHA.DE.REGISTRO-datos3$FECHA.DE.INGRESO.AL.PAIS
view(DIAS_EN_COLOMBIA)
datos3<-data.frame(datos3,DIAS_EN_COLOMBIA)
str(datos3)
datos3$DIAS_EN_COLOMBIA<-as.numeric(datos3$DIAS_EN_COLOMBIA)
datos3<-datos3[,-8]###NO CORRER
datos3<-filter(datos3,DIAS_EN_COLOMBIA<=h,DIAS_EN_COLOMBIA>=0) ## NUmero de dias que transcurrieron entre el inicio de la crisis migratoria hasta la realización de la encuesta
which.max(datos3$DIAS_EN_COLOMBIA)
which.min(datos3$DIAS_EN_COLOMBIA)
datos3[8608,]
View(datos3)

str(datos3)
datos3$Fecha<-datos3$FECHA.DE.INGRESO.AL.PAIS
datos3<-datos3[,-16]

#Grafico de ocupacion 2017-2018
library(plotly)
library(scales)
p1=ggplot(data = datos3, mapping = aes(x = Fecha, fill = Ocupación,  text = paste('Date: ', as.Date(Fecha)))) +
  geom_bar(alpha = 1/2, position = "identity")+xlab("Fecha de ingreso al país")+ylab("TOTAL")+ggtitle("Variacion en la ocupación de los venezolanos 2017-2018")+theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(labels = date_format("%b/%y"))
ggplotly(p1,tooltip = c("count","Fecha","Ocupación"))



formal <- filter(datos3, Ocupación=="Empleado formal")
formal_freq<-formal %>%
  group_by(Fecha) %>%
  count() %>%
  ungroup() %>%
  mutate(formal=n)

informal<-filter(datos3,Ocupación=="Empleado Informal")
informal_freq<-informal %>%
  group_by(Fecha) %>%
  count() %>%
  ungroup() %>%
  mutate(informal=n)

desempleado<-filter(datos3,Ocupación=="Desempleado")
desempleado_freq<-desempleado%>%
  group_by(Fecha) %>%
  count() %>%
  ungroup() %>%
  mutate(desempleado=n)

independiente<-filter(datos3,Ocupación=="Independiente")
independiente_freq<-independiente%>%
  group_by(Fecha) %>%
  count() %>%
  ungroup() %>%
  mutate(independiente=n)

hogar<-filter(datos3,Ocupación=="Hogar")
hogar_freq<-hogar%>%
  group_by(Fecha) %>%
  count() %>%
  ungroup() %>%
  mutate(hogar=n)

a=ggplot() + 
  geom_line(data=formal_freq,aes(x=Fecha, y=formal),colour="red") +
  geom_line(data=informal_freq,aes(x=Fecha, y=informal),colour="blue") +
  geom_line(data=desempleado_freq,aes(x=Fecha, y=desempleado),colour="purple") +
  geom_line(data=independiente_freq,aes(x=Fecha, y=independiente),colour="orange") +
  geom_line(data=hogar_freq,aes(x=Fecha, y=hogar),colour="green") +
  scale_color_jco(name="Ocupacion",
                  breaks = c("formal","informal","desempleado","independiente","hogar"),
                  labels=c("Formal","Informal","Desempleado","Independiente","Hogar"))
  
  a+transition_reveal(Fecha)
  
datoxx<-table(datos3$Fecha,datos3$Ocupación)
View(datoxx)
datos4<-as.data.frame(datoxx)
datos4<-rename(datos4, Fecha = Var1, Ocupación = Var2, frecuencia = Freq)

gplot <- ggplot(datos4,aes(x=as.Date(Fecha),y=frecuencia,color=Ocupación,group=1)) + 
  geom_line()+scale_x_date(labels = date_format("%b/%y"))
ggplotly(gplot)
gplot+transition_reveal(as.Date(Fecha))
 


#GRAFICO DE BARRAS OCUPACION
dat<-data.frame(Ocupación)
mycolors<-c("blue","#FFC125","darkgreen","darkorange")
p6<-dat%>%
  group_by(Ocupación)%>%
  summarise(count=n())%>%
  plot_ly(labels=~Ocupación,
          values=~count,
          marker=list(colors=mycolors))%>%
  add_pie(hole=0.2)%>%
  layout(title = "Ocupacion de Venezolanos en Colombia",xaxis=list(zeroline=F, showline=F,showticklabels=F,showgrid=F),yaxis=list(zeroline=F, showline=F,showticklabels=F,showgrid=F))
p6

#grafico animado
library(gganimate)
ocupacion1 <- datos3%>%
  count(Ocupación,EDAD)%>%
  filter(EDAD %in% 18:60)
pgg <- ggplot(
  ocupacion1,
  aes(EDAD, n, color=Ocupación)) +   geom_line() +   scale_alpha() +
  labs(x = "Edades", y = "Cantidad de personas") + ggtitle("Ocupación de Venezolanos según edad")+theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold.italic"))+
  theme(legend.position = "top")
pgg+   
  geom_point(aes(group = seq_along(EDAD))) +
  transition_reveal(EDAD)


#grafico ocupacion segun el nivel de escolaridad

p2<- ggplot(datos3)+
  geom_bar(mapping = aes(NIVEL_ACADEMICO,fill=Ocupación))+ggtitle("Ocupación según nivel académico")+theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold.italic"))+
  coord_flip()
ggplotly(p2)
fct_count(datos3$Ocupación)

#GRAFICO DE PASTEL TOP MUNICIPIOS DONDE TIENEN MAYOR DESEMPLEO
mycolors<-c("blue","#FFC125","darkgreen","darkorange")
datos_desempleo<-filter(datos3,Ocupación=="Desempleado")
p2<-datos_desempleo%>%
  group_by(MUNICIPIO)%>%
  summarise(count=n())%>%
  filter(count>2700)%>%
  plot_ly(labels=~MUNICIPIO,
          values=~count,
          marker=list(colors=mycolors))%>%
  add_pie(hole=0.2)%>%
  layout(title = "Top 10 Municipios con mayor desempleo en venezolanos",xaxis=list(zeroline=F, showline=F,showticklabels=F,showgrid=F),yaxis=list(zeroline=F, showline=F,showticklabels=F,showgrid=F))
p2



##MAPA POR DEPARTAMENTO (desempleo)
library(rgdal)
library(sp)
col1<-readOGR("C:/Users/Usuario/Desktop/HACKATON/depto.shp")
plot(col1)
box()

col1@data
write.csv(as.data.frame(col1@data),file = "datosmapa.csv")
write.csv(datos_desempleo,file = "datos_desempleo.csv")
write.csv(datos3,file="datos3.csv")
write.csv(datos2,file = "datos2.csv")
write.csv(dat,file = "dat.csv")
View(fct_count(datos_desempleo$DEPARTAMENTO))
#Despues de esto, edito los archivos para ordenar los datos por departamento
datamap<-read.csv("C:/Users/Usuario/Desktop/analisis/mapa_desempleo_dpto.csv",sep=";",header = T,row.names = 1)
datamap
col1@data<-datamap
spplot(col1,"Venezolanos_Desempleados",main = "Población de venezolanos desempleados \n por Departamento")
View(col1@data)

#GRAFICO DE BARRAS
p1 <- datos3 %>%
  group_by(DEPARTAMENTO) %>%
  summarise(count = n()) %>%
  plot_ly(x = ~DEPARTAMENTO,
          y = ~count,
          color = "blue",
          type = 'bar') %>%
  layout(xaxis = list(title = "Departamento"),
         yaxis = list(title = 'Count'),
         title="Diagrama de barras desempleados por Dpto.")
p1


#GRAFICO DE PASTEL CON INTENCION DE PERMANENCIA
mycolors<-c("blue","#FFC125","darkgreen","darkorange")

p3<-datos3%>%
  group_by(INTENCIÓN.DE.PERMANENCIA)%>%
  summarise(count=n())%>%
  plot_ly(labels=~INTENCIÓN.DE.PERMANENCIA,
          values=~count,
          marker=list(colors=mycolors))%>%
  add_pie(hole=0.2)%>%
  layout(title = "Intencion de permanencia actual Venezolanos",xaxis=list(zeroline=F, showline=F,showticklabels=F,showgrid=F),yaxis=list(zeroline=F, showline=F,showticklabels=F,showgrid=F))
p3


#GRAFICO DE PASTEL AFILIADO A SEGURIDAD SOCIAL
mycolors<-c("blue","#FFC125","darkgreen","darkorange")

p4<-datos3%>%
  group_by(AFILIADO.A.SEGUDAD.SOCIAL.)%>%
  summarise(count=n())%>%
  plot_ly(labels=~AFILIADO.A.SEGUDAD.SOCIAL.,
          values=~count,
          marker=list(colors=mycolors))%>%
  add_pie(hole=0.2)%>%
  layout(title = "Venezolanos afiliados a seguridad social",xaxis=list(zeroline=F, showline=F,showticklabels=F,showgrid=F),yaxis=list(zeroline=F, showline=F,showticklabels=F,showgrid=F))
p4

#grafico convalidado segun ocupacion

p5<- ggplot(datos3)+
  geom_bar(mapping = aes(CONVALIDADO.,fill=Ocupación))+ggtitle("Ocupacion si es convalidado o no")+theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold.italic"))+
  coord_flip()
ggplotly(p5)



#GRAFICO DE PASTEL SECTOR DE OCUPACION
mycolors<-c("blue","#FFC125","darkgreen","darkorange")
datos_ocupacion<-filter(datos3,SECTOR.DE.OCUPACION!="No Aplica")
p4<-datos_ocupacion%>%
  group_by(SECTOR.DE.OCUPACION)%>%
  summarise(count=n())%>%
  filter(count>1000)%>%
  plot_ly(labels=~SECTOR.DE.OCUPACION,
          values=~count,
          marker=list(colors=mycolors))%>%
  add_pie(hole=0.2)%>%
  layout(title = "Mejores sectores de ocupacion",xaxis=list(zeroline=F, showline=F,showticklabels=F,showgrid=F),yaxis=list(zeroline=F, showline=F,showticklabels=F,showgrid=F))
p4

#Grafico ocupacion vs genero
p8<-ggplot(data=datos3) + geom_bar(mapping= aes(x= GENERO, fill= Ocupación), position = "fill")
ggplotly(p8)
