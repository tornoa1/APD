#########################################################
#  Ejemplo: data                                #
#########################################################

#_______________________________________________________
# Paso 1: Obtener y procesar la data
#_______________________________________________________
# Usando librería arules
library(arules)
library(ggraph)
library(arulesViz)
library(TSP)

data <- read.transactions("D:/ULIMA/2024-2/Analitica Predictiva de Datos/Evaluaciones/SIMULACION/EVA02/Cosmetic.txt", header = F, format = 'basket', sep = ',')

View(data)
# Ver la data sparse
data@data
# Visualizar la matriz de transacciones (p. ej. para las 5 primeras transacciones)
image(data[1:20])
# Visualizar la matriz de transacciones (p. ej. seleccionar al azar 100 transacciones)
set.seed(180)
image(sample(data, 100))

#----------------------------------------------------------------------------------
# Interpretaciones :
# ---------------------------------------------------------------------------------
# Los puntos deberán verse dispersos con un patrón aleatorio.
# Buscar patrones no aleatorios:
# - Si un ítem aparece en todas las transacciones podría tratarse de información que no 
#   corresponde a un item comprado.
# - Si se ordenan los items por algún criterio, por ejemplo fecha de compra, podrían
#   detectarse algún comportamiento estacional (Halloween, Navidad, etc.)
#-----------------------------------------------------------------------------------

# Ver un resumen
summary(data)
#----------------------------------------------------------------------------------
# Interpretaciones :
# ---------------------------------------------------------------------------------
# El valor de densidad de 0.02578629 (2.6 %) se refiere a la proporci?n de celdas
# en la matriz que son distintas de cero.  Dado que hay 9835 * 171 = 1681785 celdas
# en la matriz, es posible calcular el n?mero total 1681785 * 0.02578629 = 43367
# de ítems comprados en la tienda durante los 30 días de funcionamiento
#-----------------------------------------------------------------------------------
# En el siguiente bloque de la salidad de summary() se muestran los items más frecuentes
# encontrados en la base de datos de transacciones. Dado que 2513/9835 = 0.2555,
# podemos determinar que "leche entera" aparece en un 25.6% de todas las transacciones, 
# del mismo modo se interpetan el resto de los items frecuentes.
#-----------------------------------------------------------------------------------
# Finalmente, se presentan un conunto de estadísticas sobre el tamaño de las transacciones,
# Un total de  2159 transacciones contienen tan sólo un ítem, mientras hubo una transacción
# con 32 ítems. 
#-----------------------------------------------------------------------------------
#Calcular numero de productos comprados
957*14*0.3273623
#-----------------------------------------------------------------------------------

# Mostrar las transacciones
labels(data)

# Mostrar un subconjunto de transacciones (p. ej. las cinco primeras)
inspect(data[1:5])

#percentiles
quantile(size(data), probs = 0.8) #probs = 0.45 es sacar el percentil 45

# Mostrar el soporte (proporci?n de transacciones) de un item (p. ej. de los tres primeros)
itemFrequency(data[, 1:12])
# Visualizar el soporte de los items (p. ej. de aquellos items con una proporci?n mayor a 0.10)
itemFrequencyPlot(data, support = 0.5)

# Visualizar el soporte de los items (p. ej.de los 20 ?tems con mayor soporte)
itemFrequencyPlot(data, topN = 20)

#_______________________________________________________
# Paso 2: Entrenar el modelo con los datos
#_______________________________________________________
help(apriori)
apriori(data)
#----------------------------------------------------------------------------------
# Comentario :
# ---------------------------------------------------------------------------------
# Recordar que por defecto un soporte = 0.1 es usado para generar una regla, es decir
# que al menos un item debe aparecer en 0.1 * 9835 = 983.5 transacciones. Dado que solo
# ocho item tienen esta frecuencia, es bastante predecible que no se encuentre ninguna 
# regla de asociacion.
# ---------------------------------------------------------------------------------
# Recomendaciones para fijar un soporte y confianza m?nimo :
# --------------------------------------------------------------------------------- 
# - Pensar en el menor número de transacciones que necesites para considerar que un patrón 
#   es interesante. Por ejemplo, si se argumenta que si se compra un art?culo dos veces al día
#  (alrededor de 60 veces en un mes), esto puede ser un patr?n interesante. A partir de ah?,
#   es posible calcular el nivel de apoyo necesario para encontrar sólo las reglas que coincidan
#   con al menos ese número de transacciones. Como 60 de 9835 es aprox. 0.006, se puede establecer
#   el soporte a partir de este valor.
# - Determinar la confianza mínima involucra realizar un balance muy delicado.  Por un lado, si la
#   confianza es es demasiado baja, es posible obtener un número abrumadoramente alto de reglas con
#   poca fiabilidad (p. ej. pañales de bebe son comprados junto con muchos productos). Por otro lado,
#   si se fija una confianza muy alta, se limitaran a las reglas que son obvias o inevitable, (p. ej.
#   pañales de bebe son comprados junto a biberones o leche para recien nacidos).
# - El nivel de confianza mínimo adecuado depende en gran medida de los objetivos del análisis.
#   Si se parte de un valor conservador, siempre se puede reducir para ampliar la búsqueda.
# - Para este ejemplo se iniciará con un umbral para la confianza de 0.25, esto indica que para que una
#   regla de asociaci?n se considere relevante debería ocurrir en al menos un 25% de las veces. Esto
#   ayudar?a a eliminar la mayor?a de reglas poco fiables, al mismo tiempo que permite un cierto margen
#   para incentivar el comportamiento del cliente con promociones específicas.
# - Adicionalmente al soporte y la confianza, ayuda fijar minlen = 2  para eliminar reglas que contengan
#   menos de dos ítems.  Esto previene obtener reglas poco interesantes que se generan porque un item 
#   es comprado muy frecuentemente, por ejemplo {} ? leche entera. Esta regla cumple con el m?nimo de
#   soporte y confianza porque leche entera es comprada en m?s del 25% de las transacciones, pero no brinda
#   un insight accionable. 
#-----------------------------------------------------------------------------------

datarules <- apriori(data, parameter = list(support =0.1,
                                                      confidence = 0.5, minlen = 4))
datarules

# Visualización
# --------------
library(arulesViz)
plot(datarules)
plot(datarules,method="graph",control=list(alpha=1))

subrules <- head(sort(datarules, by="lift"), 10)
plot(subrules)
plot(subrules,method="graph",control=list(alpha=1))
plot(subrules,method="paracoord")

#_______________________________________________________
# Paso 3: Evaluar el modelo
#_______________________________________________________

summary(datarules)

#----------------------------------------------------------------------------------
# Interpretaciones :
# ---------------------------------------------------------------------------------
# - La distribución para la longitud de las reglas (rule length distribution ) muestra
#   el n?mero de reglas existentes para cierta cantidad de items. Por ejemplo, en 
#   la salida se observa que 150 reglas tienen solo dos items, mientras que 297 tienen
#   tres, y 16 tienen 4.  Adem?s se muestra un resumen estad?stico.
# - El resumen de las medidas de calidad para las reglas (rule quality measures) es 
#   importante para evaluar si los par?metros fijados son adecuados.  Por ejemplo, si
#   la mayor?a de las reglas tuvieran un soporte y confianza muy cercana al m?nimo de
#   los umbrales fijados eso implicar?a que quiz? se fij? un l?mite demasiado alto.
#-----------------------------------------------------------------------------------

# Mostrar las tres primeras reglas de asociacion
inspect(datarules[1:3])

#----------------------------------------------------------------------------------
# Interpretaciones :
# ---------------------------------------------------------------------------------
# - La primera regla puede ser leida de la siguiente forma: "Si un cliente compra
#   plantas en macetas, también comprará leche entera.
#   Esto se da con un soporte de 0.007 y una confianza de 0.400, lo cual implica
#   que esta regla cubre el  0.7% de las transacciones y es cierta para el 40% de las
#   compras que involucren plantas en maceta. Coverage es el soporte de la parte 
#   izquierda de la regla (antecedente), las macetas de plantas aparece en un 1.17% en el conjunto de transacciones
# - El valor del lift nos dice que tanto m?s probable es que un cliente compre leche
#   entera en relación al cliente típico, sabiendo que compró plantas en macetas. Dado que se
#   sabe que cerca del 25.6% de los clientes compran leche entera (soporte), mientras
#   que un 40% de los clientes compran plantas en maceta (confianza), es posible calcular
#   el valor del lift 0.40/0.256 = 1.56.
# - ?Es razonable la regla anterior? Clasificar las reglas en: accionables/triviales/inexplicables
#-----------------------------------------------------------------------------------

#_______________________________________________________
# Paso 4: Mejorar la performance del modelo
#_______________________________________________________

# Mostrar las 5 reglas con mayor lift
inspect(sort(datarules, by = "lift")[1:5])

#----------------------------------------------------------------------------------
# Interpretaciones :
# ---------------------------------------------------------------------------------
# - La primera regla, con un lift de aprox. 3.96, implica que las personas que compran hierbas son 
#   casi cuatro veces m?s propensos a comprar hortalizas que el cliente t?pico
#   (algún tipo de guiso?)
# - La regla n?mero dos es tambi?n interesante. Crema batida es m?s de tres veces m?s probable
#   de ser encontrada en una canasta de compras con bayas en comparaci?n con otras canastas.
#   (?alg?n tipo de postre?)
#-----------------------------------------------------------------------------------

# Subconjuntos de reglas
bayasrules <- subset(datarules, items %in% "bayas")
inspect(bayasrules)

bayas_yogurtrules <- subset(datarules, items %in%c("bayas", "yogurt"))
inspect(bayas_yogurtrules)

bayasrules <- subset(datarules, items %ain%c("bayas", "yogurt"))
inspect(bayasrules)


#----------------------------------------------------------------------------------
# Uso de subset() :
# ---------------------------------------------------------------------------------
# - La palabra clave items empareja un item que aparezca en alguna regla. Es posible delimitar
#   que esta ocurra solo a la izquierda o derecha usando lhs y rhs.
# - El operador %in% significa que al menos uno de los items debe ser encontrado, de la lista de
#   items definidos.  Si se desea encontrar reglas con galletas y yogurt,deber?a escribirse
#   %in%c("galletas", "yogurt").
# - Existen otros operadores disponibles para emparejamiento parcial (%pin%) y emparejamiento
#   completo (%ain%). Emparejamiento parcial permite encontrar ambos: citrus fruit y tropical fruit
#   en una sola busqueda: items %pin% "fruit". Emparejamiento completo requiere que todos los items
#   listados est?n presentes. Por ejemplo, items %ain% c("galletas", "yogurt") encuentra solo las
#   reglas con yogurt y galletas al mismo tiempo.
# - Los subconjuntos tambien pueden ser imitados por soporte, confianza o lift. Por ejemplo,
#   confidence > 0.50.
# - Los criterios de emparejamiento pueden ser combinados con operadores de R estandar y logicos como 
#   y (&), o (|), y negacion (!).
#-----------------------------------------------------------------------------------

reglas_galletas1 <-apriori(data, parameter = list(support =0.1,
                                                       confidence = 0.5, minlen = 2),
                           appearance = list(rhs = "Mascara"))

inspect(sort(reglas_galletas1, by = "lift")[1:5])

reglas_galletas2 <-apriori(data, parameter = list(support =0.006,
                                                       confidence = 0.25, minlen = 2),
                           appearance = list(lhs = "Mascara"))

inspect(sort(reglas_galletas2, by = "lift"))

# Exportar las reglas obtenidas
setwd("D:/ULIMA/2024-2/Analitica Predictiva de Datos/RStudio/REGLAS DE ASOCIACION/RESULTADOS")
write(datarules, file = "datarules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# Convertir reglas en dataframe
datarules_df <- as(datarules, "data.frame")
head(datarules_df)




#----------------SIMULACRO---------------------
data <- read.transactions("D:/ULIMA/2024-2/Analitica Predictiva de Datos/Evaluaciones/SIMULACION/EVA02/Cosmetic.txt", header = F, format = 'basket', sep = ',')

#PREGUNTA1
summary(data)
#156 transacciones que tienen 5 items

#PREGUNTA2
#cuantos productos como minimo debe tener una transaccion para pertenecer al 15%
#mayor (85) registros de compra
quantile(size(data), probs = 0.85) #probs = 0.45 es sacar el percentil 45

#PREGUNTA3
itemFrequency(data) #[, 1:14]

#PREGUNTA4
itemFrequencyPlot(data, support = 0.08)
itemFrequencyPlot(data, topN = 20)

#PREGUNTA5
#PREGUNTA5.1
datarules <- apriori(data, parameter = list(support =0.008,
                                                      confidence = 0.01, minlen = 2))
summary(datarules)
datarules
#PREGUNTA5.2
datarules <- apriori(data, parameter = list(support =0.008,
                                            confidence = 0.01, minlen = 2))
summary(datarules)

inspect(sort(datarules, by = "lift")[1:5])

#PREGUNTA6
#rhs consecuente - lhs antecedente
reglas_mascara <-apriori(data, parameter = list(support =0.008,
                                                     confidence = 0.01, minlen = 2),
                         appearance = list(lhs = "Lápiz_Ceja"))
inspect(sort(reglas_mascara, by = "lift"))
