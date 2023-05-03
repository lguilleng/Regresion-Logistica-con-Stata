/* Regresión Logística */

/* Ejemplo Comportamiento de compra */

import excel "data_logistica", firstrow

encode sexo, gen(csexo)
encode comportamiento, gen(ccomportamiento)

recode ccomportamiento (1=1) (2=1) (3=0)
label drop ccomportamiento
label define c 1 "Compra" 0 "No compra"
label value ccomportamiento c

recode csexo (1=1) (2=0)
label drop csexo
label define cs 1 "Hombre" 0 "Mujer"
label value csexo cs

logit ccomportamiento edad tiempo csexo

/* Ejemplo Traumatología de la UNM */

graph box iss, over(fallece, relabel(1 "Sobrevive" 2 "Fallece" ) descending) ytitle(ISS) title(ISS por Fallece) name(iss) 

graph box rts, over(fallece, relabel(1 "Sobrevive" 2 "Fallece" ) descending) ytitle(RTS) title(RTS por Fallece) name(rts) 

graph box edad, over(fallece, relabel(1 "Sobrevive" 2 "Fallece" ) descending) ytitle(Edad) title(Edad por Fallece) name(edad) 

graph bar bp,over(fallece,relabel(1 "Sobrevive" 2 "Fallece") descending) ytitle("Proporción profunda") title("Proporción de herida profunda por Fallece") name(bp) 

graph combine iss rts edad bp

logistic fallece iss bp rts edad, coef

logistic fallece iss
estat gof

/* Ejemplo Eficacia de un programa de aprendizaje */

import excel "eficacia_programa", firstrow

label var cm "media de las calificaciones pasadas del alumno"
label var np "nota del alumno en un examen previo al periodo de aprendizaje"
label var psi "1 = estudió con el nuevo método didáctico, 0 = no estudió con el nuevo método didáctico"
label var mejora "1 = mejoró la nota del alumno, 0 = no mejoró la nota del alumno"

logit mejora cm np psi

sum cm np psi mejora

* ¿Cuál es el efecto marginal del nuevo método didáctico?

g pmejora_psi0 = 1/(1+ 1/exp(-13.021 + 2.826*cm + 0.095*21.938))
g pmejora_psi1 = 1/(1+1/exp(-13.021 + 2.826*cm + 0.095*21.938 +2.379))
label var pmejora_psi0 "Con PSI=0"
label var pmejora_psi1 "Con PSI=1"

twoway (scatter pmejora_psi0 cm, connect(l) msymbol(i)) (scatter pmejora_psi1 cm, connect(l) msymbol(i))

* ¿Qué tan bueno es el ajuste del modelo?

estat gof
estat gof, group(10)

* Capacidad predictiva

quietly logit mejora cm np psi
predict prob_mejora
g mejora_fit=prob_mejora>=0.5
label var mejora_fit "Condición estimada"
ssc install diagtest
diagt mejora mejora_fit


g mejora_fit2=prob_mejora>=0.3
diagt mejora mejora_fit2, notable

* Evaluar la sensibilidad y especificidad en umbrales (valor de corte) entre 0 y 1

lsens

lsens, genprob(corte) gensens(sensibilidad) genspec(especificidad) nograph

* Ontenemos punto de corte

  /* Paso 1: Guardar en variables, los valores de corte, sensibilidad y
             especificidad que generan la gráfica anterior */

     lsens, genprob(corte) gensens(sensibilidad) genspec(especificidad) nograph

  /* Paso 2: Crear una variable que contenga el valor absoluto de la diferencia
             entre sensibilidad y especificidad */

     gen diferencia = abs(sensibilidad-especificidad)

  /* Paso 3: Ordenar ascendentemente en base a la variable DIFERENCIA */

     sort diferencia

  /* Paso 4: Capturar los valores de las variables CORTE y SENSIBILIDAD
             que se encuentran en el primer registro */

     local x=corte[1]
     local y=sensibilidad[1]

  /* Paso 5: Graficar la SENSIBILIDAD, ESPECIFICIDAD en función a CORTE,
             añadiendo las línea vertical “x” y la línea horizontal “y”, que
             corresponden a la intersección de SENSIBILIDAD y ESPECIFICIDAD. */

    format corte %11.2gc
    format sensibilidad %11.2gc
    format especificidad %11.2gc
    twoway (scatter especificidad corte, c(l) sort( especificidad corte)) /*
        */ (scatter sensibilidad corte, c(l) sort(corte sensibilidad ) /*
        */ xline(`x') yline(`y') xlab(0 0.2 `x' 0.6 0.8 1) ylab(0 0.2 `y' 0.6 1))

g mejora_fit3=prob_mejora>=0.36
diagt mejora mejora_fit3


* Curva ROC

  lroc

* El comando estat classification

  logit mejora cm np psi
  estat classification
  estat classification, cut(0.36)

/* ¿Cómo podemos  evaluar la capacidad predictiva de un modelo y su capacidad
   para generalizar a datos nuevos e independientes */

   ssc install cvauroc
   
   /* Consideramos que un bajo peso al nacer es menor a 2500 */
      use peso_nacer, clear
      generate lbw = cond(bweight<2500,1,0)

   /* Estiamos la probabilidad de tener un bebé con peso bajo al nacer */
      quietly logistic lbw mage medu mmarried prenatal1 fedu mbsmoke mrace fbaby

   /* Estimamos las probabilidades para obtener el valor de AUC del modelo */
      predict fitted, pr

   /* Calculamos el AUC */
      roctab lbw fitted

/* Aplicación de Logit a la ENHO

   use sumaria-2021, clear
   sum mieperho percepho gashog2d inghog2d
   tab pobreza
   g gasmenp=gashog2d/12/mieperho
   g ingmenp=inghog2d/12/mieperho
   sum gasmenp ingmenp

   g pobre=pobreza<=2
   label define pobre 1 "Pobre" 0 "No pobre"
   label value pobre pobre
   tab pobre

   g area=estrato<=5
   label define area 1 "urbano" 0 "rural"
   label value area area
   tab area

   svyset conglome [pw= factor07 ], strata(estrato)
   svy: logit pobre ingmenp gasmenp mieperho percepho area
   margins, dydx(*)
   margins, eyex(*)
