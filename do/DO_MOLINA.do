* ============================================
* ANÁLISIS DE COINTEGRACIÓN
* Remesas y Consumo de los Hogares en Ecuador
* Período: 2015Q1 - 2024Q4
* ============================================

clear all
set more off
cd "C:/Datos_Econometria"

* ============================================
* PREPARACIÓN DE DATOS
* ============================================

* Importar datos del BCE
import excel "C:\Users\USER\Documents\ESTUDIOS_AVANZADA\MOLINA\data\DATA_MOLINA.xlsx", sheet("Hoja1") firstrow clear 

* Generar variable temporal
gen time = tq(2015q1) + _n - 1
format time %tq
tsset time

* Transformación logarítmica
gen logCONS = log(CONSUMO_HOGARES)
gen logREM = log(Remesas)
gen logPIB = log(PIB)
gen logCRED = log(CarteraBancoPrivado)

* Etiquetas
label var logCONS "Log Consumo Hogares"
label var logREM "Log Remesas"
label var logPIB "Log PIB Real"
label var IPC "Índice de Precios al Consumidor"
label var logCRED "Log Crédito Privado"

* ============================================
* PASO 1: IDENTIFICACIÓN DETERMINISTA
* ============================================

gen t = _n

* Para cada variable, evaluar tendencia
reg logCONS t
test t
reg logREM t
test t
reg logPIB t
test t

* ============================================
* PASO 2: PRUEBAS DE RAÍZ UNITARIA EN NIVELES
* ============================================

* Variable dependiente: logCONS
dfuller logCONS, trend
pperron logCONS, trend
kpss logCONS, trend

* Variable independiente principal: logREM
dfuller logREM, trend
pperron logREM, trend
kpss logREM, trend

* Variables de control
dfuller logPIB, trend
pperron logPIB, trend
kpss logPIB, trend

dfuller IPC, trend
pperron IPC, trend
kpss IPC, trend

dfuller logCRED, trend
pperron logCRED, trend
kpss logCRED, trend

* ============================================
* PASO 3: VERIFICACIÓN EN PRIMERAS DIFERENCIAS
* ============================================

* Generar primeras diferencias
gen d_logCONS = D.logCONS
gen d_logREM = D.logREM
gen d_logPIB = D.logPIB
gen d_IPC = D.IPC
gen d_logCRED = D.logCRED

* Evaluar componente determinista en diferencias
reg d_logCONS t
reg d_logREM t

* Tests en diferencias (usar drift si no hay tendencia)
dfuller d_logCONS, drift
pperron d_logCONS, drift
kpss d_logCONS, drift

dfuller d_logREM, drift
pperron d_logREM, drift
kpss d_logREM, drift

* ============================================
* PASO 4: REGRESIÓN MCO EN NIVELES
* ============================================

reg logCONS logREM logPIB IPC logCRED

* Guardar estadísticos clave
scalar R2 = e(r2)
scalar R2_adj = e(r2_a)
scalar F_stat = e(F)
scalar N_obs = e(N)

display "R-cuadrado: " R2
display "R-cuadrado ajustado: " R2_adj
display "Estadístico F: " F_stat
display "Observaciones: " N_obs

* ============================================
* PASO 5: SIGNIFICANCIA DE LOS BETAS
* ============================================

* La regresión anterior muestra t-stats automáticamente
* Test de significancia conjunta
test logREM logPIB IPC logCRED

* Test de restricciones específicas
test logREM = 0
test logPIB = 0

* ============================================
* PASO 6: DIAGNÓSTICO DE PROBLEMAS DEL MODELO
* ============================================

* 6.1 Heterocedasticidad
estat hettest
estat imtest, white

* 6.2 Autocorrelación
estat bgodfrey, lags(1/4)
estat dwatson

* 6.3 Especificación
estat ovtest

* 6.4 Normalidad
predict res1, resid
swilk res1
sktest res1
jb res1

* 6.5 Multicolinealidad
vif

* 6.6 Estabilidad estructural
estat sbcusum
cusum res1

* ============================================
* PASO 7: PREDICCIÓN DE RESIDUOS
* ============================================

predict ehat, resid

* Gráfico de residuos
tsline ehat, title("Residuos del Modelo") ///
    ytitle("Residuos") xtitle("Tiempo")

* Correlograma
ac ehat, lags(12)
pac ehat, lags(12)

* ============================================
* PASO 8: TEST DE COINTEGRACIÓN (RESIDUOS)
* ============================================

* Pruebas sobre residuos (sin constante)
dfuller ehat, noconstant
pperron ehat, noconstant
kpss ehat, noconstant

* Interpretación
display "========================================"
display "RESULTADOS DE COINTEGRACIÓN:"
display "Si los residuos son I(0), existe cointegración"
display "========================================"

* ============================================
* GRÁFICOS ADICIONALES
* ============================================

* Series en niveles
twoway (tsline logCONS, yaxis(1)) ///
       (tsline logREM, yaxis(2)), ///
       title("Consumo y Remesas en Logaritmos") ///
       legend(order(1 "Log Consumo" 2 "Log Remesas"))

* Relación bivariada
scatter logCONS logREM || lfit logCONS logREM, ///
    title("Relación Consumo-Remesas") ///
    xtitle("Log Remesas") ytitle("Log Consumo")

* ============================================
* TABLA RESUMEN DE RESULTADOS
* ============================================

quietly reg logCONS logREM logPIB IPC logCRED
outreg2 using "resultados_modelo.doc", replace word ///
    title("Modelo de Cointegración: Consumo y Remesas") ///
    addstat(R-squared, e(r2), F-stat, e(F), ///
            Durbin-Watson, r(dw))

log close