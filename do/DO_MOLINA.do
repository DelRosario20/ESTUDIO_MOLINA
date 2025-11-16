* ============================================
* ANÁLISIS DE COINTEGRACIÓN
* Remesas y Consumo de los Hogares en Ecuador
* Período: 2015Q1 - 2024Q4
* Autor: Estudiante UNEMI - Econometría Aplicada
* Fecha: Noviembre 2025
* ============================================

clear all
set more off

cd "C:\Users\USER\Documents\ESTUDIOS_AVANZADA\MOLINA"

* Crear tabla resumen en formato texto
log using "resultados/resultados_do.txt", text replace

* ============================================
* PREPARACIÓN DE DATOS
* ============================================

* Importar datos desde CSV delimitado
import delimited "data\DATA_MOLINA_DELIMITADO.csv", clear varnames(1) encoding(UTF-8)

* Verificar importación
desc
list in 1/5

* Limpiar nombres de variables (remover espacios)
rename gastodeconsumofinalhogar consumo_hogares
rename pibre pib_real
rename índicedepreciosalconsumi ipc
rename variaciónipc var_ipc

* Convertir variables numéricas con separadores de miles
destring remesas, replace ignore(",")
destring consumo_hogares, replace ignore(",")
destring pib_real, replace ignore(",")

* Generar variable temporal desde el string Trimestre
gen year = real(substr(trimestre, 1, 4))
gen quarter = real(substr(trimestre, 6, 1))
gen time = yq(year, quarter)
format time %tq
tsset time

* Estadísticas descriptivas iniciales
summarize remesas consumo_hogares pib_real ipc

* ============================================
* TRANSFORMACIONES LOGARÍTMICAS
* ============================================

* Variable dependiente y variables independientes
gen logCONS = ln(consumo_hogares)
gen logREM = ln(remesas)
gen logPIB = ln(pib_real)

* Etiquetas descriptivas
label var logCONS "Log del Consumo Final de los Hogares"
label var logREM "Log de las Remesas Recibidas"
label var logPIB "Log del PIB Real"
label var ipc "Índice de Precios al Consumidor"
label var var_ipc "Variación del IPC (%)"

* Verificar transformaciones
summarize logCONS logREM logPIB ipc

* ============================================
* PASO 1: IDENTIFICACIÓN DE COMPONENTES DETERMINISTAS
* ============================================

* Generar variable de tendencia temporal
gen t = _n

* Evaluar presencia de tendencia determinista en cada serie
di _newline(2) "=== PASO 1: IDENTIFICACIÓN DETERMINISTA ===" _newline

* Consumo de hogares
quietly reg logCONS t
di "1. Log Consumo Hogares vs Tendencia:"
test t
scalar p_tendencia_cons = r(p)
di "   p-value = " %6.4f p_tendencia_cons

* Remesas
quietly reg logREM t
di _newline "2. Log Remesas vs Tendencia:"
test t
scalar p_tendencia_rem = r(p)
di "   p-value = " %6.4f p_tendencia_rem

* PIB
quietly reg logPIB t
di _newline "3. Log PIB vs Tendencia:"
test t
scalar p_tendencia_pib = r(p)
di "   p-value = " %6.4f p_tendencia_pib

* IPC
quietly reg ipc t
di _newline "4. IPC vs Tendencia:"
test t
scalar p_tendencia_ipc = r(p)
di "   p-value = " %6.4f p_tendencia_ipc

di _newline "Nota: Si p-value < 0.05, la tendencia es significativa" _newline

* ============================================
* PASO 2: PRUEBAS DE RAÍZ UNITARIA EN NIVELES
* ============================================

di _newline(2) "=== PASO 2: PRUEBAS DE RAÍZ UNITARIA EN NIVELES ===" _newline

* ===== Variable dependiente: logCONS =====
di "===== LOG CONSUMO HOGARES (logCONS) =====" _newline

di "Test Augmented Dickey-Fuller (ADF):"
dfuller logCONS, trend lags(4)

di _newline "Test Phillips-Perron (PP):"
pperron logCONS, trend lags(4)

di _newline "Test KPSS:"
kpss logCONS, trend

* ===== Variable independiente principal: logREM =====
di _newline(2) "===== LOG REMESAS (logREM) =====" _newline

di "Test Augmented Dickey-Fuller (ADF):"
dfuller logREM, trend lags(4)

di _newline "Test Phillips-Perron (PP):"
pperron logREM, trend lags(4)

di _newline "Test KPSS:"
kpss logREM, trend

* ===== Variable de control 1: logPIB =====
di _newline(2) "===== LOG PIB REAL (logPIB) =====" _newline

di "Test Augmented Dickey-Fuller (ADF):"
dfuller logPIB, trend lags(4)

di _newline "Test Phillips-Perron (PP):"
pperron logPIB, trend lags(4)

di _newline "Test KPSS:"
kpss logPIB, trend

* ===== Variable de control 2: IPC =====
di _newline(2) "===== ÍNDICE DE PRECIOS AL CONSUMIDOR (IPC) =====" _newline

di "Test Augmented Dickey-Fuller (ADF):"
dfuller ipc, trend lags(4)

di _newline "Test Phillips-Perron (PP):"
pperron ipc, trend lags(4)

di _newline "Test KPSS:"
kpss ipc, trend

di _newline "INTERPRETACIÓN:" _newline ///
"ADF y PP: H0 = raíz unitaria (no estacionaria)" _newline ///
"  - Si p-value > 0.05 → NO rechazo H0 → Serie es I(1)" _newline ///
"KPSS: H0 = estacionaria" _newline ///
"  - Si p-value < 0.05 → Rechazo H0 → Serie es I(1)" _newline

* ============================================
* PASO 3: VERIFICACIÓN EN PRIMERAS DIFERENCIAS
* ============================================

di _newline(2) "=== PASO 3: PRUEBAS EN PRIMERAS DIFERENCIAS ===" _newline

* Generar primeras diferencias
gen d_logCONS = D.logCONS
gen d_logREM = D.logREM
gen d_logPIB = D.logPIB
gen d_ipc = D.ipc

label var d_logCONS "Primera diferencia de Log Consumo"
label var d_logREM "Primera diferencia de Log Remesas"
label var d_logPIB "Primera diferencia de Log PIB"
label var d_ipc "Primera diferencia de IPC"

* Evaluar componente determinista en diferencias
di "Evaluación de tendencia en diferencias:" _newline

quietly reg d_logCONS t
di "1. D.logCONS vs tendencia:"
test t
di "   p-value = " %6.4f r(p)

quietly reg d_logREM t
di _newline "2. D.logREM vs tendencia:"
test t
di "   p-value = " %6.4f r(p)

di _newline(2) "Pruebas de raíz unitaria en PRIMERAS DIFERENCIAS:" _newline

* ===== D.logCONS =====
di "===== PRIMERA DIFERENCIA LOG CONSUMO (D.logCONS) =====" _newline

di "Test ADF:"
dfuller d_logCONS, drift lags(4)

di _newline "Test KPSS:"
kpss d_logCONS

* ===== D.logREM =====
di _newline(2) "===== PRIMERA DIFERENCIA LOG REMESAS (D.logREM) =====" _newline

di "Test ADF:"
dfuller d_logREM, drift lags(4)

di _newline "Test KPSS:"
kpss d_logREM

* ===== D.logPIB =====
di _newline(2) "===== PRIMERA DIFERENCIA LOG PIB (D.logPIB) =====" _newline

di "Test ADF:"
dfuller d_logPIB, drift lags(4)


di _newline "Test KPSS:"
kpss d_logPIB

* ===== D.ipc =====
di _newline(2) "===== PRIMERA DIFERENCIA IPC (D.ipc) =====" _newline

di "Test ADF:"
dfuller d_ipc, drift lags(4)

di _newline "Test KPSS:"
kpss d_ipc

di _newline "INTERPRETACIÓN:" _newline ///
"Si las diferencias son I(0) y los niveles I(1)," _newline ///
"entonces las series están integradas de orden 1." _newline

* ============================================
* PASO 4: REGRESIÓN MCO EN NIVELES
* ============================================

di _newline(2) "=== PASO 4: REGRESIÓN MCO EN NIVELES ===" _newline

* Modelo de cointegración: logCONS = f(logREM, logPIB, IPC)
reg logCONS logREM logPIB ipc

* Guardar estadísticos clave
scalar R2 = e(r2)
scalar R2_adj = e(r2_a)
scalar F_stat = e(F)
scalar N_obs = e(N)
scalar RMSE = e(rmse)

* Reporte de estadísticos
di _newline "========================================"
di "ESTADÍSTICOS CLAVE DEL MODELO:"
di "========================================"
di "R-cuadrado:           " %6.4f R2
di "R-cuadrado ajustado:  " %6.4f R2_adj
di "Estadístico F:        " %8.2f F_stat
di "Prob > F:             " %6.4f Ftail(e(df_m), e(df_r), e(F))
di "RMSE:                 " %6.4f RMSE
di "Observaciones:        " N_obs
di "========================================" _newline

* Verificación del R² mínimo requerido
if R2 >= 0.70 {
    di "✓ CUMPLE: R² >= 0.70 (Modelo explica al menos 70% de la variabilidad)"
}
else {
    di "✗ NO CUMPLE: R² < 0.70 (Se requiere mayor poder explicativo)"
}

* ============================================
* PASO 5: SIGNIFICANCIA DE LOS COEFICIENTES (BETAS)
* ============================================

di _newline(2) "=== PASO 5: SIGNIFICANCIA DE LOS COEFICIENTES ===" _newline

* Reestimar modelo para análisis detallado
quietly reg logCONS logREM logPIB ipc

* Test de significancia conjunta de TODAS las variables
di "Test de significancia CONJUNTA (todas las variables):"
test logREM logPIB ipc
scalar F_conjunta = r(F)
scalar p_conjunta = r(p)
di "   F = " %8.4f F_conjunta
di "   Prob > F = " %6.4f p_conjunta

if p_conjunta < 0.05 {
    di "   ✓ Los coeficientes son conjuntamente significativos (α = 0.05)" _newline
}
else {
    di "   ✗ Los coeficientes NO son conjuntamente significativos (α = 0.05)" _newline
}

* Test de significancia INDIVIDUAL de cada variable
di "Tests de significancia INDIVIDUAL:" _newline

di "1. Log Remesas (logREM):"
test logREM
di "   F = " %8.4f r(F) "   p-value = " %6.4f r(p)
if r(p) < 0.05 {
    di "   ✓ Significativa al 5%" _newline
}
else {
    di "   ✗ NO significativa al 5%" _newline
}

di "2. Log PIB (logPIB):"
test logPIB
di "   F = " %8.4f r(F) "   p-value = " %6.4f r(p)
if r(p) < 0.05 {
    di "   ✓ Significativa al 5%" _newline
}
else {
    di "   ✗ NO significativa al 5%" _newline
}

di "3. IPC:"
test ipc
di "   F = " %8.4f r(F) "   p-value = " %6.4f r(p)
if r(p) < 0.05 {
    di "   ✓ Significativa al 5%" _newline
}
else {
    di "   ✗ NO significativa al 5%" _newline
}

* ============================================
* PASO 6: DIAGNÓSTICO DE PROBLEMAS DEL MODELO
* ============================================

di _newline(2) "=== PASO 6: DIAGNÓSTICO DE SUPUESTOS CLÁSICOS ===" _newline

* 6.1 HETEROCEDASTICIDAD
di "===== 6.1 HETEROCEDASTICIDAD =====" _newline

di "Test de Breusch-Pagan:"
quietly reg logCONS logREM logPIB ipc
estat hettest
scalar p_bp = r(p)
if p_bp > 0.05 {
    di "   ✓ NO hay evidencia de heterocedasticidad (p = " %6.4f p_bp ")" _newline
}
else {
    di "   ✗ Existe heterocedasticidad (p = " %6.4f p_bp ")" _newline
}

di "Test de White:"
estat imtest, white
scalar p_white = r(p)
if p_white > 0.05 {
    di "   ✓ NO hay evidencia de heterocedasticidad (p = " %6.4f p_white ")" _newline
}
else {
    di "   ✗ Existe heterocedasticidad (p = " %6.4f p_white ")" _newline
}

* 6.2 AUTOCORRELACIÓN
di _newline "===== 6.2 AUTOCORRELACIÓN =====" _newline

di "Test de Breusch-Godfrey (LM):"
estat bgodfrey, lags(1/4)

di _newline "Estadístico Durbin-Watson:"
estat dwatson
scalar dw_stat = r(dw)
di "   DW = " %6.4f dw_stat
if dw_stat >= 1.5 & dw_stat <= 2.5 {
    di "   ✓ No hay autocorrelación severa" _newline
}
else {
    di "   ✗ Posible autocorrelación" _newline
}

* 6.3 ESPECIFICACIÓN DEL MODELO
di "===== 6.3 ESPECIFICACIÓN (Test RESET de Ramsey) =====" _newline
estat ovtest
scalar p_reset = r(p)
if p_reset > 0.05 {
    di "   ✓ Modelo bien especificado (p = " %6.4f p_reset ")" _newline
}
else {
    di "   ✗ Problemas de especificación (p = " %6.4f p_reset ")" _newline
}

* 6.4 NORMALIDAD DE RESIDUOS
di "===== 6.4 NORMALIDAD DE RESIDUOS =====" _newline

predict res1, resid
label var res1 "Residuos del modelo"

di "Test de Shapiro-Wilk:"
swilk res1
scalar p_sw = r(p)
if p_sw > 0.05 {
    di "   ✓ Residuos normales (p = " %6.4f p_sw ")" _newline
}
else {
    di "   ✗ Residuos NO normales (p = " %6.4f p_sw ")" _newline
}

di "Test de Skewness-Kurtosis:"
sktest res1

di _newline "Test de Jarque-Bera:"
quietly sum res1
scalar N = r(N)
scalar S = r(skewness)
scalar K = r(kurtosis)
scalar JB = N/6 * (S^2 + (K-3)^2/4)
scalar p_jb = chi2tail(2, JB)
di "   JB = " %8.4f JB
di "   p-value = " %6.4f p_jb
if p_jb > 0.05 {
    di "   ✓ Residuos normales" _newline
}
else {
    di "   ✗ Residuos NO normales" _newline
}

* 6.5 MULTICOLINEALIDAD
di "===== 6.5 MULTICOLINEALIDAD (VIF) =====" _newline
quietly reg logCONS logREM logPIB ipc
vif
di _newline "Nota: VIF > 10 indica multicolinealidad severa" _newline

* 6.6 ESTABILIDAD ESTRUCTURAL
di "===== 6.6 ESTABILIDAD ESTRUCTURAL =====" _newline

di "Test CUSUM (gráfico guardado):"
quietly reg logCONS logREM logPIB ipc
estat sbcusum

* ============================================
* PASO 7: PREDICCIÓN Y ANÁLISIS DE RESIDUOS
* ============================================

di _newline(2) "=== PASO 7: ANÁLISIS DE RESIDUOS ===" _newline

* Predicción de residuos (ya generados como res1)
quietly reg logCONS logREM logPIB ipc
predict ehat, resid
label var ehat "Residuos estimados (ehat)"

* Estadísticas descriptivas de residuos
di "Estadísticas descriptivas de los residuos:" _newline
summarize ehat, detail

* Gráfico de residuos en el tiempo
tsline ehat, title("Residuos del Modelo de Cointegración") ///
    ytitle("Residuos") xtitle("Tiempo") ///
    tlabel(2015q1(4)2024q4, angle(45)) ///
    yline(0, lcolor(red) lpattern(dash))

* Correlograma - ACF
ac ehat, lags(12) title("Función de Autocorrelación (ACF) de Residuos") ///
    name(acf_plot, replace)

* Correlograma - PACF
pac ehat, lags(12) title("Función de Autocorrelación Parcial (PACF) de Residuos") ///
    name(pacf_plot, replace)

* Histograma de residuos
histogram ehat, normal kdensity ///
    title("Distribución de Residuos") ///
    xtitle("Residuos") ytitle("Densidad") ///
    name(hist_resid, replace)

* Q-Q plot para normalidad
qnorm ehat, name(qq_plot, replace)

* ============================================
* PASO 8: TEST DE COINTEGRACIÓN (Residuos)
* ============================================

di _newline(2) "=== PASO 8: TEST DE COINTEGRACIÓN ===" _newline
di "Pruebas de raíz unitaria sobre los RESIDUOS del modelo" _newline

* Nota: Los residuos de una regresión cointegrada NO deben tener
* constante ni tendencia en las pruebas de raíz unitaria

di "Test Augmented Dickey-Fuller (ADF) - sin constante:"
dfuller ehat, noconstant lags(4)
scalar p_adf_resid = r(p)

di _newline "Test Phillips-Perron (PP) - sin constante:"
pperron ehat, noconstant lags(4)
scalar p_pp_resid = r(p)

di _newline "Test KPSS - sin constante:"
kpss ehat, notrend
scalar kpss_stat = r(stat)

* Interpretación de cointegración
di _newline "========================================"
di "INTERPRETACIÓN DE COINTEGRACIÓN:"
di "========================================" _newline

di "Hipótesis en ADF y PP:" _newline ///
"  H0: Los residuos tienen raíz unitaria (NO cointegración)" _newline ///
"  H1: Los residuos son estacionarios (SÍ cointegración)" _newline

if p_adf_resid < 0.05 {
    di "Test ADF: ✓ RECHAZA H0 → Evidencia de cointegración (p = " %6.4f p_adf_resid ")"
}
else {
    di "Test ADF: ✗ NO rechaza H0 → Sin evidencia de cointegración (p = " %6.4f p_adf_resid ")"
}

if p_pp_resid < 0.05 {
    di "Test PP:  ✓ RECHAZA H0 → Evidencia de cointegración (p = " %6.4f p_pp_resid ")"
}
else {
    di "Test PP:  ✗ NO rechaza H0 → Sin evidencia de cointegración (p = " %6.4f p_pp_resid ")"
}

di _newline "Hipótesis en KPSS:" _newline ///
"  H0: Los residuos son estacionarios (SÍ cointegración)" _newline ///
"  H1: Los residuos tienen raíz unitaria (NO cointegración)" _newline

di "Test KPSS: Estadístico = " %6.4f kpss_stat _newline

* Conclusión final
di _newline "========================================"
di "CONCLUSIÓN SOBRE COINTEGRACIÓN:"
di "========================================" _newline

if (p_adf_resid < 0.05 | p_pp_resid < 0.05) {
    di "✓ EXISTE COINTEGRACIÓN entre las variables." _newline ///
    "  Las series comparten una relación de equilibrio de largo plazo." _newline ///
    "  Los residuos son estacionarios I(0), aunque las variables" _newline ///
    "  individuales sean I(1)." _newline
}
else {
    di "✗ NO EXISTE evidencia suficiente de cointegración." _newline ///
    "  Las variables podrían no tener una relación estable" _newline ///
    "  de largo plazo." _newline
}

* Cerrar cualquier log abierto
capture log close

* Fin del script
