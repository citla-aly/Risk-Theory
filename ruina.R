##
# Funciones auxiliares
##

# Recibe la funcion quantil de una distribucion y regresa una muestra
# que siga la misma
sample_from <- function (Finv)
	Finv(runif(1, 0, 1))

##
# Variables para simulacion del proceso poisson
##

# funcion de tasa de intensidad del proceso no homogeneo
lambda <- function(t, c)
	c*tanh(t)

# simula el tiempo de interarribo dado que el evento anterior sucedio
# al momento `t_ant`
simular_interarribo <- function(t_ant, c) {
	quantil <- function(p)
		acosh(cosh(t_ant)*(1-p)^(-1/c)) - t_ant
	sample_from(quantil)
}

# simula un pago aleatorio con promedio especificado
simular_pago <- function(media)
	media*rchisq(1, df=1)

# simula el proceso de poisson de la aseguradora por completo,
# por a lo mas `t_max` unidades de tiempo, empezando con `fondos` fondos
# pagando al rededor de `pago_promedio` por reclamo, recibiendo
# `ganancias` por unidad de tiempo continuas y teniendo `n_clientes` clientes
simular_proceso <- function(t_max, fondos, pago_promedio, ganancias,
	                        n_clientes) {

	n <- 1 # el numero de reclamos actuales
	Ts <- NULL # los tiempos de interarribo
	Ys <- NULL # los pagos de reclamos
	Xs <- NULL # las perdida netas del n-esimo reclamo
	Ss <- NULL # las perdidas cumulativas que ha recibido la empresa

	while (sum(Ts) < t_max && (length(Ss) == 0 || max(Ss) <= fondos)) {
		Ys[n] <- simular_pago(pago_promedio)
		Ts[n] <- simular_interarribo(sum(Ts), n_clientes)
		Xs[n] <- Ys[n] - ganancias*Ts[n]
		if (n == 1)
			Ss[n] <- Xs[n]
		else
			Ss[n] <- Xs[n] + Ss[n-1]
		n <- n + 1
	}

	list(Ys, Ts, Xs, Ss)
}

##
# Converger siumulacion
##

# repite el evento estocastico `simular` hasta converger en su
# valor por al menos `iter_no_change` iteraciones seguidas
converger <- function(simular, rtol = 0.001, atol = 0.0001,
	                  iter_no_change = 10) {
	it <- 0 # iteracion actual
	est <- 0.0 # estimado actual
	est_ant <- 0.0 # estimado anterior
	it_nochange <- 0 # iteraciones sin cambio
	while (it_nochange < iter_no_change) {
		actual <- simular()
		est_ant <- est
		est <- (est_ant*it + actual)/(it + 1)

		if (abs(est - est_ant) <= atol + rtol*abs(est_ant))
			it_nochange <- it_nochange + 1
		else
			it_nochange <- 0

		it <- it + 1
		print(paste("It:", it, "Estimacion:", est))
	}
	est
}

##
# Correr simulacion
##

# ejecuta el proceso estocastico e imprime y muestra resultados
main <- function() {
	clientes <- 10
	fondos <- 10
	pago_promedio <- 1
	ganancias <- 0.99*clientes
	t_max <- 100

	# estima probabilidad
	simular <- function() {
		res <- simular_proceso(t_max, fondos, pago_promedio, ganancias,
		                       clientes)
		max(res[[4]]) > fondos
	}
	prob <- converger(simular)
	print(paste("Probabilidad estimada:", prob))

	# muestra resultados de una ejecucion particular
	res <- simular_proceso(t_max, fondos, pago_promedio, ganancias, clientes)
	Ys <- res[[1]]
	Ts <- res[[2]]
	Xs <- res[[3]]
	Ss <- res[[4]]
	n <- length(Ys)

	# prepara plot
	tiempo = NULL
	for (i in 1:n)
		tiempo[i] <- sum(Ts[1:i])
	ingresos = numeric(n) + ganancias
	perdidas = Ys
	dinero = fondos - Ss
	print("Graficando una simulacion particular")
	plot(tiempo, dinero, type = "l", ylim = c(0, max(dinero)),
	     xlab = "tiempo", ylab = "dinero", col = "blue", pch = 1)
	points(tiempo, ingresos, type = "l", col = "green", pch = 2)
	points(tiempo, perdidas, type = "s", col = "red", pch = 3)
	legend("bottomleft", legend = c("dinero", "ganancias", "perdidas"),
	       col = c("blue", "green", "red"), pch = c(1,2,3))
	title("Resumen capital de aseguradora")
}

main()
