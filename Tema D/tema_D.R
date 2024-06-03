# D1) ##########################################

data = read.csv("probabilitati.csv", header = TRUE)
punctaje = data$probabilitati

n = length(punctaje)  
dispersia = 92.16     
nivel_incredere = c(0.95, 0.99)  

margini_eroare = qnorm(1 - (1 - nivel_incredere) / 2) * sqrt(dispersia / n)

intervale_incredere = sapply(margini_eroare, function(margin) {
  medie = mean(punctaje)
  c(medie - margin, medie + margin)
})

for (i in seq_along(nivel_incredere)) {
  cat(sprintf("Intervalul de incredere de %.0f%% pentru punctajul mediu este: [%.2f, %.2f]\n", 
              nivel_incredere[i] * 100, 
              intervale_incredere[1, i], 
              intervale_incredere[2, i]))
}

# D2) ##########################################

date_statistica <- read.csv("statistica.csv", skip = 1)
punctaje_statistica <- date_statistica$statistica
n <- length(punctaje_statistica)

media <- mean(punctaje_statistica)

deviatie_std <- sd(punctaje_statistica)
nivel_incredere_95 <- 0.95
nivel_incredere_99 <- 0.99

quantile_t <- qt(c(nivel_incredere_95 + (1 - nivel_incredere_95) / 2, 
                   nivel_incredere_99 + (1 - nivel_incredere_99) / 2), df = n - 1)

marja_eroare_95 <- quantile_t[1] * deviatie_std / sqrt(n)
marja_eroare_99 <- quantile_t[2] * deviatie_std / sqrt(n)

limita_inferioara_95 <- media - marja_eroare_95
limita_superioara_95 <- media + marja_eroare_95

limita_inferioara_99 <- media - marja_eroare_99
limita_superioara_99 <- media + marja_eroare_99

cat("Interval de incredere 95% pentru punctajul mediu la Statistica:", 
    round(limita_inferioara_95, 2), "-", round(limita_superioara_95, 2), "\n")

cat("Interval de incredere 99% pentru punctajul mediu la Statistica:", 
    round(limita_inferioara_99, 2), "-", round(limita_superioara_99, 2), "\n")

# D3) ##########################################

n = 100
x = 14  

alpha = c(0.01, 0.05)

test1 = binom.test(x, n, p = 0.85, conf.level = 1 - alpha[1])
test2 = binom.test(x, n, p = 0.85, conf.level = 1 - alpha[2])

cat("Intervalul de incredere de 95% pentru primul test:", test1$conf.int, "\n")
cat("Intervalul de incredere de 95% pentru al doilea test:", test2$conf.int, "\n")


