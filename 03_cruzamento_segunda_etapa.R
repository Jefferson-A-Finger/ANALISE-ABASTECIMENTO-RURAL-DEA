# =====================================================================================
# SCRIPT DE CRUZAMENTO ENTRE AS ANÁLISES E SEGUNDA ETAPA
# 1) Cruzamento Modelo A × Modelo B: Spearman entre escores e concordância das eficientes
# 2) Segunda etapa: escores × condições físicas e × ocorrências fora do padrão de 2017
#    (cloro residual, E. coli, coliformes totais — Goulart, 2017, fonte SAAE)
# Entradas: Resultados_Modelo_A_R.xlsx (script 01_modelo_A.R), Resultados_Modelo_B_R.xlsx
# (script 02_modelo_B.R, se já houver) e Planilha_Estatistica_DEA_41.xlsx, aba "Segunda_etapa".
# =====================================================================================
 
library(readxl); library(writexl)
setwd(dirname(file.choose()))
 
A <- as.data.frame(read_excel("Resultados_Modelo_A_R.xlsx"))
A <- A[!is.na(A$escore), c("N", "DMU", "escore")]
 
## ---------------------------------------------------------------- 1. CRUZAMENTO A × B
if (file.exists("Resultados_Modelo_B_R.xlsx")) {
  B <- as.data.frame(read_excel("Resultados_Modelo_B_R.xlsx"))[, c("N", "escore")]
  names(B)[2] <- "escore_B"
  AB <- merge(A, B, by = "N")
  cat("linhas presentes nos dois modelos:", nrow(AB), "\n")
  print(cor.test(AB$escore, AB$escore_B, method = "spearman", exact = FALSE))
  AB$efic_A <- AB$escore   >= 0.999999
  AB$efic_B <- AB$escore_B >= 0.999999
  print(table(Modelo_A = ifelse(AB$efic_A, "eficiente", "ineficiente"),
              Modelo_B = ifelse(AB$efic_B, "eficiente", "ineficiente")))
  write_xlsx(AB, "Cruzamento_A_B.xlsx")
} else cat("Modelo B ainda sem resultados: cruzamento adiado.\n")
 
## ---------------------------------------------------------------- 2. SEGUNDA ETAPA
ctx <- as.data.frame(read_excel("Planilha_Estatistica_DEA_41.xlsx",
                                sheet = "Segunda_etapa", n_max = 40))[, c(1, 4:10)]
names(ctx) <- c("N", "rede_lig", "profundidade", "nivel_dinamico",
                "cloro_2017", "ecoli_2017", "colif_totais_2017", "total_2017")
d <- merge(A, ctx, by = "N")
seg <- do.call(rbind, lapply(names(ctx)[-1], function(v) {
  ok <- !is.na(d[[v]])
  t  <- cor.test(d$escore[ok], d[[v]][ok], method = "spearman", exact = FALSE)
  data.frame(variavel = v, n = sum(ok), rho = round(unname(t$estimate), 3),
             p = round(t$p.value, 3))
}))
print(seg, row.names = FALSE)
# Esperado (Modelo A): rede/lig −0,252 (p 0,127); profundidade +0,375 (p 0,022);
# nível dinâmico +0,160; cloro +0,194 (p 0,244); E. coli +0,083; coliformes +0,038;
# total +0,119 — só a profundidade tem p < 0,05, mas não ao nível corrigido de Bonferroni (0,05/7 ≈ 0,007): indício.
write_xlsx(seg, "Segunda_etapa_Modelo_A.xlsx")
 
## ---------------------------------------------------------------- 3. GRÁFICOS (Figuras 19 e 21 do TCC)
dir.create("figuras", showWarnings = FALSE)
ce <- "#B8860B"; ci <- "#2F6DB5"
f3 <- function(x) formatC(x, digits = 3, format = "f", decimal.mark = ",")
# Figura 19 — estilo da Figura 3: Y = escore, fronteira em escore = 1, tendência LOWESS
png("figuras/Fig19_segunda_etapa.png", width = 2000, height = 1000, res = 220)
par(mfrow = c(1, 2), mar = c(4, 4.5, 2.5, 1), bty = "l")
for (v in c("profundidade", "cloro_2017")) {
  s <- seg[seg$variavel == v, ]; ok <- !is.na(d[[v]])
  x <- d[[v]][ok]; y <- round(d$escore[ok], 6)
  plot(x, y, pch = 21, bg = ifelse(y >= 0.999999, ce, "grey20"), col = "white", ylim = c(0.25, 1.1),
       xlab = if (v == "profundidade") "X = profundidade do poço (m)" else
              "X = ocorrências de cloro (2017)", ylab = "Y = escore BCC (Modelo A)")
  abline(h = 1, lwd = 2); text(max(x), 1.04, "fronteira (escore = 1)", adj = 1, font = 3, cex = 0.7)
  lines(lowess(x, y, f = 0.6), col = ci, lwd = 2)
  i <- which.min(y); segments(x[i], y[i], x[i], 1, lty = 3, col = "grey40")
  mtext(paste0("ρ = ", f3(s$rho), "; p = ", f3(s$p), "; n = ", s$n), side = 3, cex = 0.75, adj = 0)
}
dev.off()
# Figura 21 — comparação A × B (modelo em branco enquanto o Modelo B não roda)
png("figuras/Fig21_comparacao_A_B.png", width = 1500, height = 1100, res = 220)
plot(NA, xlim = c(0, 1.05), ylim = c(0, 1.05), xlab = "Escore BCC – Modelo A (estrutural)",
     ylab = "Escore BCC – Modelo B (operacional)")
abline(v = 1, h = 1, col = "grey75"); abline(0, 1, lty = 2, col = "grey80")
if (exists("AB")) {
  points(AB$escore, AB$escore_B, pch = 16,
         col = ifelse(AB$efic_A & AB$efic_B, ce, ci))
  mtext(paste0("ρ = ", f3(cor(AB$escore, AB$escore_B, method = "spearman")),
               "; n = ", nrow(AB)), side = 3, cex = 0.75)
} else text(0.52, 0.55, "AGUARDANDO RODAR O MODELO B", col = ce, font = 2)
dev.off()
