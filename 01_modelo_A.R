# =====================================================================================
# SCRIPT DA PRIMEIRA ANÁLISE (MODELO A, ESTRUTURAL)
# DEA-BCC orientado ao produto — 41 sistemas rurais de Marechal Cândido Rondon – PR
# Jefferson Antônio Finger — TCC2 — Engenharia Civil — UTFPR Toledo
#
# Insumos:  X1 = extensão de rede (m)           X2 = nº de fontes de captação
# Produtos: Y1 = volume anual outorgado (m³/ano) Y2 = ligações ativas
#
# São 40 DMUs: a Linha Três Voltas não entra (rede e ligações cadastradas com a Maracanã).
# A fronteira é formada só pelas 38 linhas de dado observado
# (coluna Ref_fronteira = "Sim"); as 2 linhas com valores estimados (Cinco Cantos
# e Gaúcha) são avaliadas contra essa fronteira, sem compor a referência.
# Procedimento conforme Stulp (2025): pacote deaR, model_basic(), orientation = "oo",
# rts = "vrs"; escore = 1/φ.
# Entrada: Planilha_Estatistica_DEA_41.xlsx, aba "Matriz_A_41".
# =====================================================================================
 
# install.packages(c("deaR", "readxl", "writexl"))   # só na primeira vez
library(deaR); library(readxl); library(writexl)
 
setwd(dirname(file.choose()))   # escolha a planilha na janela que abrir
 
## ---------------------------------------------------------------- 1. LEITURA
m <- read_excel("Planilha_Estatistica_DEA_41.xlsx", sheet = "Matriz_A_41")
m <- as.data.frame(m)
m <- m[!grepl("^Tr.s Voltas", m[[2]]), ]     # fora do Modelo A: contada na linha Maracanã / Três Voltas
nrow(m)                                       # esperado: 40
dados <- data.frame(DMU = m[[2]],          # coluna 2: nome do sistema
                    X1_rede = m$X1_rede_m, X2_fontes = m$X2_fontes,
                    Y1_volume = m$Y1_volume_m3_ano, Y2_ligacoes = m$Y2_ligacoes)
stopifnot(!any(is.na(dados)), all(dados[, 2:5] > 0))
ref <- which(m$Ref_fronteira == "Sim")        # 38 linhas observadas
est <- which(m$Ref_fronteira != "Sim")        # 2 linhas estimadas
length(ref); length(est)                      # esperado: 38 e 2
 
dea <- make_deadata(dados, dmus = 1, inputs = 2:3, outputs = 4:5)
 
## ---------------------------------------------------------------- 2. MODELO PRINCIPAL
# 2a) linhas observadas: fronteira e avaliação com as 38
res_obs <- model_basic(dea, dmu_ref = ref, dmu_eval = ref,
                       orientation = "oo", rts = "vrs")
phi <- rep(NA_real_, nrow(dados)); phi[ref] <- efficiencies(res_obs)
 
# 2b) linhas estimadas: avaliadas uma a uma contra a fronteira das observadas
for (k in est) {
  phi[k] <- tryCatch({
    r <- model_basic(dea, dmu_ref = ref, dmu_eval = k, orientation = "oo", rts = "vrs")
    as.numeric(efficiencies(r))
  }, error = function(e) NA_real_)
}
phi[!is.finite(phi)] <- NA                    # sem solução = fora do conjunto de produção
escore <- round(1 / phi, 6)                   # arredondar evita falsos desempates em 1
 
## ---------------------------------------------------------------- 3. ESCALA (CCR)
res_crs <- model_basic(dea, dmu_ref = ref, dmu_eval = ref, orientation = "oo", rts = "crs")
ccr <- rep(NA_real_, nrow(dados)); ccr[ref] <- 1 / efficiencies(res_crs)
for (k in est) ccr[k] <- tryCatch(1 / as.numeric(efficiencies(
  model_basic(dea, dmu_ref = ref, dmu_eval = k, orientation = "oo", rts = "crs"))),
  error = function(e) NA_real_)
escala <- ccr / escore
 
## ---------------------------------------------------------------- 4. SAÍDAS
saida <- data.frame(N = m[[1]], DMU = dados$DMU, Ref_fronteira = m$Ref_fronteira,
                    phi = round(phi, 6), escore = escore,
                    situacao = ifelse(is.na(escore), "Não avaliável",
                               ifelse(escore >= 0.999999, "Eficiente", "Ineficiente")),
                    ccr = round(ccr, 6), escala = round(escala, 6))
saida <- saida[order(-saida$escore, na.last = TRUE), ]
print(saida, row.names = FALSE)
 
# Conferência com a planilha (aba Resultados_A), resolvida também em Python e R/lpSolve:
#   avaliadas 39 | eficientes 9 | média 0,7834 | mediana 0,8382 | mínimo 0,3597 (Passo Fundo)
#   Cinco Cantos: não avaliável (rede menor que a de qualquer linha de referência)
e <- saida$escore[!is.na(saida$escore)]
cat("avaliadas:", length(e), "| eficientes:", sum(e >= 0.999999),
    "| média:", round(mean(e), 4), "| mediana:", round(median(e), 4),
    "| mínimo:", round(min(e), 4), "\n")
 
lambdas(res_obs)      # pesos das referências (linhas observadas)
references(res_obs)   # benchmarks de cada DMU ineficiente
targets(res_obs)      # alvos de insumo e de produto
slacks(res_obs)       # folgas
# Supereficiência (Andersen; Petersen, 1993): cada unidade eficiente é retirada da própria fronteira
efic <- ref[abs(phi[ref] - 1) < 1e-6]
sup  <- model_supereff(dea, dmu_eval = efic, dmu_ref = ref, orientation = "oo", rts = "vrs")
round(1 / efficiencies(sup), 3)   # > 1: supereficiente; NA ou Inf: sem solução (posição extrema)
 
write_xlsx(saida, "Resultados_Modelo_A_R.xlsx")
 
## ---------------------------------------------------------------- 5. SENSIBILIDADE (40 com estimadas na fronteira; 36 homogêneas)
bcc <- function(sel) {
  r <- model_basic(dea, dmu_ref = sel, dmu_eval = sel, orientation = "oo", rts = "vrs")
  s <- round(1 / efficiencies(r), 6)
  c(n = length(sel), eficientes = sum(s >= 0.999999), media = round(mean(s), 4),
    mediana = round(median(s), 4), minimo = round(min(s), 4))
}
todas  <- 1:nrow(dados)                                      # estimadas compondo a fronteira
homog  <- setdiff(ref, which(grepl("Capta", m[[3]]) | m$Operador == "SAAE"))  # sem captações e sem a Hermann (Ludwig = "Poço" na planilha)
rbind(`40 c/ estimadas na fronteira` = bcc(todas),
      `36 homogêneas`                = bcc(homog))
# Esperado (Tabela 5): 40 → 7 eficientes, média 0,7644 | 36 → 8, média 0,7860
 
## ---------------------------------------------------------------- 6. CINCO CANTOS: O "FALSO ESCORE"
## Só para documentar (seção 4.3 e Tabela 6 do TCC): NÃO altera o modelo principal.
## Com as estimadas compondo a fronteira, a Cinco Cantos só pode ser comparada consigo
## mesma (rede de 3.765 m, menor que a de qualquer linha observada) e recebe escore 1.
r_todas <- model_basic(dea, dmu_ref = todas, dmu_eval = todas, orientation = "oo", rts = "vrs")
esc_todas <- round(1 / efficiencies(r_todas), 6)
cc <- grep("Cinco Cantos", dados$DMU)
cat("Cinco Cantos com as estimadas na fronteira: escore", esc_todas[cc], "(falso positivo)\n")
cat("Menor rede entre as observadas:", min(dados$X1_rede[ref]), "m | rede da Cinco Cantos:",
    dados$X1_rede[cc], "m\n")
efeito <- data.frame(DMU = dados$DMU, principal = escore, com_estimadas = esc_todas)
efeito$diferenca <- round(efeito$com_estimadas - efeito$principal, 6)
efeito <- subset(efeito, is.na(principal) | abs(diferenca) > 1e-6)
print(efeito[order(efeito$diferenca), ], row.names = FALSE)
# Esperado: Cinco Cantos 1 | 13 linhas com escore menor | Horizontina 1 -> 0,716;
#           Heidrich 1 -> 0,857; Concórdia 1 -> 0,979; média das demais 39: 0,783 -> 0,758
 
 
## ---------------------------------------------------------------- 7. GRÁFICOS (Figuras 16, 17, 18 e 20 do TCC)
## Observação: na Figura 16 do TCC, a Linha Cinco Cantos (sem escore) aparece apenas como marcação, sem barra, e as linhas com dados estimados estão hachuradas; esse acabamento foi dado na edição final da figura.
## 7.1 extração dos resultados do deaR para objetos simples
n_dmu <- nrow(dados)
alvo <- matrix(NA_real_, n_dmu, 2)            # colunas: Y1 volume, Y2 ligações
L    <- matrix(0, n_dmu, n_dmu)                # pesos λ: linha = avaliada, coluna = referência
alvo[ref, ] <- as.matrix(targets(res_obs)$target_output)
L[ref, ref] <- as.matrix(lambdas(res_obs))
for (k in est) {
  r <- tryCatch(model_basic(dea, dmu_ref = ref, dmu_eval = k, orientation = "oo",
                            rts = "vrs"), error = function(e) NULL)
  if (!is.null(r)) { alvo[k, ] <- as.numeric(targets(r)$target_output)
                     L[k, ref] <- as.numeric(lambdas(r)) }
}
esc_de <- function(sel) {                      # escores de uma especificação (NA fora dela)
  r <- model_basic(dea, dmu_ref = sel, dmu_eval = sel, orientation = "oo", rts = "vrs")
  v <- rep(NA_real_, n_dmu); v[sel] <- round(1 / efficiencies(r), 6); v
}
e36 <- esc_de(homog); e40 <- esc_todas
graficos_modelo_A <- function(nome, escore, alvo, L, X1, X2, Y1, Y2, ref, est, e36, e40,
                              pasta = "figuras") {
  library(lpSolve)                             # usado só para traçar a fronteira da Figura 17
  dir.create(pasta, showWarnings = FALSE)
  ce <- "#B8860B"; ci <- "#2F6DB5"; nv <- "#1F3864"
  nm <- sub("Linha ", "", sub(" \\(Sistema.*\\)", "", nome)); nm[est] <- paste0(nm[est], "*")
  f3 <- function(x) formatC(x, digits = 3, format = "f", decimal.mark = ",")
  ok <- !is.na(escore); ef <- ok & escore >= 0.999999
  # Figura 16 — escores BCC ordenados
  o <- order(escore[ok]); id <- which(ok)[o]
  png(file.path(pasta, "Fig16_escores_BCC.png"), width = 1500, height = 2100, res = 220)
  par(mar = c(4, 10, 1, 1))
  b <- barplot(escore[id], horiz = TRUE, names.arg = nm[id], las = 1, cex.names = 0.55,
               col = ifelse(ef[id], ce, ci), border = NA, xlim = c(0, 1.12),
               xlab = "Escore de eficiência BCC (1/φ)")
  text(escore[id] + 0.01, b, f3(escore[id]), cex = 0.45, adj = 0)
  abline(v = mean(escore[ok]), lty = 2, col = nv)
  legend("bottomright", c("Eficiente", "Ineficiente"), fill = c(ce, ci), border = NA, cex = 0.6)
  dev.off()
  # Figura 17 — fronteira BCC no espaço dos produtos (estilo da Figura 3), para linhas escolhidas
  # Fixados os insumos x_o de uma linha, a fronteira é obtida maximizando t na direção d(θ):
  #   max t  s.a.  Σλ_j y_j ≥ t·d(θ),  Σλ_j x_j ≤ x_o,  Σλ_j = 1,  λ ≥ 0  (j = linhas observadas)
  fronteira_produtos <- function(xo, Xr, Yr, ns = 200) {
    s1 <- max(Yr[, 1]); s2 <- max(Yr[, 2]); nr <- nrow(Xr)
    t(sapply(seq(0, pi / 2, length.out = ns), function(th) {
      d <- c(sin(th) * s1, cos(th) * s2)
      A <- rbind(cbind(t(Yr), -d), cbind(t(Xr), 0), c(rep(1, nr), 0))
      r <- lp("max", c(rep(0, nr), 1), A, c(">=", ">=", "<=", "<=", "="), c(0, 0, xo, 1))
      r$solution[nr + 1] * d[2:1]                       # (ligações, volume)
    }))
  }
  Xr <- cbind(X1, X2)[ref, ]; Yr <- cbind(Y1, Y2)[ref, ]
  casos <- match(c("Linha Passo Fundo", "Linha Campo Sales", "Linha Perdigão", "Linha Guarani"), nome)
  png(file.path(pasta, "Fig17_fronteira_produtos.png"), width = 2000, height = 1900, res = 220)
  par(mfrow = c(2, 2), mar = c(4.2, 4.5, 2.6, 1), bty = "l")
  for (k in casos) {
    fr <- fronteira_produtos(c(X1[k], X2[k]), Xr, Yr)
    A <- c(Y2[k], Y1[k]); phi_k <- 1 / escore[k]; B <- A * phi_k; Bl <- c(alvo[k, 2], alvo[k, 1])
    plot(NA, xlim = c(0, max(fr[, 1], B[1]) * 1.2), ylim = c(0, max(fr[, 2], B[2]) * 1.2),
         xlab = "Y2 = ligações", ylab = "Y1 = volume (m³/ano)",
         main = paste0(nm[k], " — escore ", f3(escore[k])), cex.main = 0.8, font.main = 1)
    polygon(c(0, fr[, 1], 0), c(0, fr[, 2], fr[nrow(fr), 2]), col = "#EEF2F8", border = NA)
    lines(fr[, 1], fr[, 2], lwd = 2); segments(0, fr[nrow(fr), 2], fr[nrow(fr), 1], fr[nrow(fr), 2], lwd = 2)
    segments(fr[1, 1], 0, fr[1, 1], fr[1, 2], lwd = 2)
    if (escore[k] < 0.999999) {
      segments(0, 0, B[1], B[2], lty = 3, col = "grey40")
      arrows(A[1], A[2], B[1], B[2], length = 0.07, col = ci, lwd = 1.5)
      points(B[1], B[2], pch = 21, bg = ce, cex = 1.3); text(B[1], B[2], "B", pos = 2, font = 2)
      if (any(abs(Bl - B) > c(0.5, 50))) { arrows(B[1], B[2], Bl[1], Bl[2], length = 0.06, col = ce, lty = 2)
        points(Bl[1], Bl[2], pch = 21, bg = "white", col = ce, cex = 1.2); text(Bl[1], Bl[2], "B'", pos = 4, col = ce, font = 2) }
    }
    points(A[1], A[2], pch = 21, bg = if (escore[k] >= 0.999999) ce else ci, cex = 1.3)
    text(A[1], A[2], if (escore[k] >= 0.999999) "A = B" else "A", pos = 4, font = 2)
  }
  dev.off()
  # Figura 18 — rede de referências (benchmarks) com espessura proporcional a λ
  E <- which(ef); I <- which(ok & !ef)
  fr <- colSums(L[I, , drop = FALSE] > 1e-6)
  E <- E[order(-fr[E])]; I <- I[order(-escore[I])]
  yE <- setNames(seq(1, length(I), length.out = length(E)), E); yI <- setNames(seq_along(I), I)
  png(file.path(pasta, "Fig18_rede_benchmarks.png"), width = 1700, height = 2000, res = 220)
  par(mar = c(1, 1, 2, 1))
  plot(NA, xlim = c(-0.9, 1.9), ylim = c(length(I) + 1, 0), axes = FALSE, xlab = "", ylab = "")
  pal <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
  cE <- setNames(c(pal, rep("#8C8C8C", 20))[seq_along(E)], E)   # uma cor por referência (9ª em cinza)
  for (a in I) for (e in E) if (L[a, e] > 1e-6)
    segments(0, yE[as.character(e)], 1, yI[as.character(a)], lwd = 0.5 + 3.4 * L[a, e],
             col = adjustcolor(cE[as.character(e)], 0.85))
  points(rep(0, length(E)), yE, pch = 16, col = cE, cex = 0.8 + fr[E] / 6)
  principal <- E[apply(L[I, E, drop = FALSE], 1, which.max)]   # referência de maior λ
  points(rep(1, length(I)), yI, pch = 16, col = cE[as.character(principal)], cex = 0.7)
  text(-0.06, yE, paste0(nm[E], " (", fr[E], ")"), adj = 1, cex = 0.55)
  text(1.05, yI, paste0(nm[I], " – ", f3(escore[I])), adj = 0, cex = 0.5)
  mtext("Eficientes (nº de linhas)        Ineficientes (escore)", side = 3, cex = 0.7)
  dev.off()
  # Figura 20 — robustez: principal × especificações alternativas
  png(file.path(pasta, "Fig20_robustez.png"), width = 2000, height = 1000, res = 220)
  par(mfrow = c(1, 2), mar = c(4, 4, 2.5, 1))
  for (k in 1:2) {
    alt <- if (k == 1) e36 else e40
    mud <- abs(alt - escore) > 1e-6
    plot(escore, alt, pch = 16, col = ifelse(mud %in% TRUE, ce, ci), xlim = c(0.3, 1), ylim = c(0.3, 1),
         xlab = "Escore – principal", ylab = "Escore – alternativa",
         main = if (k == 1) "Amostra homogênea (36)" else "Estimadas na fronteira (40)", cex.main = 0.8)
    abline(0, 1, lty = 2, col = "grey60")
    ok2 <- !is.na(escore) & !is.na(alt)
    mtext(paste0("ρ = ", formatC(cor(escore[ok2], alt[ok2], method = "spearman"), 3,
                 format = "f", decimal.mark = ",")), side = 3, line = 0.1, cex = 0.7)
  }
  dev.off()
  cat("Figuras salvas na pasta:", normalizePath(pasta), "\n")
}
graficos_modelo_A(dados$DMU, escore, alvo, L, dados$X1_rede, dados$X2_fontes, dados$Y1_volume,
                  dados$Y2_ligacoes, ref, est, e36, e40)
# Observação: com dois insumos e dois produtos, a fronteira é uma superfície em quatro
# dimensões; a Figura 17 traça a fronteira no plano dos produtos, fixados os insumos de cada linha.
# deaR também oferece gráficos próprios: plot(res_obs).
