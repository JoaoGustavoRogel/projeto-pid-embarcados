# importação das bibliotecas necessárias
# install.packages("R.matlab", "control")
library(R.matlab)
library(control)

# leitura e processamento dos dados
dados <-  readMat("inputs/amostras_equipe7.mat")
tempo <- dados[[3]]
degrau <- dados[[1]]
resp <- dados[[2]]

# método dos mínimos quadrados
size <- length(tempo)
f <- cbind(resp[1, 1:size-1], degrau[1, 1:size-1])
j <- cbind(resp[1, 2:size])
m <- solve(t(f) %*% f) %*% t(f) %*% j
a1 <- m[[1]]
b1 <- m[[2]]
sys <- control::tf(b1, c(1, a1))

check_overshoot <- function(r, limite){
  limite_inf <- (limite * 0.9 + 1) * 1
  limite_sup <- (limite * 1.1 + 1) * 1
  overshoot <- max(r)
  if(overshoot >= limite_inf && overshoot <= limite_sup){
    return(TRUE)
  } 
  else{
    return(FALSE)
  }
}

check_time <- function(r, t, limite){
  
  limite_inf <- limite - 2
  limite_sup <- limite + 2
  y_inf <- 0.98
  y_sup <- 1.02
  size <- length(tempo)
  check_values <- c()
  
  for(i in 1:size){
    val <- r[i]
    if(val >= y_inf && val <= y_sup){
      check_values <- c(check_values, TRUE)
    }
    else{
      check_values <- c(check_values, FALSE)
    }
  }
  
  flag <- TRUE
  ret <- FALSE
  for(i in 1:size){
    time <- tempo[i]
    val <- check_values[i]
    if(flag && time >= limite_inf && time <= limite_sup){
      if(val && !check_values[i-1]){
        ret <- TRUE
        flag <- FALSE
      }
    }else if(!flag && !val){
      ret <- FALSE
    }
  }
  return(ret)
}

get_resp <- function(kp, ki, kd){
  ans <- c(0)
  ts <- 0.3
  sp <- 1
  pv <- 0
  acao_int <- 0
  erro_anterior <- sp - pv
  for(i in 2:size){
    erro <- sp - pv
    acao_prop <- kp * erro
    acao_int <- acao_int + ki * ts * erro
    acao_deriv <- ((erro - erro_anterior) / ts) * kd
    erro_anterior <- erro
    acao_controlador <- acao_prop + acao_int + acao_deriv
    pv <-  a1 * pv + b1 * acao_controlador
    ans <- c(ans, pv)
  }
  return(ans)
}

resp_malha_aberta <- control::step(sys)
resp_malha_fechada <- get_resp(1, 0, 0)

kp_f <- 0
ki_f <- 0
kd_f <- 0
found = FALSE
resp_malha_fechada_ganho <- NULL

# operações com ganhos
for(kp in seq(from = 1, to = 10, by = 1)){
  for(ki in seq(from = 0, to = 5, by = 0.01)){
    kd <- 10
    aux <- get_resp(kp, ki, kd)
    found <- (check_overshoot(aux, 0.1) && check_time(aux, tempo, 60))
    if(found){
      resp_malha_fechada_ganho <- aux
      kp_f <- kp
      ki_f <- ki
      kd_f <- kd
      break
    }
  }
  if(found){
    break
  }
}
