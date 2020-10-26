check_overshoot <- function(r, limite, size){
  limite_inf <- (limite * 0.9 + 1) * 50
  limite_sup <- (limite * 1.1 + 1) * 50
  overshoot <- max(r)
  if(overshoot >= limite_inf && overshoot <= limite_sup){
    return(TRUE)
  } 
  else{
    return(FALSE)
  }
}

check_time <- function(r, t, limite, size, tempo){
  
  limite_inf <- limite - 2
  limite_sup <- limite + 2
  y_inf <- 0.98 * 50
  y_sup <- 1.02 * 50
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

get_resp <- function(kp, ki, kd, ts, size, a1, b1){
  ans <- c(0)
  sp <- 50
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