mx_to_qx <- function(mx, ax){
  qx <- mx / (1 + (1 - ax) * mx)
  qx[length(qx)] <- 1
  return(qx)
}

qx_to_lx <- function(qx, radix = 1){
  lx <- cumprod(1 - qx)
  lx <- c(1,lx)
  lx <- lx[1:length(qx)]
  lx <- radix * lx
  return(lx)
}

lxqx_to_dx <- function(lx,qx){
  lx * qx
}

lxdx_to_Lx <- function(lx,ax,dx){
  Lx <- lx - (1- ax) * dx
  return(Lx)
}

Lx_to_Tx <- function(Lx){
  Tx <-
    # start with the Lx by age
    Lx |> 
    # turn it backwards
    rev() |> 
    # then take the cumulative sum
    cumsum() |> 
    # and put it back in the original age order
    rev()
  return(Tx)
}

Txlx_to_ex <- function(Tx, lx){
  ex <- Tx / lx
  return(ex)
}