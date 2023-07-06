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

# version of the lifetable that uses vector
# arguments; 
# 6 July
my_lifetable <- function(age,mx,ax, close_method = "constant"){
  
  # ax closeout, use 1/mx
  # that is, the mean of a constant hazard 
  n     = length(mx)
  
  if (close_method == "contstant"){
    ax[n] = 1/mx[n]
  }
  
  if (close_method == "my_nifty_method"){
    # then use that code instead
  }
  
  qx    = mx_to_qx(mx = mx, ax = ax)
  lx    = qx_to_lx(qx, radix = 1)
  dx    = lxqx_to_dx(lx,qx)
  Lx    = lxdx_to_Lx(lx,ax,dx)
  
  # Lx needs thoughtful closeout too
  Lx[n] = lx[n] * ax[n]
  
  Tx    = Lx_to_Tx(Lx)
  ex    = Txlx_to_ex(Tx,lx)
  
  # ex = ax in open interval
  ex[n] = ax[n]
  
  out <- tibble(age, 
                mx, 
                ax, 
                qx, 
                lx, 
                dx, 
                Lx, 
                Tx, 
                ex)
  return(out)
}

my_lifetable_tibble <- function(data_chunk){
  
  # create the 3 vectors needed by our first lifetable function
  age <- data_chunk |> pull(age)
  mx  <- data_chunk |> pull(mx)
  ax  <- data_chunk |> pull(ax)
  
  # now apply our first version of the function
  out <- my_lifetable(age, 
                      mx, 
                      ax )
  return(out)
}
