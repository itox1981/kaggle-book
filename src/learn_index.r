#### 評価指標を見てみる ----
library(ggplot2)
library(dplyr)
library(purrr)
library(gridExtra)

### * 回帰の指標 ----
x = c(1,3,4,10,100)
y = 0:1000/10

result = expand.grid(x=x, y=y) %>%
  dplyr::group_by(y) %>%
  dplyr::summarise(
    rmse = sqrt(mean((x-y)^2)),
    rmlse_x_10 = sqrt(mean( ( log(1+x) - log(1+y) )^2 ) ) * 10,
    mae = mean(abs(x-y))
  ) %>%
  dplyr::ungroup() %>%
  tidyr::gather(key = type, value = val, rmse:mae)

result %>%
  ggplot(aes(x=y, y=val, color=type)) +
  geom_line(size=2) +
  theme(legend.position = "bottom") +
  xlim(0,50) + ylim(0,50)

result %>%
  dplyr::group_by(type) %>%
  dplyr::slice(which.min(val))


### *2値分類の指標 ----
# 比率
num_p_ <- c(1)
num_n_ <- c(1,9)
beta_  <- c(1,3,5)

# expand.grid(num_p = num_p_, num_n = num_n_, beta = beta_) %>%
#   pmap( function(num_p, num_n, beta) {
expand.grid(beta = beta_, num_p = num_p_, num_n = num_n_) %>%
  pmap( function(beta, num_p, num_n) {
        #  tpr, tnr
    x_range <- 0:100/100
    y_range <- 0:100/100

    #  tnr, tpr
    dat <- expand.grid(tpr = x_range , tnr = y_range ) %>%
      dplyr::mutate(
        Fvalue = ( 1 + beta^2 ) * ( num_p * tpr) / ( num_p * ( beta^2 + tpr) + num_n * ( 1 - tnr) )
      )

    # precision recall
    # dat <- expand.grid(precision = x_range , recall = y_range ) %>%
    #   dplyr::mutate( 
    #     precision = ifelse( precision == 0, 0.01, precision),
    #     recall    = ifelse( recall    == 0, 0.01, recall)
    #     ) %>%
    #   dplyr::mutate(
    #     Fvalue = ( 1 + beta^2 ) / ( beta^2 / recall + 1 / precision)
    #   ) 
    
    dat %>%
      ggplot(aes( y=tpr, x=tnr, fill=Fvalue)) +
      # ggplot(aes( x=precision, y = recall, fill=Fvalue)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint=0.5) +
      labs( title = paste0("Fvalue　 β:", beta, "　　P:N=", num_p, ":", num_n)) +
      theme( legend.position = "none")
    
  }) %>%
  grid.arrange(grobs = ., ncol = 3) 
