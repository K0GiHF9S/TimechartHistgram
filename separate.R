separate <- function(x){
  uniques <- order(unique(x$kind))
  
  output <- lapply(uniques,
                   try(
                     function(k){
                       act <- c(NULL)
                       cycle <- c(NULL)
                       first = TRUE
                       act_tmp = 0
                       cycle_tmp = 0
                       for(i in 1:(length(x$time)-1)){
                         if(x$kind[i] == k){
                           if(flag[i] == 0){
                             if(!first){
                               cycle <- c(cycle, x$time[i] - cycle_tmp)
                             }
                             cycle_tmp = x$time[i]
                             act_tmp = - x$time[i] + x$time[i+1]
                             first = FALSE
                           }else{
                             if(!first){
                               if(x$kind[i-1] != k){
                                 act_tmp = act_tmp - x$time[i-1] + x$time[i]
                               }
                               act <- c(act, act_tmp)
                             }
                           }
                         }
                       }
                       return(list(kind=k, act=act, cycle=cycle))
                     }
                   )
  )
  
  return(output)
}