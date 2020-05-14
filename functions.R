update_price <- function(tab, imbu, imbu_stage, new_price){
  tab[name == imbu & stage  == imbu_stage, price := new_price]
  
}

