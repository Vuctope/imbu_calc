update_price <- function(tab, imbu, imbu_item, new_price){
  tab[name == imbu & item == item, price := new_price]
  
}

