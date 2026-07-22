#kph to meter per sekond
convert_kph_to_minutes_per_meter = function(x){
  
 return (60/(x*1000))
}
convert_kph_to_minutes_per_meter(0.0001)

minutes_per_meter = sapply(c(80,40,20,10,4.5,3,2,1.6),convert_kph_to_minutes_per_meter)
