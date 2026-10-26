

rule_based_acled_attribution <- function(inconclusive_territory){
  
  for(row_idx in 1:nrow(inconclusive_territory)){
    instance = inconclusive_territory[row_idx,]
    max_date = instance$event_date
    
    past_instance = gdf_territory%>%filter((event_date<max_date) & (geometry ==instance$geometry))%>%
      slice_max(order_by = event_date, with_ties =FALSE)
    
    if (nrow(past_instance)==0){
      
      inconclusive_territory[row_idx,]$sub_event ="M23 overtakes territory"
      
    }else if (past_instance$sub_event =="Government regains territory"){# territory war before under gov controle
      
      inconclusive_territory[row_idx,]$sub_event ="M23 overtakes territory"
      
    } else if (past_instance$sub_event =="M23 overtakes territory"){
      
      inconclusive_territory[row_idx,]$sub_event ="not M23 overtakes territory"
      
    }
    else {
      
      past_instance = inconclusive_territory%>%filter((event_date<max_date) & (geometry ==instance$geometry))%>%
        slice_max(order_by = event_date, with_ties =FALSE)
      
      if (past_instance$sub_event =="Government regains territory"| past_instance$sub_event =="not M23 overtakes territory"){# territory war before under gov controle
        
        inconclusive_territory[row_idx,]$sub_event ="M23 overtakes territory"
        
      } else if (past_instance$sub_event =="M23 overtakes territory"){
        
        inconclusive_territory[row_idx,]$sub_event ="not M23 overtakes territory"}
      
      else{
        warning(paste0(row_idx,": we need a double loop, two times after each other peaceful transition"))
        
      }
      
      
    }

  }
  inconclusive_territory = st_drop_geometry(inconclusive_territory[,c("id","notes","sub_event")])
  
  return(inconclusive_territory)
  
  
}

