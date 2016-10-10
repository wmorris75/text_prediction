


 function get_id(clicked_id)
 {
 	var input = document.getElementById("text").value; 
 	var suggestion = document.getElementById(clicked_id).innerHTML; 
 	var completed_str = input + " " + suggestion; 
 	// console.log(suggestion); 
 	document.getElementById("text").value  = completed_str;
 	
 } 

