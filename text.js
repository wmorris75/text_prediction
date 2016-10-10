<script type="text/javascript">

 function get_id(clicked_id){
 	var input = document.getElementById("text"); 
 	var suggestion = document.getElementById(clicked_id); 
 	var completed_str = input + " " + suggestion; 
 	console.log(completed_str); 
 	document.getElementById("text").innerHTML= completed_str;
 } 
</script>


