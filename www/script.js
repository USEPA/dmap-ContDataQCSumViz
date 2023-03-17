$(function(){ 
  $(document).on("shiny:busy", function() {
  	setTimeout(function() {
	      if ($('html').hasClass('shiny-busy')) {
	        $("#customBusy").show();
	        $(":input").prop("disabled", true);
	      }
	    }, 500) 
  });
  
  $(document).on("shiny:idle", function() {
    $(":input").prop("disabled", false);
     $("#customBusy").hide();
    });
    
  $(document).on("shiny:error", function(event) {
    $(":input").prop("disabled", false);
     $("#customBusy").hide();
   });


});


