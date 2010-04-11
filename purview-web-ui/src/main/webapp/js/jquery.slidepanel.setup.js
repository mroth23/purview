jQuery(document).ready(function() {
	$("#slideit").click(function() {
		$("div#slidepanel").slideDown("slow");
	});

	$("#closeit").click(function() {
		$("div#slidepanel").slideUp("slow");	
	});

	$("#toggle a").click(function() {
		$("#toggle a").toggle();
	});
});