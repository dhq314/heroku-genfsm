$(document).ready(function() {
	$("#timestamp_btn").click(function() {
		var inp = $("#timestamp_inp").val();
		var url = "http://" + Domain + "/time/" + inp;
        $.getJSON(url, function(rs){
		    $("#timestamp_sta").html(rs.time);
        });
	});
});
