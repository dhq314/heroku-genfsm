$(document).ready(function() {
	$("#timestamp_btn").click(function() {
		var inp = $("#timestamp_inp").val();
		var url = "http://" + Domain + "/time/" + inp;
        $.getJSON(url, function(rs){
		    $("#timestamp_sta").html(rs.time);
        });
	});
    $("#ip_btn").click(function() {
		var inp = $("#ip_inp").val();
		var url = "http://" + Domain + "/ip/" + inp;
        $.getJSON(url, function(rs){
            var _html = "";
            if (rs.result == 1)
            {
                _html += "<span>";
                _html += "<p>IP: " + rs.ip + "</p>";
                if ( rs.country )
                {
                    _html += "<p>Country: " + rs.country + "</p>";
                }
                if ( rs.city )
                {
                    _html += "<p>City: " + rs.city + "</p>";
                }
                _html += "</span>";
            }
            else
            {
		        _html = "<p style='color: red;'>数据有误！</p>";
            }
            $("#ip_sta").html(_html);
        });
	});
    $("#phone_btn").click(function() {
		var phone = $.trim($("#phone_inp").val());
        if ( phone )
        {
            $.ajax({
                type : "get",
                async : false,
                url : "http://tcc.taobao.com/cc/json/mobile_tel_segment.htm?tel=" + phone,
                dataType : "jsonp",
                jsonp : "callback",
                data : {},
                success : function(rs) {
                    $("#phone_sta").html("<p>" + rs.catName + " " + rs.province + "</p>");
                },
                error : function() {
                    $("#phone_sta").html("<p style='color: red;'>获取数据失败！</p>");
                }
            });
        }
        else
        {
            $("#phone_sta").html("<p style='color: red;'>手机号码不能为空！</p>");
        }
	});
});
