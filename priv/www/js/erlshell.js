var ErlShell = {
    "action" : 1,                 //1 ErlShell没启动，2已启动
    "pid" : undefined,
    "timer" : undefined,
    "interval" : 10,
    "line_num" : 1,
    "process" : 0,
    "url" : "http://" + Domain + "/erlshellaction/",
};

ErlShell.create_es_line = function(line_num) {
    var _html = "";
    _html += '<table class="es_line">';
    _html += '    <tr>';
    _html += '        <td class="es_num">' + line_num + '></td>';
    _html += '        <td class="es_str">';
    _html += '            <div id="es_str_zone" contenteditable="true"></div>';
    _html += '        </td>';
    _html += '    </tr>';
    _html += '    <tr><td colspan="2" id="es_result_' + line_num + '"></td></tr>';
    _html += '</table>';
    $("#es_div").append(_html);
    $("#es_str_zone").focus();
};

ErlShell.bind_es_keypress = function() {
    $("#es_str_zone").bind("keypress", function(event) {
        var keycode = event.keyCode ? event.keyCode : event.which;
        if ( keycode == 13 )        //回车事件 
        {
            var erl_str = "", data = {};
            erl_str = $.trim($("#es_str_zone").text());
            //alert(erl_str);
            if ( erl_str )
            {
                data = { "action" : 3, "pid" : ErlShell.pid, "erl_str" : erl_str };
                $("#es_div").css({"background-color" : "#EDEDED"});
                $.post(ErlShell.url, data, function(rs) {
                    if ( parseInt(rs.action) == 3 )
                    {
                        $("#es_div").css({"background-color" : "#FFF"});
                        var es_result = "#es_result_" + ErlShell.line_num;
                        $(es_result).html(rs.value);
                        if ( rs.result == 1 )
                        {
                            ErlShell.reset_es_keypress();
                            ErlShell.line_num = rs.line_num;
                            ErlShell.create_es_line(ErlShell.line_num);
                            ErlShell.bind_es_keypress();
                            //alert(rs.value);
                        }
                        else if ( rs.result == 31 )         //进程异常关闭
                        {
                            ErlShell.erlshell_stop();
                            alert("进程异常已关闭，请重新启动 ErlShell！"); 
                        }
                    }
                }, "json");
            } 
            return false;
        }
        //alert(keycode);
    });
};

ErlShell.reset_es_keypress = function() {
    $('#es_str_zone').unbind('keypress');
    $('#es_str_zone').attr({"id" : "", "contenteditable" : "false"});
};

// ErlShell 的心跳包函数
ErlShell.erlshell_heart = function() {
    if ( ErlShell.action != 2 )
    {
        if ( ErlShell.timer )
        {
            clearTimeout(ErlShell.timer);
        }
        ErlShell.timer = undefined;
        return false;
    }
    var data = { "action" : 4, "pid" : ErlShell.pid};
    $.post(ErlShell.url, data, function(rs) {
        if ( rs.result == 41 )                      //进程异常关闭
        {
            ErlShell.erlshell_stop();
            alert("进程异常已关闭，请重新启动 ErlShell！"); 
        }
    }, "json");
};

// 关闭ErlShell
ErlShell.erlshell_stop = function() {
    if ( ErlShell.timer )
    {
        clearTimeout(ErlShell.timer);
    }
    ErlShell.timer = undefined;
    ErlShell.pid = undefined;
    ErlShell.action = 1;
    $("#erlshell_action").html("Start");
    $("#es_div").css({"background-color" : "#EDEDED"});
    ErlShell.reset_es_keypress();
}

$("#erlshell_action").click(function() {
    if ( ErlShell.process == 1 ) {
        return false;
    }
    ErlShell.process = 1;
    var data = {};
    if ( ErlShell.action == 1 )
    {
        data = { "action" : 1 };
    }
    else
    {
        data = { "action" : 2, "pid" : ErlShell.pid }
    }
    $.post(ErlShell.url, data, function(rs) {
        if ( rs.result == 1 )
        {
            switch ( parseInt(rs.action, 10) )
            {
                //启动ErlShell
                case 1:
                    ErlShell.pid = rs.pid;
                    ErlShell.interval = rs.interval;
                    ErlShell.line_num = rs.line_num;
                    ErlShell.action = 2;
                    ErlShell.process = 0,
                    $("#es_div").html("");
                    ErlShell.create_es_line(ErlShell.line_num);
                    ErlShell.bind_es_keypress();
                    $("#erlshell_action").html("Stop");
                    ErlShell.erlshell_heart();
                    $("#es_div").css({"background-color" : "#FFF"});
                    ErlShell.timer = setInterval(ErlShell.erlshell_heart, ErlShell.interval * 1000);
                    break;
                //关闭ErlShell
                case 2:
                    ErlShell.erlshell_stop();
                    break;
                default:
                    alert("ActionCode " + rs.action);
                    break;
            }
        }
        ErlShell.process = 0
    }, "json");
});

