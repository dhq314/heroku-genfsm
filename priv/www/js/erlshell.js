var ErlShell = {
    "status" : 1,                                       //ErlShell状态，1表示没启动，2已启动
    "pid" : undefined,                                  //ErlShell的进程标识     
    "timer" : undefined,                                //心跳包定时
    "interval" : 10,                                    //心跳包定时间隔
    "line_num" : 1,                                     //ErlShell的行数
    "process" : 0,                                      //0标识当前没请求要处理，1反之
    "url" : "http://" + Domain + "/erlshellaction/",    //POST请求的地址
    "command_history" : [],                            	//使用过的历史命令
	"command_cursor" : 0								//命令光标位置
};

//创建命令行
ErlShell.create_es_command_line = function(line_num) {
    var _html = "";
    _html += '<table class="es_line">';
    _html += '    <tr>';
    _html += '        <td class="es_num">' + line_num + '></td>';
    _html += '        <td class="es_str">';
    _html += '            <div id="es_command_line" contenteditable="true"></div>';
    _html += '        </td>';
    _html += '    </tr>';
    _html += '    <tr><td colspan="2" id="es_result"></td></tr>';
    _html += '</table>';
    $("#es_div").append(_html);
    $("#es_command_line").focus();
};

//绑定命令行事件
ErlShell.bind_es_command_line_keypress = function() {
    $("#es_command_line").bind("keydown", function(event) {
        var keycode = event.keyCode ? event.keyCode : event.which;
        switch ( keycode )
        {
            //回车键
            case 13:
                var erl_str = "", data = {};
                // 获取命令行里的 erlang 表达式字符串
                erl_str = $.trim($("#es_command_line").text());
                if ( erl_str )
                {
                    data = { "action" : 3, "pid" : ErlShell.pid, "erl_str" : erl_str };
                    $("#es_div").css({"background-color" : "#EDEDED"});
                    $.post(ErlShell.url, data, function(rs) {
                        if ( parseInt(rs.action) == 3 )
                        {
                            $("#es_div").css({"background-color" : "#FFF"});
                            $("#es_result").html(rs.value);
                            if ( rs.result == 1 )
                            {
                                ErlShell.reset_es_keypress();
                                ErlShell.line_num = rs.line_num;
                                ErlShell.create_es_command_line(ErlShell.line_num);
                                ErlShell.bind_es_command_line_keypress();
                            }
                            else if ( rs.result == 31 )         //进程异常关闭
                            {
                                ErlShell.erlshell_stop();
                                alert("进程异常已关闭，请重新启动 ErlShell！"); 
                            }
                        }
                        //alert(erl_str);
                        ErlShell.command_history.unshift(erl_str);
                        ErlShell.command_cursor = 0;
                    }, "json");
                } 
                else            //空行按回车键光标跳到下一行
                {
                    ErlShell.reset_es_keypress();
                    ErlShell.create_es_command_line(ErlShell.line_num);
                    ErlShell.bind_es_command_line_keypress();
                }
                return false;
                break;
            //上方向键
            case 38:
                if ( ErlShell.command_history.length > 0 )
                {
                    if ( ErlShell.command_cursor >= ErlShell.command_history.length )
                    {
                        ErlShell.command_cursor = ErlShell.command_history.length - 1; 
                    }
                    $("#es_command_line").html(ErlShell.command_history[ErlShell.command_cursor]);
                    ErlShell.command_cursor++;
                }
                break;
            //下方向键
            case 40:
                if ( ErlShell.command_history.length > 0 )
                {
                    if ( ErlShell.command_cursor <= 0 )
                    {
                        ErlShell.command_cursor = 0; 
                        $("#es_command_line").html("");
                    }
                    else
                    {
                        ErlShell.command_cursor--;
                        $("#es_command_line").html(ErlShell.command_history[ErlShell.command_cursor]);
                    }
                }
                break;
            default:
                break;
        }
    });
};

ErlShell.reset_es_keypress = function() {
    $('#es_command_line').unbind('keypress');
    $('#es_command_line').attr({"id" : "", "contenteditable" : "false"});
    $('#es_result').attr({"id" : ""});
};

// ErlShell 的心跳包函数
ErlShell.erlshell_heart = function() {
    //ErlShell如果已经关闭，则关停定时器
    if ( ErlShell.status != 2 )
    {
        if ( ErlShell.timer )
        {
            clearTimeout(ErlShell.timer);
        }
        ErlShell.timer = undefined;
        return false;
    }
    var data = { "action" : 4, "pid" : ErlShell.pid };
    $.post(ErlShell.url, data, function(rs) {
        if ( rs.result == 41 )                      //进程异常关闭
        {
            ErlShell.erlshell_stop();
            alert("进程异常已关闭，请重新启动 ErlShell！"); 
        }
    }, "json");
};

//启动ErlShell
ErlShell.erlshell_init = function(rs) {
    //初始状态数据
    ErlShell.pid = rs.pid;
    ErlShell.interval = rs.interval;
    ErlShell.line_num = rs.line_num;
    ErlShell.status = 2;
    ErlShell.process = 0,
    ErlShell.command_history = [];
    ErlShell.command_cursor = 0;
    $("#es_div").html("");
    //创建命令行
    ErlShell.create_es_command_line(ErlShell.line_num);
    //绑定命令行事件
    ErlShell.bind_es_command_line_keypress();
    $("#es_div").css({"background-color" : "#FFF"});
    $("#erlshell_action").html("Stop");
    //开启 ErlShell 心跳包定时器
    ErlShell.timer = setInterval(ErlShell.erlshell_heart, ErlShell.interval * 1000);
    ErlShell.erlshell_heart();
    $(window).bind('beforeunload', function() {
        return "确定要退出 ErlShell ？";
    });
};

// 关闭ErlShell
ErlShell.erlshell_stop = function() {
    if ( ErlShell.timer )
    {
        clearTimeout(ErlShell.timer);
    }
    ErlShell.timer = undefined;
    ErlShell.pid = undefined;
    ErlShell.status = 1;
    $("#erlshell_action").html("Start");
    $("#es_div").css({"background-color" : "#EDEDED"});
    ErlShell.reset_es_keypress();
    $(window).unbind('beforeunload');
}

$("#erlshell_action").click(function() {
    if ( ErlShell.process == 1 ) {
        return false;
    }
    ErlShell.process = 1;
    var data = {};
    if ( ErlShell.status == 1 )
    {
        data = { "action" : 1 };
    }
    else
    {
        data = { "action" : 2, "pid" : ErlShell.pid };
        //关闭ErlShell
        ErlShell.erlshell_stop();
    }
    $.post(ErlShell.url, data, function(rs) {
        if ( rs.result == 1 && parseInt(rs.action, 10) == 1 )
        {
            //启动ErlShell
            ErlShell.erlshell_init(rs);
        }
        ErlShell.process = 0
    }, "json");
});

