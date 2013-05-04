<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<link rel="icon" href="http://{{domain}}/static/favicon.ico" type="image/x-icon" />
<link rel="shortcut icon" href="http://{{domain}}/static/favicon.ico" type="image/x-icon" />
<link rel="alternate" type="application/atom+xml" href="http://{{domain}}/static/feed.rss" title="{{application_name}}" />
<title>{{application_name}}</title>
<link rel="stylesheet" type="text/css" media="all" href="http://{{domain}}/static/css/style.css" />

{% block other_css %}

{% endblock %}

</head>
<body>
    <div class="main">
        <div class="header">
            <h1>{{application_name}}</h1>
            <h2>Just code for fun!</h2>
        </div>     

    {% block content %}

    {% endblock %}

    	<div class="footer">
            Powered By&nbsp;:&nbsp;<a href="http://dhq.me/">D.H.Q的烂笔头</a>
        </div>    
    
    </div>
<script type="text/javascript" src="http://code.jquery.com/jquery-1.8.0.min.js"></script>
<script type="text/javascript">
    var Domain = "{{domain}}";
</script>

{% block other_js %}

{% endblock %}

</body>
</html>
