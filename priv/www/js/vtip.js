/**
Vertigo Tip by www.vertigo-project.com
Requires jQuery
*/

this.vtip = function() {    
    var body_width = $(document.body).width();
    var body_height = screen.height;
    //alert(body_width); 
    this.xOffset = -10; // x distance from mouse
    this.yOffset = 10; // y distance from mouse       
    
    $(".vtip").unbind().hover(    
        function(e) {
            this.t = this.title;
            this.title = ''; 
            this.top = (e.pageY + yOffset); 
            this.left = (e.pageX + xOffset);
                        
            $('body').append( '<div id="vtip">' + this.t + '</div>' );
            this.w = $("div#vtip").width();
            if ( this.left + this.w > body_width - 100 )
            {
                this.left = body_width - this.w - 100; 
            }

            this.h = $("div#vtip").height();
		    if ( (body_height - 150 - this.top) < this.h )
            { 
                this.top -= this.h + 30; 
                
            }

            $('div#vtip').css("top", this.top+"px").css("left", this.left+"px").fadeIn("slow");
            
        },
        function() {
            this.title = this.t;
            $("div#vtip").fadeOut("slow").remove();
        }
    ).mousemove(
        function(e) {
            /*this.top = (e.pageY + yOffset);
            this.left = (e.pageX + xOffset);
            this.w = $("div#vtip").width();
            if ( this.left + this.w > body_width - 100 )
            {
                this.left = body_width - this.w - 100; 
            }
            this.h = $("div#vtip").height();
		    if ( (screen.height - 170 - this.top) < this.h )
            { 
                this.top -= this.h; 
                
            }

          
            $("div#vtip").css("top", this.top+"px").css("left", this.left+"px");*/
        }
    );            
    
};

jQuery(document).ready(function($){vtip();}) 

