const canvassize = 110;

function shuffle(a) { //credit SO
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = a[i];
        a[i] = a[j];
        a[j] = x;
    }
    return a;
}

function triangle(x1,y1,x2,y2,x3,y3){
    this.x1=x1;
    this.x2=x2;
    this.x3=x3;
    this.y1=y1;
    this.y2=y2;
    this.y3=y3;

    this.cloneme = function(){
	return new triangle(this.x1,this.y1,this.x2,this.y2,this.x3,this.y3);
    }

    this.rotate90 = function(){//swaps width & height while preserving similarity
	var newx1=-this.y1;
	var newx2=-this.y2;
	var newx3=-this.y3;
	var newy1=this.x1;
	var newy2=this.x2;
	var newy3=this.x3;

	return new triangle(newx1,newy1,newx2,newy2,newx3,newy3);
    }
    
    this.scaleSize=function(scalefactor){
	var next_x1 = this.x1*scalefactor;
	var next_x2 = this.x2*scalefactor;
	var next_x3 = 	this.x3*scalefactor;
	var next_y1 = 	this.y1*scalefactor;
	var next_y2 = 	this.y2*scalefactor;
	var next_y3 = 	this.y3*scalefactor;

	return new triangle(next_x1,next_y1,next_x2,next_y2,next_x3,next_y3);
    }

    this.pullToWidth = function(targwidth){ //too hacky?
	var apex_gap = this.x2-this.x1;
	var next_x3=this.x1+targwidth;
	var next_x2=this.x2+apex_gap;
	return new triangle(this.x1,this.y1,next_x2,this.y2,next_x3,this.y3)// hmm. I guess you have to distort the angles sometime?
    }
    this.pullToHeight = function(targheight){
	return new triangle(this.x1,this.y1,this.x2,this.y1-targheight,this.x3,this.y3);
    }
    
    this.drawme = function(canvas){
	var leftmost = Math.min(this.x1,this.x2,this.x3);
	var highest = Math.min(this.y1,this.y2,this.y3);
	var rightmost = Math.max(this.x1,this.x2,this.x3);
	var lowest = Math.max(this.y1,this.y2,this.y3);
	var width = rightmost-leftmost;
	var height = lowest-highest;
	
	var shiftx = -leftmost+canvas.width/2-width/2;//shift centers the shape
	var shifty = canvas.height-lowest-canvas.height/2+height/2;

	console.log(shiftx+":"+shifty);
	if (canvas.getContext) {

	    var ctx = canvas.getContext('2d');
	    // ctx.rect(this.x1+shiftx,this.y1+shifty,10,10)
	    // ctx.stroke();

	    // ctx.rect(this.x2+shiftx,this.y2+shifty,10,10)
	    // ctx.stroke();

	    // ctx.rect(this.x3+shiftx,this.y3+shifty,10,10)
	    // ctx.stroke();


	    ctx.beginPath();
	    ctx.moveTo(this.x1+shiftx,this.y1+shifty);
	    ctx.lineTo(this.x2+shiftx,this.y2+shifty);
	    ctx.lineTo(this.x3+shiftx,this.y3+shifty);
	    ctx.fill();
	}
    }
}//end triangle

var triangleTypes = [//room to play with these: but make sure the 'base' templates have the same area! Later sizes should be as multiples of this base size.
    new triangle(0,0,     0,   -2,2,0),//right-angle
    new triangle(0,0,    1,   -2,2,0), //equi
    new triangle(0,0,    3,   -2,2,0) //acute skew
    ]

function trialobj(triangles,roles,stimid){ //responsible for randomization of locations and starting orientation, drawing to screen, (and later recording responses?)
    this.triangles = triangles;
    this.roles = roles;
    this.presentation_position = shuffle([0,1,2]);
    this.stimid = stimid;
    var hm_rotations = shuffle([0,1,2,3])[0];

    for(i=0;i<hm_rotations;i++){
	this.triangles[i]=this.triangles[i].rotate90();//whee
    }

    this.drawme = function(targdiv){
	document.getElementById(targdiv).innerHTML="<table style='border:solid 3px black'>"+//haha, tables. Oh dear.
	"<tr><td colspan='2' align='center'>"+
	    "<canvas id='canvas0"+stimid+"' width='"+canvassize+"' height='"+canvassize+"'></canvas>"+
	    "</td></tr>"+
	    "<tr>"+
	    "<td>"+"<canvas id='canvas1"+stimid+"' width='"+canvassize+"' height='"+canvassize+"'></canvas>"+"</td>"+
	    "<td>"+"<canvas id='canvas2"+stimid+"' width='"+canvassize+"' height='"+canvassize+"'></canvas>"+"</td>"+
	    "<tr>"+
	    "</table>";
	for(i=0;i<this.presentation_position.length;i++){
	    triangles[this.presentation_position[i]].drawme(document.getElementById('canvas'+i+this.stimid));
	}
    }

    
}
		  

//main()

 for(i=0;i<triangleTypes.length;i++){triangleTypes[i]=triangleTypes[i].scaleSize(25);}//setup
// var test_demo = new trialobj(triangleTypes,["A","B","C"],"test")
// test_demo.drawme('uberdiv');
