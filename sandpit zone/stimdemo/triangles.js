function triangle(x1,y1,x2,y2,x3,y3){
    this.x1=x1;
    this.x2=x2;
    this.x3=x3;
    this.y1=y1;
    this.y2=y2;
    this.y3=y3;

    this.rotate90 = function(){//swaps width & height while preserving similarity
	var newx1=-this.y1;
	var newx2=-this.y2;
	var newx3=-this.y3;
	var newy1=this.x1;
	var newy2=this.x2;
	var newy3=this.x3;

	this.x1=newx1;
	this.x2=newx2;
	this.x3=newx3;
	this.y1=newy1;
	this.y2=newy2;
	this.y3=newy3;
    }
    
    this.scaleSize=function(scalefactor){
	this.x1*=scalefactor;
	this.x2*=scalefactor;
	this.x3*=scalefactor;
	this.y1*=scalefactor;
	this.y2*=scalefactor;
	this.y3*=scalefactor;
    }

    this.drawme = function(canvas){
	var leftmost = Math.min(this.x1,this.x2,this.x3);
	var highest = Math.min(this.y1,this.y2,this.y3);
	var rightmost = Math.max(this.x1,this.x2,this.x3);
	var lowest = Math.max(this.y1,this.y2,this.y3);
	var width = rightmost-leftmost;
	var height = lowest-highest;
	
	var shiftx = -leftmost+canvas.width/2-width/2;//canvas.width/2-width/2; //centers the shape by pegging the bottom left corner.
	var shifty = canvas.height-lowest-canvas.height/2+height/2;//canvas.height/2-height/2;

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
}


//main()
document.getElementById('uberdiv').innerHTML="<canvas id='testcanvas' width='300',height='300' style='border:1px solid black'></canvas>"+
    "<canvas id='testcanvas2' width='300',height='300' style='border:1px solid black'></canvas>"+
    "<canvas id='testcanvas3' width='300',height='300' style='border:1px solid black'></canvas>";
var type1 = new triangle(0,0,.5,-2,2,0);

type1.scaleSize(25);

type1.drawme(document.getElementById('testcanvas'));

type1.rotate90();
type1.drawme(document.getElementById('testcanvas2'));

type1.scaleSize(.8);
type1.drawme(document.getElementById('testcanvas3'));
