//housekeeping
const canvassize = 125;

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

function recordResponse(responseType,stimsummary){
    //TODO, save response and advance to next trial.
    console.log(responseType);//placeholder
    console.log(stimsummary);
}

//stim template. Can rotate, stretch height or width, & dilate
function triangle(x1,y1,x2,y2,x3,y3,orientation){
    this.orientation = orientation; //0,1,2,3 = NSEW, point opposite the shortest side. Is this a useful concept?
    this.x1=x1;
    this.x2=x2;
    this.x3=x3;
    this.y1=y1;
    this.y2=y2;
    this.y3=y3;

    this.cloneme = function(){//js is literally satan. Never do anything 'in place' just in case.
	return new triangle(this.x1,this.y1,this.x2,this.y2,this.x3,this.y3,this.orientation);
    }

    this.area = function(){
	//heron's formula, because 'base' and 'height' are visually obvious (i hope?) but annoying in xycords land.
	function linelength(x1,y1,x2,y2){
	    var a = x2-x1;
	    var b = y2-y1;
	    return Math.sqrt(a*a+b*b)
	}
	var a = linelength(this.x1,this.y1,this.x2,this.y2);
	var b = linelength(this.x2,this.y2,this.x3,this.y3);
	var c = linelength(this.x3,this.y3,this.x1,this.y1);
	var s = (a+b+c)/2;
	return Math.sqrt(s*(s-a)*(s-b)*(s-c));
	
    }

    this.rotate90 = function(){//swaps width & height while preserving similarity properties
	var newx1=-this.y1;
	var newx2=-this.y2;
	var newx3=-this.y3;
	var newy1=this.x1;
	var newy2=this.x2;
	var newy3=this.x3;

	return new triangle(newx1,newy1,newx2,newy2,newx3,newy3,(this.orientation+1)%4);
    }
    
    this.scaleSize=function(scalefactor){
	var next_x1 = this.x1*scalefactor;
	var next_x2 = this.x2*scalefactor;
	var next_x3 = 	this.x3*scalefactor;
	var next_y1 = 	this.y1*scalefactor;
	var next_y2 = 	this.y2*scalefactor;
	var next_y3 = 	this.y3*scalefactor;

	return new triangle(next_x1,next_y1,next_x2,next_y2,next_x3,next_y3,this.orientation);
    }

    this.equal_area_slide = function (whichdimension,squashiness){
	var smaller = this.scaleSize(squashiness);
	if(whichdimension=="x"){
	    while(smaller.area()<this.area()){smaller.x2+=.001; smaller.x3+=.001}
	}else{
	    while(smaller.area()<this.area()){smaller.y2-=.001;}
	}
	return smaller;//which has now been hackily stretched to equal. Note orientation left alone (but maybe you should check this...)
    }
    
    this.drawme = function(canvas){
	var leftmost = Math.min(this.x1,this.x2,this.x3);
	var highest = Math.min(this.y1,this.y2,this.y3);
	var rightmost = Math.max(this.x1,this.x2,this.x3);
	var lowest = Math.max(this.y1,this.y2,this.y3);
	var width = rightmost-leftmost;
	var height = lowest-highest;
	
	var shiftx = -leftmost+canvas.width/2-width/2 + (-5+10*Math.random());//center the shape & add 10px jitter 
	var shifty = canvas.height-lowest-canvas.height/2+height/2 + (-5+10*Math.random());

	if (canvas.getContext) {

	    var ctx = canvas.getContext('2d');
	    ctx.beginPath();
	    ctx.moveTo(this.x1+shiftx,this.y1+shifty);
	    ctx.lineTo(this.x2+shiftx,this.y2+shifty);
	    ctx.lineTo(this.x3+shiftx,this.y3+shifty);
	    ctx.fill();
	}
    }
}//end triangle

//exp specific: three basic shape-type templates.
var triangleTypes = [//room to play with these: but make sure the 'base' templates have the same area! Later sizes should be as multiples of this base size.
    new triangle(0,0,     0,-5,   2,0,    0),//right-angle
    new triangle(0,0,     1,-5,   2,0,    0), //equi
    new triangle(0,0,     3,-5,   2,0,    0) //acute skew
    ]

function trialobj(triangles,roles,stimid){ //responsible for drawing to screen, including randomization of locations and starting orientation. Also TODO here recording responses.
    this.triangles = triangles;
    this.roles = roles;
    this.presentation_position = shuffle([0,1,2]);
    this.stimid = stimid;
    this. hm_rotations = shuffle([0,1,2,3])[0]; //canonical orientation is tall, randomize so NSEW versions all presented.

    for(var i=0;i<this.hm_rotations;i++){
    	for(var j=0;j<this.triangles.length;j++)this.triangles[j]=this.triangles[j].cloneme().rotate90();//whee
    }

    this.drawme = function(targdiv){
	document.getElementById(targdiv).innerHTML="<table style='border:solid 3px black'>"+//haha, tables. Oh dear.
	"<tr><td colspan='2' align='center' class='buttontd'><button onclick=recordResponse('"+this.roles[this.presentation_position[0]]+"','"+this.summarystring()+"')>This one</button></td></tr>"+
	"<tr><td colspan='2' align='center'>"+
	    "<canvas id='canvas0"+stimid+"' width='"+canvassize+"' height='"+canvassize+"'></canvas>"+
	    "</td></tr>"+
	    "<tr>"+
	    "<td align='center'>"+"<canvas id='canvas1"+stimid+"' width='"+canvassize+"' height='"+canvassize+"'></canvas>"+"</td>"+
	    "<td align='center'>"+"<canvas id='canvas2"+stimid+"' width='"+canvassize+"' height='"+canvassize+"'></canvas>"+"</td>"+
	    "</tr>"+
	    "<tr><td align='left' class='buttontd'><button onclick=recordResponse('"+this.roles[this.presentation_position[1]]+"','"+this.summarystring()+"')>This one</button></td> <td align='right' class='buttontd'><button onclick=recordResponse('"+this.roles[this.presentation_position[2]]+"','"+this.summarystring()+"')>This one</button></td></tr>"+
	    "</table>";
	for(var i=0;i<this.presentation_position.length;i++){
	    triangles[this.presentation_position[i]].drawme(document.getElementById('canvas'+i+this.stimid));
	}
    }
    //possibly add this.summarystring(), pass that to recordResponse
    this.summarystring=function(){
	this.areas = [this.triangles[0].area(), this.triangles[1].area(),this.triangles[2].area()];//you might want to inspect these?
	return(JSON.stringify(this)); //could mess with this if it's convenient? This seems like a nice conservative way though.
    }
    
}

//trial-type generators:
function getSingleWinnerTrial(shapelist, trialid, winnersize){//shapelist should be an array of indexes into 'triangleTypes'
    triangleTypes = shuffle(triangleTypes);//random mapping
    var mytriangles = [
	triangleTypes[shapelist[0]].cloneme().scaleSize(winnersize),//winner!
	triangleTypes[shapelist[1]].cloneme(),
	triangleTypes[shapelist[2]].cloneme()
    ];
    var rnd_spins = [shuffle([0,1,2,3])[0],
		     shuffle([0,1,2,3])[0],
		     shuffle([0,1,2,3])[0]
		    ];

    for(var i=0;i<mytriangles.length;i++){
	for(var j=0;j<rnd_spins[i];j++)mytriangles[i]=mytriangles[i].rotate90();
    }
    return new trialobj(mytriangles,["winner","A","B"],trialid);
}

function getAttractionDecoyTrial(shapelist,trialid,decoysize){//decoy inferior on both attributes
    triangleTypes = shuffle(triangleTypes);//random mapping
    var mytriangles = [
	triangleTypes[shapelist[0]].cloneme(),
	triangleTypes[shapelist[1]].cloneme().rotate90(), //template triangles are in 'canonical orientation' whatever that is: rotate90 swaps width & height
	triangleTypes[shapelist[2]].cloneme().scaleSize(decoysize),//decoy is on both attributes
    ];
    
    return new trialobj(mytriangles,["targ","comp","decoy"],trialid);
}

function getSimilarityDecoyTrial(shapelist,trialid,squashfactor){ //the perceptual similarity story in triangle-land makes these 'similarity effect' stimuli VERY WEIRD. Fat vs skinny odd-one-out effects look like they might be huge?
    triangleTypes = shuffle(triangleTypes);
    var slidefactor = 1.2;
    
    var mytriangles = [
	triangleTypes[shapelist[0]].cloneme(),
	triangleTypes[shapelist[1]].cloneme().rotate90(),
	triangleTypes[shapelist[2]].cloneme().equal_area_slide("y",squashfactor) //orientation is randomized, but 'x' here means "fatter" and 'y' means "skinnier" same-area triangle compared to the template.
    ];
    
    return new trialobj(mytriangles,["targ","comp","decoy"],trialid);
}

//special generator: take in a trial-getting-function and make 5 shape-type variations with it.
function getShapeFlavors(trialgetter,id_prefix,sizesetting){
    return [trialgetter([0,0,0],id_prefix+"000",sizesetting),//all match
	    trialgetter([0,0,1],id_prefix+"001",sizesetting),//odd decoy
	    trialgetter([0,1,0],id_prefix+"010",sizesetting),//odd competitor
	    trialgetter([1,0,0],id_prefix+"100",sizesetting),//odd target
	    trialgetter([0,1,2],id_prefix+"012",sizesetting)]//no matches
}

//main()
 for(i=0;i<triangleTypes.length;i++){triangleTypes[i]=triangleTypes[i].scaleSize(20);}//setup, original templates are tiny.


//var stimlist = []
var stimlist = [].concat(
    getShapeFlavors(getSimilarityDecoyTrial,"similarity",.7)).concat(
	getShapeFlavors(getAttractionDecoyTrial,"attraction_both",.8)).concat(
	    getShapeFlavors(getSingleWinnerTrial,"singlewinner",1.2)) //max 1.3
	

for(var i =0;i<stimlist.length;i++){
     document.write("<div id='stimholder"+i+"'></div><p>"+stimlist[i].stimid+"</p>");
     stimlist[i].drawme("stimholder"+i);
 }
