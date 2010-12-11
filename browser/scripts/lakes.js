inc("lake.js");

function Lakes(ctx) {
    this.ctx = ctx;
    this.lakes = new Array();

    var p = new Point(1010,312);
    var c = new Circle(p,93);
    var file = 'images/' + imglakes.reed;
    var lake = new Lake(c,file);
    this.lakes[0] = lake;


    p = new Point(1349,153);
    c = new Circle(p,93);
    file = 'images/' + imglakes.corner;
    lake = new Lake(c,file);
    this.lakes[1] = lake;

    p = new Point(729,361);
    c = new Circle(p,93);
    file = 'images/' + imglakes.deep_water;
    lake = new Lake(c,file);
    this.lakes[2] = lake;
   
    p = new Point(505,506);
    c = new Circle(p,93);
    file = 'images/' + imglakes.shallow_water;
    lake = new Lake(c,file);
    this.lakes[3] = lake;

    p = new Point(545,342);
    c = new Circle(p,93);
    file = 'images/' + imglakes.peninsula;
    lake = new Lake(c,file);
    this.lakes[4] = lake;

    this.activate = function(x, y) {
        for(var i in this.lakes) {
            if(this.lakes[i].activate(x, y)) {
                this.ctx.drawImage(this.lakes[i].img,0,0);
            }
        }
    } 
} 
