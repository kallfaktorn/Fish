inc("selectobject.js");

function Lake(circle, file) {
    this.inheritFrom = SelectObject; 
    this.inheritFrom();
    this.bounds  = circle;
    this.img     = new Image();
    this.img.src = file;
}
