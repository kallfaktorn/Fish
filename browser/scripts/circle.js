inc("point.js");

function Circle(center, radius) {
    this.center = center;
    this.radius = radius;

    this.intersectPoint = function(x,y) {
        var dx = x - this.center.x;
        var dy = y - this.center.y;

        if((dx*dx + dy*dy) < this.radius*this.radius) {
            return true;
        }

        else {
            return false;
        }
    };
}
