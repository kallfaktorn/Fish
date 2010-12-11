function SelectObject() {
    this.active = false;
    this.bounds;

    this.activate = function(x, y) {
        this.active = this.bounds.intersectPoint(x, y);
        return this.active; 
    }
}
