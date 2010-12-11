function Mouse() {
    this.x = 0;
    this.y = 0;

    this.update = function(e) {
        if(e.offsetX) {
            this.x = e.offsetX;
            this.y = e.offsetY;
        }
        else if(e.layerX) {
            this.x = e.layerX;
            this.y = e.layerY;
        }

        var tmp = getAbsolutePosition(canvas);
        this.x = this.x - tmp.x;
        this.y = this.y - tmp.y;
    }
}

