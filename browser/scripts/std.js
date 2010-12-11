function getAbsolutePosition(element) {
     var r = { x: element.offsetLeft, y: element.offsetTop };
     if (element.offsetParent) {
         var tmp = getAbsolutePosition(element.offsetParent);
         r.x += tmp.x;
         r.y += tmp.y;
     }
     return r;
}

