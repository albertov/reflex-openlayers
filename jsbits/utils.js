"use strict";
function h$updateGroupLayers (group, layerArray) {
  var m  = h$mkHashMap(group.getLayers().getArray())
    , ls = [];
  for (var i=0; i<layerArray.length; i++) {
    var o=layerArray[i], h=o["h$hash"], old=m[h];
    ls.push((typeof old == "undefined")? o : old);
  }
  group.setLayers(new ol.Collection(ls));
}

function NoHashFound(message) {
   this.message = message;
   this.name = "NoHashFound";
}

function h$mkHashMap (arr) {
  var map = {}
  for (var i=0; i<arr.length; i++) {
    var o=arr[i], h=o["h$hash"];
    if (typeof h != "undefined") {
      map[h] = o;
    } else {
      throw new NoHashFound("Object did not have a StableName hash");
    }
  }
  return map
}
