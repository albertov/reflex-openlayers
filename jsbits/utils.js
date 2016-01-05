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

function h$updateView (map, view) {
  var v = map.getView() , z=view.get('zoom');
  if (v.getCenter()[0]!=view.getCenter()[0] ||
      v.getCenter()[1]!=view.getCenter()[1]
    ) {
    v.setCenter(view.getCenter());
  }
  if (v.getRotation()!=view.getRotation()) {
    v.setRotation(view.getRotation());
  }
  if (typeof z == "undefined") {
    if (v.getResolution()!=view.getResolution()) {
      v.setResolution(view.getResolution());
    }
  } else {
    if (v.getZoom()!=view.getZoom()) {
      v.setZoom(view.getZoom());
    }
  }
}
