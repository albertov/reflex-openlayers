"use strict";

function h$isDefined (a) {
  return typeof a != "undefined";
}
function h$updateGroupLayers (group, layerArray) {
  var m  = h$mkKeyMap(group.getLayers().getArray())
    , ls = [];
  for (var i=0; i<layerArray.length; i++) {
    var o=layerArray[i], h=o["h$key"], old=m[h];
    if (typeof old == "undefined") {
      ls.push(o);
    } else {
      h$updateLayer(old, o);
      ls.push(old);
    }
  }
  group.setLayers(new ol.Collection(ls));
}

function h$simplePropCompare(a,b) {return a===b};

function h$arraysEqual(a,b) {
  if (h$isInstanceOf(a, Array) && 
      h$isInstanceOf(b, Array) &&
      a.length === b.length) {
    for (var i=0; i<a.length; i++) {
      if (a[i]!==b[i])
        return false;
    }
    return true;
  }
  return a===b;
}

function h$compareHashes(a,b) {
  return h$isDefined(a['h$hash']) && h$eqStableName(a['h$hash'], b['h$hash']);
}

var h$layerProps = {
    "opacity"       : h$simplePropCompare
  , "visible"       : h$simplePropCompare
  , "extent"        : h$arraysEqual
  , "minResolution" : h$simplePropCompare
  , "maxResolution" : h$simplePropCompare
  , "zIndex"        : h$simplePropCompare
  , "source"        : h$compareHashes
};

function h$updateLayer(old, v) {
  var changed=false;
  for (var k in h$layerProps) {
    var eq = h$layerProps[k]
      , oldVal = old.get(k)
      , newVal = v.get(k);
    if (!eq(oldVal, newVal)) {
      changed = true;
      old.set(k, newVal, true);
      console.debug('changed', k);
    }
  }
  if (changed) {
    old.changed();
  }
}

function NoKeyFound(message) {
   this.message = message;
   this.name = "NoKeyFound";
}

function h$mkKeyMap (arr) {
  var map = {}
  for (var i=0; i<arr.length; i++) {
    var o=arr[i], h=o["h$key"];
    if (h$isDefined(h)) {
      map[h] = o;
    } else {
      throw new NoKeyFound("Object did not have a key");
    }
  }
  return map
}

function h$updateView (map, view) {
  var v = map.getView();
  if (!h$arraysEqual(v.getCenter(), view.getCenter())) {
    v.setCenter(view.getCenter());
  }
  if (v.getRotation()!=view.getRotation()) {
    v.setRotation(view.getRotation());
  }
  if (v.getResolution()!=view.getResolution()) {
    v.setResolution(view.getResolution());
  }
}
