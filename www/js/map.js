function init(bbox) {
    var options = {
        projection: new OpenLayers.Projection("EPSG:900913"),
        displayProjection: new OpenLayers.Projection("EPSG:4326")
    };
    map = new OpenLayers.Map("telekarte", options);
    mapnik = new OpenLayers.Layer.OSM("Open Street Map")
    var gphy = new OpenLayers.Layer.Google(
	"Google Physical",
	{type: google.maps.MapTypeId.TERRAIN}
	// used to be {type: G_PHYSICAL_MAP}
    );
    
    var gmap = new OpenLayers.Layer.Google(
	"Google Streets", // the default
	{numZoomLevels: 20}
	// default type, no change needed here
    );
    var ghyb = new OpenLayers.Layer.Google(
	"Google Hybrid",
	{type: google.maps.MapTypeId.HYBRID, numZoomLevels: 20}
	// used to be {type: G_HYBRID_MAP, numZoomLevels: 20}
    );
    var gsat = new OpenLayers.Layer.Google(
	"Google Satellite",
	{type: google.maps.MapTypeId.SATELLITE, numZoomLevels: 22}
	// used to be {type: G_SATELLITE_MAP, numZoomLevels: 22}
    );
    var layers = [mapnik, gphy, gmap, ghyb, gsat];

    // Animal data
    var abc;
    var ids = ['11578', '11579'];
    var colors = ['#ffcc66', '#66ccff'];
    for (var i = 0, len = ids.length; i < len; i++) {
	var id = ids[i];

	// styles
	var pstyle = OpenLayers.Util.extend({}, OpenLayers.Feature.Vector.style['default']);
	//style.fillOpacity = 0.2;
	//style.graphicOpacity = 1;
	pstyle.strokeWidth = 0; 
	pstyle.fillColor = colors[i];
	pstyle.strokeOpacity = 1;

	var lstyle = OpenLayers.Util.extend({}, OpenLayers.Feature.Vector.style['default']);
	lstyle.fillOpacity = 0.2;
	//style.graphicOpacity = 1;
	lstyle.strokeWidth = 2; 
	lstyle.fillColor = colors[i];
	lstyle.strokeColor = colors[i];
	lstyle.strokeOpacity = 1;
	
	var lstyle14 = OpenLayers.Util.extend({}, OpenLayers.Feature.Vector.style['default']);
	lstyle14.fillOpacity = 0.2;
	//style.graphicOpacity = 1;
	lstyle14.strokeWidth = 2; 
	lstyle14.fillColor = colors[i];
	lstyle14.strokeColor = colors[i];
	lstyle14.strokeOpacity = 1;
	lstyle14.strokeDashstyle = "dash";

	// Add layers
	layers.push(tracks_2w = new OpenLayers.Layer.Vector(id + ": Ortungen 2 Wochen", {
            projection: map.displayProjection,
            strategies: [new OpenLayers.Strategy.Fixed()],
            protocol: new OpenLayers.Protocol.HTTP({
		url: "kmls/" + id + "_lokalisierungen_2w.kml",
		format: new OpenLayers.Format.KML({
                    extractStyles: false,
                    extractAttributes: true
		})
            }),
	    style:pstyle,
	    visibility:true
	}));

	layers.push(new OpenLayers.Layer.Vector(id + ": Ortungen (alle)", {
            projection: map.displayProjection,
            strategies: [new OpenLayers.Strategy.Fixed()],
            protocol: new OpenLayers.Protocol.HTTP({
		url: "kmls/" + id + "_lokalisierungen_alle.kml",
		format: new OpenLayers.Format.KML({
                    extractStyles: false,
                    extractAttributes: true
		})
            }),
	    style:pstyle,
	    visibility:false
	}));


	layers.push(new OpenLayers.Layer.Vector(id + ": MCP 2 Wochen", {
            projection: map.displayProjection,
            strategies: [new OpenLayers.Strategy.Fixed()],
            protocol: new OpenLayers.Protocol.HTTP({
		url: "kmls/" + id + "_mcp_2w.kml",
		format: new OpenLayers.Format.KML({
                    extractStyles: false,
                    extractAttributes: true
		})
            }),
	    style:lstyle14,
	    visibility:false
	}));

	layers.push(new OpenLayers.Layer.Vector(id + ": MCP (alle)", {
            projection: map.displayProjection,
            strategies: [new OpenLayers.Strategy.Fixed()],
            protocol: new OpenLayers.Protocol.HTTP({
		url: "kmls/" + id + "_mcp_alle.kml",
		format: new OpenLayers.Format.KML({
                    extractStyles: false,
                    extractAttributes: true
		})
            }),
	    style:lstyle,
	    visibility:false
	}));

	layers.push(new OpenLayers.Layer.Vector(id + ": LoCoH 2 Wochen", {
            projection: map.displayProjection,
            strategies: [new OpenLayers.Strategy.Fixed()],
            protocol: new OpenLayers.Protocol.HTTP({
		url: "kmls/" + id + "_locoh_2w.kml",
		format: new OpenLayers.Format.KML({
                    extractStyles: false,
                    extractAttributes: true
		})
            }), 
	    style:lstyle14,
	    visibility:true
	}));

	layers.push(new OpenLayers.Layer.Vector(id + ": LoCoH (alle) <br>", {
            projection: map.displayProjection,
            strategies: [new OpenLayers.Strategy.Fixed()],
            protocol: new OpenLayers.Protocol.HTTP({
		url: "kmls/" + id + "_locoh_alle.kml",
		format: new OpenLayers.Format.KML({
                    extractStyles: false,
                    extractAttributes: true
		})
            }),
	    style:lstyle,
	    visibility:false
	}));
    }

    // bounding box of first overlay layer

    // cycle over all overlays, starting from 4 becasue the 0 to 4 are base layers
    for (var i = 4, len = length.length; i < len; i++) {
    
    }
    // add layers to the map
    map.addLayers(layers);


    //map.addControl(external_control);
    map.addControl(new OpenLayers.Control.LayerSwitcher({'ascending':true, 'div': OpenLayers.Util.getElement('right') }));
    map.zoomToExtent(new OpenLayers.Bounds(bbox).transform(map.displayProjection, map.projection));
}

