function init(){
    var options = {
        projection: new OpenLayers.Projection("EPSG:900913"),
        displayProjection: new OpenLayers.Projection("EPSG:4326")
    };


    // first animal

    map = new OpenLayers.Map('telekarte', options);

    var mapnik = new OpenLayers.Layer.OSM("OpenStreetMap (Mapnik)");

    var track_all = new OpenLayers.Layer.Vector("Alle Ortungen", {
        projection: map.displayProjection,
        strategies: [new OpenLayers.Strategy.Fixed()],
        protocol: new OpenLayers.Protocol.HTTP({
            url: "kml/12150_lokalisierungen_alle.kml",
            format: new OpenLayers.Format.KML({
                extractStyles: true,
                extractAttributes: true
            })
        })
    });

    var track_2w = new OpenLayers.Layer.Vector("Ortungen 2 Wochen", {
        projection: map.displayProjection,
        strategies: [new OpenLayers.Strategy.Fixed()],
        protocol: new OpenLayers.Protocol.HTTP({
            url: "kml/12150_lokalisierungen_2w.kml",
            format: new OpenLayers.Format.KML({
                extractStyles: true,
                extractAttributes: true
            })
        })
    });

    var mcp_all = new OpenLayers.Layer.Vector("MCP (alle)", {
        projection: map.displayProjection,
        strategies: [new OpenLayers.Strategy.Fixed()],
        protocol: new OpenLayers.Protocol.HTTP({
            url: "kml/12150_mcp_alle.kml",
            format: new OpenLayers.Format.KML({
                extractStyles: true,
                extractAttributes: true
            })
        })
    });

    var mcp_2w = new OpenLayers.Layer.Vector("MCP (2 Wochen)", {
        projection: map.displayProjection,
        strategies: [new OpenLayers.Strategy.Fixed()],
        protocol: new OpenLayers.Protocol.HTTP({
            url: "kml/12150_mcp_2w.kml",
            format: new OpenLayers.Format.KML({
                extractStyles: true,
                extractAttributes: true
            })
        })
    });


    var locoh_all = new OpenLayers.Layer.Vector("LoCoH (alle)", {
        projection: map.displayProjection,
        strategies: [new OpenLayers.Strategy.Fixed()],
        protocol: new OpenLayers.Protocol.HTTP({
            url: "kml/12150_locoh_alle.kml",
            format: new OpenLayers.Format.KML({
                extractStyles: true,
                extractAttributes: true
            })
        })
    });


    var locoh_2w = new OpenLayers.Layer.Vector("LoCoH (2 Wochen)", {
        projection: map.displayProjection,
        strategies: [new OpenLayers.Strategy.Fixed()],
        protocol: new OpenLayers.Protocol.HTTP({
            url: "kml/12150_locoh_2w.kml",
            format: new OpenLayers.Format.KML({
                extractStyles: true,
                extractAttributes: true
            })
        })
    });

    map.addLayers([mapnik,  track_all, track_2w, mcp_all, mcp_2w, locoh_all, locoh_2w]);

    //select = new OpenLayers.Control.SelectFeature(track_all);

    //sundials.events.on({
    //"featureselected": onFeatureSelect,
    //"featureunselected": onFeatureUnselect
    //});

    //map.addControl(select);
    //select.activate();   

    map.addControl(new OpenLayers.Control.LayerSwitcher());

    map.zoomToExtent(
        new OpenLayers.Bounds(
                10, 45, 12, 47
        ).transform(map.displayProjection, map.projection)
    );
}

function onPopupClose(evt) {
    select.unselectAll();
}

function onFeatureSelect(event) {
    var feature = event.feature;
    var selectedFeature = feature;
    var popup = new OpenLayers.Popup.FramedCloud("chicken", 
						 feature.geometry.getBounds().getCenterLonLat(),
						 new OpenLayers.Size(100,100),
						 "<h2>"+feature.attributes.name + "</h2>" + feature.attributes.description,
						 null, true, onPopupClose
						);
    feature.popup = popup;
    map.addPopup(popup);
}

function onFeatureUnselect(event) {
    var feature = event.feature;
    if(feature.popup) {
        map.removePopup(feature.popup);
        feature.popup.destroy();
        delete feature.popup;
    }
}

