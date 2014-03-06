var PP = PP || {};

PP.Constants = {
    BOUNDING_BOX : "-9222891.832889367,4212750.376909204,-9153945.633376136,4263045.941520849",
    DEFAULT_OPACITY : 0.9,
    GEOCODE_LOWERLEFT : { lat: 35.0, lng: -83.0 },
    GEOCODE_UPPERRIGHT: { lat: 36.0, lng: -82.0 }
};


PP.Geocoder = (function(){
    var config = {
        'notFoundMessage' : 'Sorry, that address could not be found.',
        'messageHideDelay': 3000,
	'agsServerGeocode': 'gis.ashevillenc.gov', //ArcGIS  server name for geocoding
	'agsServerInstanceNameGeocode': 'COA_ArcGIS_Server', //ArcGIS  server instance for geocoding
	'geocdingLayerName': 'Buncombe_Address_WGS84', //geocoding service to use.        
	'mySRID': 4326 //your projection id
    };

    var getLatLong = function (someData, callback){
	xStr=someData.x;
	yStr=someData.y;
        
	var urlStr = 'http://'+config.agsServerGeocode+'/'+config.agsServerInstanceNameGeocode+'/rest/services/Geometry/GeometryServer/project';
	var aPt=JSON.stringify({geometryType:"esriGeometryPoint",geometries : [{"x":xStr,"y":yStr}]});
        
	var sData={f:"json",inSR:config.mySRID,outSR:4326,geometries:aPt};
        
	$.ajax({
	    url: urlStr,
	    dataType: "jsonp",
	    data: sData,
	    crossDomain: true,
	    success:callback,//$.proxy(callback,this),
	    error:function(x,t,m){console.log('fail');}//updateResultsFail(t,'Error with transforming to WGS84!')
	});
    };

    var geocoder = null;
    return {
        onLoadGoogleApiCallback : function() {
            geocoder = new google.maps.Geocoder();
            document.body.removeChild(document.getElementById('load_google_api'));
        },

        init : function() {
            var url = 
                "https://maps.googleapis.com/maps/api/js?" + 
                "v=3&callback=PP.Geocoder.onLoadGoogleApiCallback&sensor=false";
            var script = document.createElement('script');
            script.id = 'load_google_api';
            script.type = "text/javascript";
            script.src = url;
            document.body.appendChild(script);
        },

        geocode : function (address, callback) {
            $.when(
                $.getJSON("gt/geocode?address=" + address)
            ).then(
                callback,
                function(err) {
                    console.error('Error geocoding address "' + address + '": ', err.statusText, err);
                }
            );

	    // addressStr = qry;
	    // var urlStr = 'http://'+config.agsServerGeocode+'/'+config.agsServerInstanceNameGeocode+'/rest/services/'+config.geocdingLayerName+'/GeocodeServer/findAddressCandidates';
	    // var sData={f:"json",Street:addressStr};
            
	    // $.ajax({
	    //     url: urlStr,
	    //     dataType: "jsonp",
	    //     data: sData,
	    //     success: function(data) {
	    //         if (data.candidates) {
	    //             it = data.candidates[0];
	    //             getLatLong({ label: it.address, value: it.address, x:it.location.x,y:it.location.y }, callback );
	    //         }
	    //     }
	    // });
        },

        geocode2 : function(address,callback) {
            var lowerLeft = new google.maps.LatLng(PP.Constants.GEOCODE_LOWERLEFT.lat, 
                                                   PP.Constants.GEOCODE_LOWERLEFT.lng);

            var upperRight = new google.maps.LatLng(PP.Constants.GEOCODE_UPPERRIGHT.lat, 
                                                    PP.Constants.GEOCODE_UPPERRIGHT.lng);

            var bounds = new google.maps.LatLngBounds(lowerLeft, upperRight);

            var parameters = {
                address: address,
                bounds: bounds
            };

            var results = geocoder.geocode(parameters, callback);
        }
    };
})();

PP.Util = (function() {
    return {
        toggleToolActive : function(e) {
            $(this).toggleClass('active');
        }
    };
})();

PP.App = (function() {
    'use strict';
    var model = (function() {
        var listeners = [];

        var layers = [];
        var categories = [];
        var activeLayers = [];
        var weights = {};
        var colorRamp = "blue-to-red";

        var notifyChange = function() { 
            _.each(listeners, function(f) { f(); });
        }

        return {
            notifyChange: notifyChange,
            
            onChange : function(f) {
                listeners.push(f);
            },

            initialize : function(ls,cats) {
                layers = ls;
                var layersMap = 
                    _.object(_.map(ls, function(layer) {
                        return [layer.id, layer]
                    }));

                categories = 
                    _.map(cats, function(category) {
                        category.layers = 
                            _.map(category.layers, function(layer) {
                                return layersMap[layer];
                            });
                        return category;
                    });

                weights =  _.object(_.map(layers, function(layer) {
                    return [layer.id, 0]
                }));
                
                // Setup default active layers.
                activeLayers = _.map(layers, function(l) { return l.id; });
            },

            getLayers: function() { return layers; },

            addActiveLayer: function(layer,weight) {
                if(!_.contains(activeLayers,layer.id)) {
                    activeLayers.push(layer.id);
                    notifyChange();
                };
            },
            removeActiveLayer: function(layer) {
                if(_.contains(activeLayers,layer.id)) {
                    var i = activeLayers.indexOf(layer.id);
                    activeLayers.splice(i,1);
                    notifyChange();
                };
            },

            updateLayerWeight: function(layer,weight) {
                if(weights[layer.id] != weight) {
                    weights[layer.id] = weight;
                    notifyChange();
                };
            },

            getActiveLayerWeights: function() {
                var activeWeights = {};
                _.forEach(activeLayers, function(id) {
                    activeWeights[id] = weights[id];
                });
                return activeWeights;
            },

            setColorRamp: function(rampId) {
                if(colorRamp != rampId) {
                    colorRamp = rampId;
                    notifyChange();
                };
            },

            getColorRamp: function() { return colorRamp }
        };
    })();

    var map = {};
    var initMap = function () {
        var viewCoords = [35.576917,-82.55275];

        var getLayer = function(url,attrib) {
            return L.tileLayer(url, { maxZoom: 18, attribution: attrib });
        };

        var Layers = {
            stamen: { 
                toner:  'http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png',   
                terrain: 'http://{s}.tile.stamen.com/terrain/{z}/{x}/{y}.png',
                watercolor: 'http://{s}.tile.stamen.com/watercolor/{z}/{x}/{y}.png',
                attrib: 'Map data &copy;2013 OpenStreetMap contributors, Tiles &copy;2013 Stamen Design'
            },
            mapBox: {
                azavea:     'http://{s}.tiles.mapbox.com/v3/azavea.map-zbompf85/{z}/{x}/{y}.png',
                wnyc:       'http://{s}.tiles.mapbox.com/v3/jkeefe.map-id6ukiaw/{z}/{x}/{y}.png',
                worldGlass:     'http://{s}.tiles.mapbox.com/v3/mapbox.world-glass/{z}/{x}/{y}.png',
                worldBlank:  'http://{s}.tiles.mapbox.com/v3/mapbox.world-blank-light/{z}/{x}/{y}.png',
                worldLight: 'http://{s}.tiles.mapbox.com/v3/mapbox.world-light/{z}/{x}/{y}.png',
                attrib: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery &copy; <a href="http://mapbox.com">MapBox</a>'
            }
        };

        var selected = getLayer(Layers.mapBox.azavea,Layers.mapBox.attrib);

        var baseLayers = {
            "Azavea" : selected,
            "WNYC" : getLayer(Layers.mapBox.wnyc,Layers.mapBox.attrib),
            "World Light" : getLayer(Layers.mapBox.worldLight,Layers.mapBox.attrib),
            "Terrain" : getLayer(Layers.stamen.terrain,Layers.stamen.attrib),
            "Watercolor" : getLayer(Layers.stamen.watercolor,Layers.stamen.attrib),
            "Toner" : getLayer(Layers.stamen.toner,Layers.stamen.attrib),
            "Glass" : getLayer(Layers.mapBox.worldGlass,Layers.mapBox.attrib),
            "Blank" : getLayer(Layers.mapBox.worldBlank,Layers.mapBox.attrib)
        };

        map = L.map('map').setView(viewCoords, 11);
        selected.addTo(map);

        map.lc = L.control.layers(baseLayers).addTo(map);

        $('#map').resize(function() {
            map.setView(map.getBounds(),map.getZoom());
        });

        var parcelLayer = 
            new L.TileLayer.WMS("http://tomcatgis.ashevillenc.gov/geoserver/wms", {
                layers: "coagis:bc_property",
                srs: "EPSG:2264",
                transparent: "true",
                format: "image/png"
            })

        parcelLayer.addTo(map);
        map.lc.addOverlay(parcelLayer, "Parcels");

        // Overview Map
        var overviewLayer = getLayer(Layers.mapBox.azavea,Layers.mapBox.attrib);
        var miniMap = new L.Control.MiniMap(overviewLayer).addTo(map);

        // Geocoder
        // var geocoderLayer = new L.Control.QuickGeocode().addTo(map);
    };

    var weightedOverlay = (function() {
        var layers = [];

        var layersToWeights = {}

        var breaks = null;

        var WOLayer = null;
        var opacity = PP.Constants.DEFAULT_OPACITY;
        var numBreaks = 10;

        var getLayerStrings = function() {
            var layers = model.getActiveLayerWeights();
            var ls = [];
            var ws = [];
            for(var id in layers) {
                if(layers.hasOwnProperty(id)) {
                    if(layers[id] != 0) {
                        ls.push(id);
                        ws.push(layers[id]);
                    };
                };
            };
            return {
                layers: ls.join(","),
                weights: ws.join(",")
            };
        };

        var update = function() {
                var layerStrings = getLayerStrings();
                if(layerStrings.layers == "") { 
                    if (WOLayer) {
                        map.lc.removeLayer(WOLayer);
                        map.removeLayer(WOLayer);
                        WOLayer = null;
                    }
                    return;
                };

                $.ajax({
                    url: 'gt/breaks',
                    data: { 
                        'bbox' : PP.Constants.BOUNDING_BOX,
                        'cols' : 400,
                        'rows' : 400,
                        'layers' : layerStrings.layers, 
                        'weights' : layerStrings.weights,
                        'numBreaks': numBreaks 
                    },
                    dataType: "json",
                    success: function(r) {
                        breaks = r.classBreaks;

                        if (WOLayer) {
                            map.lc.removeLayer(WOLayer);
                            map.removeLayer(WOLayer);
                        };

                        // Call again in case things have changed.
                        layerStrings = getLayerStrings();
                        if(layerStrings.layers == "") return;

                        // var geoJson = "";
                        // var polygon = summary.getPolygon();
                        // if(polygon != null) {
                        //     geoJson = GJ.fromPolygon(polygon);
                        // }

                        WOLayer = new L.TileLayer.WMS("gt/wo", {
                            breaks: breaks,
                            //                                    transparent: true,
                            layers: layerStrings.layers,
                            weights: layerStrings.weights,
                            colorRamp: model.getColorRamp(),
                            //                                    mask: geoJson,
                            attribution: 'Azavea'
                        })

                        WOLayer.setOpacity(opacity);
                        WOLayer.addTo(map);
                        map.lc.addOverlay(WOLayer, "Suitability Map");
                    }
                });
        };

        return {
            init : function() {
                model.onChange(function () {
                    weightedOverlay.update();
                });
            },
            
            setOpacity: function(v) {
                opacity = v;
                if(WOLayer) { WOLayer.setOpacity(opacity); }
                else { console.log("NO OVERLAY"); };
                
            },

            getOpacity: function() { return opacity ; },

            update: update
        };
    })();

    var parcelDetails = (function() {
        var getFeatureUrl = function(lat,lng) {
            return "gt/getParcel?lat=" + lat + "&lng=" + lng;
        }

        var template = Handlebars.compile($('#parcel-details-template').html())
        var popup = L.popup();

        var parcelLayer = null; 

        var fetchParcel = function(latlng, cb) {
            $.when(
                $.getJSON(getFeatureUrl(latlng.lat, latlng.lng))
            ).then(
                $.proxy(
                    function(parcelJson) {
                        parcelLayer.clearLayers();
                        if(parcelJson.features.length > 0) {
                            parcelLayer.addData(parcelJson);
                            cb(parcelJson.features[0]);
                        }
                    }, this),
                function(err) {
                    console.error('Error retrieving parcel information: ', err.statusText, err);
                }
            );
        };
        
        var parcelDetails = function(latlng) {
            fetchParcel(latlng, function(parcel) {
                var content = template(parcel.properties)
                map.panTo(latlng);
                popup.setLatLng(latlng).setContent(content).openOn(map);
            });
        }

        return {
            init : function() {
                map.on('click', function(e) { parcelDetails(e.latlng) });
                parcelLayer = L.geoJson().addTo(map);
            },
            popup: function(latlng) {
                parcelDetails(latlng);
            }
        }

    })();

    var legend = (function() {
        var geoServerLayers =
            {
                layers : [
                    { "name"    : "Railways",
                      "id"      : "railways",
                      "layer"   : "coagis:ncdot_rail",
                      "details" : [
                    // These are actually not solid colors: 
                    // "Main Line" : "#7F7F7F 3 px line horizonal through center, 
                    //                #7F7F7F line slightly offset from right 3 px, 
                    //                vertical and 3/4 the length of the first line",
                    // "Spur" : "#A7A7A7 3 px line horizontal through center",
                          { "name" : "Mail Line", "color" : "#000000" },
                          { "name" : "Spur", "color" : "#000000" }
                      ]
                    },
                    { "name"    : "Asheville Regional Airport (AVL)",
                      "id"      : "airport",
                      "layer"   : "coagis:coa_airport_view",
                      "details" : []
                    },
                    { "name"    : "Zoning Districts",
                      "id"      : "districts",
                      "layer"   : "coagis:coa_districts_zoning",
                      "details" : [
                          { "name" : "CBD - Central Business District", color : "#B6B6B6" },
                          { "name" : "NCD - Neighborhood Corridor District", color : "#A06969" },
                          { "name" : "URD - Urban Residential District", color : "#4242FA" },
                          { "name" : "UP - Urban Place", color : "#3270B9" },
                          { "name" : "UV - Urban Village", color : "#9C32B9" },
                          { "name" : "RB - Regional Business", color : "#B93232" },
                          { "name" : "HB - Highway Business", color : "#FF3232" },
                          { "name" : "CBII - Community Business II", color : "#D684AD" },
                          { "name" : "CBI - Community Business", color : "#DEB0C9" },
                          { "name" : "NB - Neighborhood Business", color : "#FFCAEC" },
                          { "name" : "IND - Industrial", color : "#BDB6FE" },
                          { "name" : "CI - Commercial Industrial", color : "#D2CEFE" },
                          { "name" : "LI - Light Industrial", color : "#EDDEFE" },
                          { "name" : "INST - Institutional", color : "#32BAEA" },
                          { "name" : "OB - Office Business", color : "#32FFFF" },
                          { "name" : "O2 - Office 2", color : "#ABE2F4" },
                          { "name" : "OFFICE", color : "#CAF2F2" },
                          { "name" : "RIVER", color : "#5FB932" },
                          { "name" : "RESORT", color : "#ACEA32" },
                          { "name" : "HCU - Historic Conditional Use", color : "#DBFFCA" },
                          { "name" : "RM16 - High Density Multi-Family", color : "#EAAC32" },
                          { "name" : "RM8 - Medium Density Multi-Family", color : "#FFBA32" },
                          { "name" : "RM6 - Low Density Multi-Family", color : "#FBEE73" },
                          { "name" : "RS8 - High Density Single-Family", color : "#FFFF32" },
                          { "name" : "RS4 - Medium Density Single-Family", color : "#F1F3B2" },
                          { "name" : "RS2 - Low Density Single-Family", color : "#FFFFCA" }
                      ]
                    }
                ]
            };

        return {
            init : function() {
                var template = Handlebars.compile($('#legend-section-template').html())
                $('#legend-container').append(template(geoServerLayers));

                // Add the layers to the map and layer control
                _.each(geoServerLayers.layers, function(l) {
                    var mapLayer =     
                        new L.TileLayer.WMS("http://tomcatgis.ashevillenc.gov/geoserver/wms", {
                            layers: l.layer,
                            srs: "EPSG:2264",
                            transparent: "true",
                            format: "image/png"
                        })
                    map.lc.addOverlay(mapLayer, l.name);

                    $("#" + l.id + "-checkbox").change(function() {
                        if(this.checked) {
                            mapLayer.addTo(map);
                        } else {
                            map.removeLayer(mapLayer);
                        }
                    });
                })
            }
        }

    })();

    var colorRamps = (function() {
        var updateColorRamp = function() {
            var $toolColorRamps = $('.tool-ramp');
            var src = $(this).attr('src');
            var key = $(this).attr('id');

            $(this).siblings('img').removeClass('active');
            $(this).addClass('active');
            $toolColorRamps.find('img').attr('src', src);
            model.setColorRamp(key);
        };

        var colorRampTemplate = Handlebars.compile($('#colorramp-template').html());

        return { 
            init : function() {
                $.when(
                    $.getJSON('gt/colors')
                ).then(
                    $.proxy(
                        function(colorsJson) {
                            var activeColor = model.getColorRamp();
                            _.each(colorsJson.colors, function(color) {
                                if(color.key == activeColor) {
                                    color.active = true;
                                } else {
                                    color.active = false;
                                };
                            });
                            
                            var $toolColorRamps = $('.tool-ramp');

                            var options = { 
                                placement: 'bottom', 
                                container: '.content', 
                                html: true, 
                                content: colorRampTemplate(colorsJson)
                            };

                            $toolColorRamps.popover(options)
                                .on({'show.bs.popover': PP.Util.toggleToolActive,
                                     'hide.bs.popover': PP.Util.toggleToolActive});

                            $('.content').on('click', '.color-ramp-selector img', updateColorRamp);

                            
                        })
                );
            }
        };
    })();

    var findAddress = (function() {

        var template = Handlebars.compile($('#find-address-template').html());

        var setAddress = function(results) {
            if (results.candidates.length != 0) {
                var location = results.candidates[0].location;
                parcelDetails.popup({ lat: location.y, lng: location.x });
            } else {
                alert("Address not found!");
            }
        };
        
        var onShowPopup = function(e) {
            var $input = $('#find-address-search')

            $('#find-address-go').on('click', function(e) { 
                PP.Geocoder.geocode($input.val(),setAddress);
            });

            $input.keypress(function (e) {
                if (e.which == 13) {
                    PP.Geocoder.geocode($input.val(),setAddress);
                }
            });
        };

        return {
            init : function() {
                var $toolFindAddress  = $('.tool-address-search');

                var findAddrOpts = { 
                    placement: 'bottom', 
                    container: '.content', 
                    html: true, 
                    content: template()
                };

                $toolFindAddress.popover(findAddrOpts)
                                .on({'show.bs.popover': PP.Util.toggleToolActive, 
                                     'hide.bs.popover': PP.Util.toggleToolActive,
                                     'shown.bs.popover': onShowPopup });
            }
        }
    })();

    var UI = (function() {

        var $sidebar = {};
        var $allFactorsPanel = {};
        var $manageFactorsBtn = {};
        var $toolLegend = {};

        var cacheElements = function () {
            // Panels
            $sidebar           = $('#sidebar');
            $allFactorsPanel   = $('.all-factors');

            // Buttons
            $manageFactorsBtn  = $('.manage-factors-btn');
            $toolLegend        = $('.tool-legend');
        };

        var loadFactors = function() {
            var $factorsList       = $('.factors');
            var factorTemplate = Handlebars.compile($('#factor-template').html());

            _.forEach(model.getLayers(), function(layer) {
                // Is there a better way to create the templates so that I can bind
                // the on('slide',...) event to close over the specific 'layer' var it is for?
                var $parentContainer = $factorsList.append(factorTemplate(layer));
                var $container = $parentContainer.find('#layer-'+layer.id);
                $container.find('.factor-info').tooltip({ placement:'left', container:'#sidebar' });
                $container.find('.slider').slider().on('slideStop', function(e) {
                    model.updateLayerWeight(layer,e.value);
                    updateLayerWeight(e);
                });
                $container.find('.factor-remove').on('click', function(e) {
                    model.removeActiveLayer(layer);
                    removeFactor(e);
                });
            });
        };

        var loadAllFactors = function() {
            var allFactorsTemplate = Handlebars.compile($('#all-factors-template').html());
            var $container = $allFactorsPanel.append(allFactorsTemplate(model));
        };

        var toggleSidebar = function() {
            $sidebar.toggleClass('active');
            $(this).toggleClass('active');
        };

        var toggleFactorsPanel = function() {
            $allFactorsPanel.toggleClass('hide-panel');
            $manageFactorsBtn.toggleClass('active');
        };

        var toggleFactorCheckbox = function() {
            $(this).parent().toggleClass('active');
        };

        var toggleAllFactorsList = function() {
            $(this).find('.glyphicon').toggleClass('glyphicon-chevron-right glyphicon-chevron-down');
            $(this).parent().toggleClass('collapsed');
        };

        var toggleLegend = function(e) {
            $(this).toggleClass('active');
            $('#tool-legend-popover').toggleClass('in');
        };

        var toggleLegendSection = function() {
            $(this).toggleClass('active').find('.glyphicon').toggleClass('glyphicon-chevron-right glyphicon-chevron-down');
            $(this).siblings('ul').toggleClass('collapsed');
        };

        var updateLayerWeight = function(e) {
            // Sets the count with the slider's value -5 thru 5
            $(this).parent().next('.count').text(e.value);
        };

        var removeFactor = function() {
            $(this).parent().parent().remove();
        };

        var updateScenario = function() {
            // TODO: Change scenarios with the dropdown list
        };

        var updateOpacity = function(e) {
            // TODO: Change opacity for map layers
            // e.value gives you the value of the slider (0 - 100)
            weightedOverlay.setOpacity(e.value / 100.0);
        };

        var bindEvents = function () {
            var $content           = $('.content');
            var $toggleSidebar     = $('#toggle-sidebar');
            var $scenarioSelect    = $('#scenario-select');
            var $opacitySlider     = $('.opacity-slider');
            var $legendPopover     = $('#tool-legend-popover');

            // Panels
            $sidebar.on('click', '.manage-factors-btn', toggleFactorsPanel);
            $toggleSidebar.on('click', toggleSidebar);

            // Inputs
            $sidebar.on('change', '.css-checkbox', toggleFactorCheckbox);
            $scenarioSelect.on('change', updateScenario);
            $opacitySlider.slider('setValue', PP.Constants.DEFAULT_OPACITY * 100)
                          .on('slide', updateOpacity);
            $sidebar.on('click', '.collapse-arrow', toggleAllFactorsList);

            $toolLegend.on('click', toggleLegend);
            $legendPopover.on('click', '.collapse-arrow', toggleLegendSection);
        };
        
        return {
            init : function() {
                cacheElements();
                loadFactors();
                loadAllFactors();
                bindEvents();
            }
        };
    })();

    var init = function () {
        $.when(
            $.getJSON('json/layers.json'),
            $.getJSON('json/categories.json')
        ).then(
            $.proxy(
                function(factorsJson, categoriesJson) {
                    model.initialize(factorsJson[0].layers,categoriesJson[0].categories);
                    UI.init();
                    initMap();
                    parcelDetails.init();
                    legend.init();
                    weightedOverlay.init();
                    colorRamps.init();
                    findAddress.init();
                    model.notifyChange();
                }, this),
            function(err) {
                console.error('Error retrieving resources: ', err.statusText, err);
            }
        );
    };

    return {
        init: init
    };

})();

jQuery(function ($) {
    PP.Geocoder.init();
    PP.App.init();
});
