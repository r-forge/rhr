/**************************************************
 * Additional JS for rhr-gui mainly jQuery
 * Author: Johannes Signer (jmsigner@gmail.com)
 * ***********************************************/


$(document).ready(function(){

    // Global vars, config contains all settings
    var config = null;

    // Update clients copy of config
    function updateConfig() {
	$.get('updateConfig.rhtml', function(json) {config = JSON.parse(json);});
    }

    // Send the most updated copy of config to the server and update the server
    function syncConfig(config) {
	$.post('snycConfig.rhtml', {config : JSON.stringify(config)}, function(json) {config = JSON.parse(json);});
    }

    function configS2C() {
	$.get('configS2C.rhtml', function(json) {
	    config = JSON.parse(json)});
    }
    
    function configC2S() {
	$.post('configC2S.rhtml', {config : JSON.stringify(config)});
    }

    function configC2Sfinal() {
	$.post('configC2S.rhtml', {config : JSON.stringify(config)}, function() {
	    //alert(JSON.stringify(config));
	    //window.location.href = "analyze.rhtml";
	    window.open("analyze.rhtml");
	});
    }

    // New validation rule
    $.validator.addMethod("hrlevels",
			  function(value, element){
			      return /^[\d,]+$/.test(value);
			  },
			  "Please enter either a single number or coma seperated numbers");
    
    function syncConfigCSC() {
	configC2S();
	configS2C();
    }


    /* *****************************************************************************
     * Hide elements
     *******************************************************************************/
    
    // hide some divs
    $("#hrAnalysis").hide();
    $("#hrConfig").hide();
    $("#modalKDEBandwidthValue").hide();

    // init config
    $.get('initConfig.rhtml', function(json) {config = JSON.parse(json);
					     });


    /* Debugging
       $('#btnFoo').click(function() {
       alert(i)
       i = "from foo";
       alert(i);
       
       });

       $('#btnBar').click(function() {
       alert(i);
       $.get('configS2C.rhtml', function(json) {i = JSON.parse(json)});
       
       });
    */
    $('#btnBaz').click(function() {
	alert(JSON.stringify(config));
    });

    /* / To remove
       $("#headDat").load("test.rhtml");
       $("#headDat").hide();
       $("#show_hide").show();

       $('#show_hide').click(function(){
       $("#headDat").slideToggle();
       }*/
    // map fields

    /* *****************************************************************************
     * Mapping fields
     *******************************************************************************/

    function mapFields() {
	// ideally that should all go via config
	var id = document.getElementById('selid').options[document.getElementById('selid').selectedIndex].text;
	var lat = document.getElementById('sellat').options[document.getElementById('sellat').selectedIndex].text;
	var lon = document.getElementById('sellon').options[document.getElementById('sellon').selectedIndex].text;
	var date = document.getElementById('seldate').options[document.getElementById('seldate').selectedIndex].text;
	var dateformat = document.getElementById('seldateformat').options[document.getElementById('seldateformat').selectedIndex].text;
	var time = document.getElementById('seltime').options[document.getElementById('seltime').selectedIndex].text;
	var timeformat = document.getElementById('seltimeformat').options[document.getElementById('seltimeformat').selectedIndex].text;

	// $.post("reMap.rhtml", {id:id, lat:lat, lon:lon, date:date, dateformat:dateformat, time:time, timeformat:timeformat});
	// $.post("restriction.rhtml", {which : "init"}, function(data) {$("#restriction").html(data);});
	// changed the previouse two lines to callback for windows

	$.post("reMap.R", {id:id, lat:lat, lon:lon, date:date, dateformat:dateformat, time:time, timeformat:timeformat}, function() { 
	    $.post("restriction.R", {which : "init"}, function(data) {
		$("#restriction").html(data);
		//updateConfig();
		configS2C();
	    });

	});

	$("#hrAnalysisPre").hide()
	$("#hrAnalysisPre1").hide()
	$("#hrConfigPre1").hide()
	$("#hrAnalysis").show();
	$("#hrConfig").show();
    }; 

    /* *****************************************************************************
     * Apply spatial and temporal restrictions
     *******************************************************************************/

    // sp restriction apply
    $(document).on('click', "#restrictionApply", function() {

	var lonmin = document.getElementById('lonmin').value;
	var lonmax = document.getElementById('lonmax').value;
	var latmin = document.getElementById('latmin').value;
	var latmax = document.getElementById('latmax').value;

	var timemin; 
	// Incase time wasn't provided
	try{
	    timemin = document.getElementById('timemin').value;
	} catch (err) {
	    // do nothing
	}

	var timemax;
	// Incase time wasn't provided
	try{
	    timemax = document.getElementById('timemax').value;
	} catch (err) {
	    // do nothing
	}

	// check which animals should be considered
	var doAnimal = [];
	//$("input[name='selectAnimal']").each(function (i) {doAnimal[i] = $(this).prop("checked");});
	$("input[name='selectAnimal']").each(function (i) {
	    if ($(this).prop("checked")) {
		doAnimal[i] = 1;
	    } else {
		doAnimal[i] = 0;
	    }});

	$.post("restriction.R", {which : "sub", lonmin:lonmin,
				 lonmax:lonmax, latmin:latmin,
				 latmax:latmax, timemin:timemin,
				 timemax:timemax,
				 doAnimal:JSON.stringify(doAnimal)},
	       function(data) {$("#restriction").html(data);});
	$.post("restriction.R", {which : "init"},
	       function(data) {$("#restriction").html(data);});
	configS2C()

    });

    // sp restriction reset       
    $(document).on('click', "#restrictionReset", function() {
	$.post("restriction.R", {which : "reset"}, function(data) {$("#restriction").html(data);});
	$.post("restriction.R", {which : "init"}, function(data) {$("#restriction").html(data);});
	configS2C()
    });

    /* *****************************************************************************
     * Select animals
     *******************************************************************************/
    $(document).on('click', "#btnUncheckAll", function() {
        $('input[name=selectAnimal]').prop("checked", false);
    });

    $(document).on('click', "#btnCheckAll", function() {
        $('input[name=selectAnimal]').prop("checked", true);
    });

    $(document).on('click', "#btnUncheckAll1", function() {
        $('input[name=selectAnimal]').prop("checked", false);
    });

    $(document).on('click', "#btnCheckAll1", function() {
        $('input[name=selectAnimal]').prop("checked", true);
    });

    /* *****************************************************************************
     * Map fields
     * This call also sets the default properties for all further analysis. They can
     * be changed interactively later on
     *******************************************************************************/

    $('#btnMapFields').click(function(){
	mapFields();

    });

    // This is only for testing purpose
    $('#btnMapFields1').click( function(event) {alert(JSON.stringify(config));});

    /* *****************************************************************************
     * Load data form
     *******************************************************************************/

    // Initialize skip
    $('#uploadSkip').val(0);

    // Input validation
    $("#loadData").validate({ 
	rules: { 
	    uploadSkip: { 
		required: true,
		digits : true}
	}});

    /* *****************************************************************************
     * Modals 
     *******************************************************************************/

    /* ========================================================================== */
    // Config
    
    // Btn to open modal; in the same run the modal is initialized
    $('#btnModalConfigOpen').click( function () {
	$('#modalConfigNCores').val(config.config.ncores);
	$('#modalConfigEPSG').val(config.config.epsg);
	$('#modalConfigInUnit').val(config.config.inUnit);
	$('#modalConfigOutUnit').val(config.config.outUnit);
	$('#modalConfigExpKml').val(config.config.expKML);
	$('#modalConfigUseGM').val(config.config.useGM);
	$('#modalConfigUnitsNotKnown').hide();
	$('#modalConfig').modal('show');
    });

    // display warning if input units is unknown or geographical
    $('#modalConfigInUnitDiv').change(function() {
	if ($('#modalConfigInUnit').val() == "geo" || $('#modalConfigInUnit').val() == "ido") {
	    $('#modalConfigUnitsNotKnown').show();
	    $('#modalConfigOutUnitDiv').hide();
	    config.config.outUnit = "ipus";
	} else {
	    $('#modalConfigUnitsNotKnown').hide();
	    $('#modalConfigOutUnitDiv').show();
	}
    });

    // use of button[id^=btnModalConfigCancel] to match multiple buttons
    // Close modal
    $('button[id^=btnModalConfigCancel]').click(function() {
	$('#modalConfig').modal('hide');
    });

    // Close and Save modal
    $('#btnModalConfigSave').click(function() {
	if ($("#modalConfigInputForm").valid()) {
	    $('#modalConfig').modal('hide');
	    config.config.ncores = $('#modalConfigNCores').val();
	    config.config.epsg = $('#modalConfigEPSG').val();
	    config.config.inUnit = $('#modalConfigInUnit').val();
	    config.config.outUnit = $('#modalConfigOutUnit').val();
	    config.config.expKML = $('#modalConfigExpKML').prop('checked');
	    config.config.useGM = $('#modalConfigUseGM').prop('checked');
	} else {
	    alert("Invalid value");
	}
    });

    // Input validation
    $("#modalConfigInputForm").validate({ 
	rules: { 
	    modalConfigNCores: { 
		required: true, 
		digits: true },
	    modalConfigEPSG : {
		required : true,
		digits : true
	    }
	}});

    /* ========================================================================== */
    // Site fidelity

    // Btn to open modal; in the same run the modal is initialized
    $('#btnModalSiteFidelityOpen').click( function () {
	$('#modalSiteFidelityInputN').val(config.preAnalysis.siteFidelity.n);
	$('#modalSiteFidelity').modal('show');
    });

    // use of button[id^=btnModalSiteFidelityCancel] to match multiple buttons
    // Close modal
    $('button[id^=btnModalSiteFidelityCancel]').click(function() {
	$('#modalSiteFidelity').modal('hide');
    });

    // Close and Save modal
    $('#btnModalSiteFidelitySave').click(function() {
	if ($("#modalSiteFidelityInputForm").valid()) {
	    $('#modalSiteFidelity').modal('hide');
	    var newVal = $('#modalSiteFidelityInputN').val();
	    config.preAnalysis.siteFidelity.n = newVal;

	} else {
	    alert("Invalid value");
	}
    });

    // Input validation
    $("#modalSiteFidelityInputForm").validate({
	rules: { modalSiteFidelityInputN: {
	    required: true,
	    range: [1, 1000], digits: true }}});

    /* ========================================================================== */
    // TTSI
    
    // Btn to open modal; in the same run the modal is initialized
    $('#btnModalTTSIOpen').click( function () {
	$('#modalTTSIInputInterval').val(config.preAnalysis.ttsi.interval);
	$('#modalTTSIInputNTimes').val(config.preAnalysis.ttsi.ntimes);
	$('#modalTTSIInputConsec').val(config.preAnalysis.ttsi.consec);
	$('#modalTTSI').modal('show');
    });

    // use of button[id^=btnModalTTSICancel] to match multiple buttons
    // Close modal
    $('button[id^=btnModalTTSICancel]').click(function() {
	$('#modalTTSI').modal('hide');
    });

    // Close and Save modal
    $('#btnModalTTSISave').click(function() {
	if ($("#modalTTSIInputForm").valid()) {
	    $('#modalTTSI').modal('hide');
	    config.preAnalysis.ttsi.interval = $('#modalTTSIInputInterval').val();
	    config.preAnalysis.ttsi.ntimes = $('#modalTTSIInputNTimes').val();
	    config.preAnalysis.ttsi.consec = $('#modalTTSIInputConsec').val();

	} else {
	    alert("Invalid value");
	}
    });

    // Input validation
    $("#modalTTSIInputForm").validate({ 
	rules: { 
	    modalTTSIInputInterval: { 
		required: true, 
		digits: true },
	    modalTTSIInputNtimes : {
		required : true,
		digits : true
		
	    }
	}});

    /* ========================================================================== */
    // Asymptote
    
    // Btn to open modal; in the same run the modal is initialized
    $('#btnModalAsymptoteOpen').click( function () {
	$('#modalAsymptoteInputMinNPts').val(config.preAnalysis.asymptote.minNPts);
	$('#modalAsymptoteInputIncrement').val(config.preAnalysis.asymptote.increment);
	$('#modalAsymptoteInputNIter').val(config.preAnalysis.asymptote.nIter);
	$('#modalAsymptoteInputLevel').val(config.preAnalysis.asymptote.level);
	$('#modalAsymptoteInputNTimes').val(config.preAnalysis.asymptote.nTimes);
	$('#modalAsymptoteInputTolTotArea').val(config.preAnalysis.asymptote.tolTotArea);
	$('#modalAsymptoteSelectEstimator').val(config.preAnalysis.asymptote.estimator);
	$('#modalAsymptote').modal('show');
    });

    // use of button[id^=btnModalAsymptoteCancel] to match multiple buttons
    // Close modal
    $('button[id^=btnModalAsymptoteCancel]').click(function() {
	$('#modalAsymptote').modal('hide');
    });

    // Close and Save modal
    $('#btnModalAsymptoteSave').click(function() {
	if ($("#modalAsymptoteInputForm").valid()) {
	    $('#modalAsymptote').modal('hide');
	    config.preAnalysis.asymptote.minNPts = $('#modalAsymptoteInputMinNPts').val();
	    config.preAnalysis.asymptote.increment = $('#modalAsymptoteInputIncrement').val();
	    config.preAnalysis.asymptote.nIter = $('#modalAsymptoteInputNIter').val();
	    config.preAnalysis.asymptote.level = $('#modalAsymptoteInputLevel').val();
	    config.preAnalysis.asymptote.estimator = $("#modalAsymptoteSelectEstimator").val();
	    config.preAnalysis.asymptote.nTimes = $("#modalAsymptoteInputNTimes").val();
	    config.preAnalysis.asymptote.tolTotArea = $("#modalAsymptoteInputTolTotArea").val();

	} else {
	    alert("Invalid value");
	}
    });

    // Input validation
    $("#modalAsymptoteInputForm").validate({ 
	rules: { 
	    modalAsymptoteInputMinNPts: { 
		required: true, 
		digits: true },
	    modalAsymptoteInputIncrement : {
		required : true,
		digits : true
	    },
	    modalAsymptoteInputLevel : {
		required : true,
		hrlevels: true
	    },
	    modalAsymptoteInputNIter : {
		required : true,
		digits : true
	    },
	    modalAsymptoteInputNTimes : {
		required : true,
		digits : true
	    },
	    modalAsymptoteInputTolTotArea : {
		required : true,
		digits : true,
		range: [1,100]
	    }
	}});

    
    /* ========================================================================== */
    // Core Area - CA
    
    // Btn to open modal; in the same run the modal is initialized
    $('#btnModalCAOpen').click( function () {
	$('#modalCAInputRes').val(config.estimator.ca.res);
	$('#modalCA').modal('show');
    });

    // use of button[id^=btnModalMCPCancel] to match multiple buttons
    // Close modal
    $('button[id^=btnModalCACancel]').click(function() {
	$('#modalCA').modal('hide');
    });

    // Close and Save modal
    $('#btnModalCASave').click(function() {
	if ($("#modalCAInputForm").valid()) {
	    $('#modalCA').modal('hide');
	    config.estimator.ca.res = $('#modalCAInputRes').val();

	} else {
	    alert("Invalid value");
	}
    });

    // Input validation
    $("#modalCAInputForm").validate({ 
	rules: { 
	    modalCAInputRes: { 
		required: true, 
		digits: true}
	}});

    /* ========================================================================== */
    // MCP
    
    // Btn to open modal; in the same run the modal is initialized
    $('#btnModalMCPOpen').click( function () {
	$('#modalMCPInputLevel').val(config.estimator.mcp.level);
	$('#modalMCP').modal('show');
    });

    // use of button[id^=btnModalMCPCancel] to match multiple buttons
    // Close modal
    $('button[id^=btnModalMCPCancel]').click(function() {
	$('#modalMCP').modal('hide');
    });

    // Close and Save modal
    $('#btnModalMCPSave').click(function() {
	if ($("#modalMCPInputForm").valid()) {
	    $('#modalMCP').modal('hide');
	    config.estimator.mcp.level = $('#modalMCPInputLevel').val();

	} else {
	    alert("Invalid value");
	}
    });

    // Input validation
    $("#modalMCPInputForm").validate({ 
	rules: { 
	    modalMCPInputLevel: { 
		required: true, 
		hrlevels: true}
	}});

    /* ========================================================================== */
    // KDE
    
    // Btn to open modal; in the same run the modal is initialized
    $('#btnModalKDEOpen').click( function () {
	$('#modalKDEInputResolution').val(config.estimator.kde.resolution);
	$('#modalKDEInputBuffer').val(config.estimator.kde.buffer);
	$('#modalKDEInputLevel').val(config.estimator.kde.level);
	$('#modalKDESelectBandwidth').val(config.estimator.kde.bandwidth);
	$('#modalKDEInputBandwidthValue').val(config.estimator.kde.bandwidthValue);
	$('#modalKDE').modal('show');
	if (config.estimator.kde.bandwidth == "user") {
	    $('#modalKDEBandwidthValue').show();
	} else {
	    $('#modalKDEBandwidthValue').hide();
	}
    });

    // use of button[id^=btnModalKDECancel] to match multiple buttons
    // Close modal
    $('button[id^=btnModalKDECancel]').click(function() {
	$('#modalKDE').modal('hide');
    });

    // Close and Save modal
    $('#btnModalKDESave').click(function() {
	if ($("#modalKDEInputForm").valid()) {
	    $('#modalKDE').modal('hide');
	    config.estimator.kde.resolution = $('#modalKDEInputResolution').val();
	    config.estimator.kde.buffer = $('#modalKDEInputBuffer').val();
	    config.estimator.kde.level = $('#modalKDEInputLevel').val();
	    config.estimator.kde.bandwidth = $('#modalKDESelectBandwidth').val();
	    config.estimator.kde.bandwidthValue = $('#modalKDEInputBandwidthValue').val();
	} else {
	    alert("Invalid value");
	}
    });

    // select bandwidth manually
    $('#modalKDESelectBandwidth').change(function() {
	if ($(this).val() == "user") {
	    $('#modalKDEBandwidthValue').show();
	} else {
	    $('#modalKDEBandwidthValue').hide();
	}
    });

    // Input validation
    $("#modalKDEInputForm").validate({ 
	rules: { 
	    modalKDEInputResolution: { 
		required: true, 
		digits: true },
	    modalKDEInputLevel: { 
		required: true, 
		hrlevels: true},
	    modalKDEInputBuffer: { 
		required: true, 
		digits: true }, 
	    modalKDEInputBandwidthValue: {
		number : true}
	}});

    /* ========================================================================== */
    // Locoh
    
    // Btn to open modal; in the same run the modal is initialized
    $('#btnModalLocohOpen').click( function () {
	$('#modalLocohSelectType').val(config.estimator.locoh.type);
	$('#modalLocohInputNValue').val(config.estimator.locoh.nValue);
	$('#modalLocohInputLevel').val(config.estimator.locoh.level);
	$('#modalLocoh').modal('show');
	if (config.estimator.locoh.type == "r") {
	    $('#modalLocohNValue').show();
	    $('#modalLocohCheckboxNDiv').hide();
	} else {
	    $('#modalLocohNValue').hide();
	}
    });

    // use of button[id^=btnModalLocohCancel] to match multiple buttons
    // Close modal
    $('button[id^=btnModalLocohCancel]').click(function() {
	$('#modalLocoh').modal('hide');
    });

    // Close and Save modal
    $('#btnModalLocohSave').click(function() {
	if ($("#modalLocohInputForm").valid()) {
	    $('#modalLocoh').modal('hide');
	    config.estimator.locoh.type = $('#modalLocohSelectType').val();
	    config.estimator.locoh.n = $('#modalLocohCheckboxN').prop("checked");
	    config.estimator.locoh.nValue = $('#modalLocohInputNValue').val();
	    config.estimator.locoh.level = $('#modalLocohInputLevel').val();
	} else {
	    alert("Invalid value");
	}
    });

    // show checkboxjquery dropdown change
    $('#modalLocohSelectType').change(function() {
	if ($(this).val() == "r") {
	    $('#modalLocohCheckboxNDiv').hide();
	    $('#modalLocohNValueDiv').show();
	} else {
	    $('#modalLocohCheckboxNDiv').show();
	    $('#modalLocohNValueDiv').show();
	}
    });

    // select automatic N
    $('#modalLocohCheckboxN').change(function() {
	if ($(this).prop("checked")) {
	    $('#modalLocohNValueDiv').hide();
	} else {
	    $('#modalLocohNValueDiv').show();
	}
    });

    // Input validation
    $("#modalLocohInputForm").validate({ 
	rules: { 
	    modalLocohNValue: { 
		number: true },
	    modalLocohInputLevel: { 
		required: true, 
		hrlevels: true}
	}});

    /* ========================================================================== */
    /* *****************************************************************************
     * Button to un/check all analytical steps at once
     *******************************************************************************/
    $(document).on('click', "#btnUncheckAllSteps", function() {
        $('input[name=selectStep]').prop("checked", false);
    });

    $(document).on('click', "#btnCheckAllSteps", function() {
        $('input[name=selectStep]').prop("checked", true);
    });

    $(document).on('click', "#btnUncheckAllSteps1", function() {
        $('input[name=selectStep]').prop("checked", false);
    });

    $(document).on('click', "#btnCheckAllSteps1", function() {
        $('input[name=selectStep]').prop("checked", true);
    });

    /* *****************************************************************************
     * Analyze
     *******************************************************************************/
    $("#hrAnalyze").click(function() {
	//check what should be done
	config.todo.doSiteFidelity = $('#doSiteFidelity').prop('checked');
	config.todo.doTTSI = $('#doTTSI').prop('checked');
	config.todo.doAsymptote = $('#doAsymptote').prop('checked');
	config.todo.doMCP = $('#doMCP').prop('checked');
	config.todo.doKDE = $('#doKDE').prop('checked');
	config.todo.doLocoh = $('#doLocoh').prop('checked');
	config.todo.doCA = $('#doCA').prop('checked');
	configC2Sfinal();
	// alert(JSON.stringify(config));
	//$.get("analyze.rhtml", function() {alert("done");})
	//window.location.href = "analyze.rhtml";
	//configS2C();
    })

});
// END document ready




