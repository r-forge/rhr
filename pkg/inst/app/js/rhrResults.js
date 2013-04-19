/**************************************************
 * Additional JS for rhr-gui mainly jQuery
 * Author: Johannes Signer (jmsigner@gmail.com)
 * ***********************************************/


$(document).ready(function(){

  // Global vars, config contains all settings
  var config = null;
  var results = null;

  function configS2C() {
    $.get('configS2C.rhtml', function(json) {
      config = JSON.parse(json)});
  }

  function configC2S() {
    $.post('configC2S.rhtml', {config : JSON.stringify(config)});
  }

  // get results
  function getConfig() {
    $.get('getConfig.rhtml', function(json) {
      results = JSON.parse(json)});
  }


    

  /* *****************************************************************************
  * Load
  *******************************************************************************/

  /* *****************************************************************************
  * Site fidelity
  *******************************************************************************/
    // Spinners
    $("#rFidelity").html("<img src='../img/loader.gif' alt='loading...' />");

    // Perform chaned analysis
    // site fidelity
    $("#rPrep").load("rPrep.R", function() {
	$("#rFidelity").load("rSiteFidelity.R", function() {
	    // load spinner for ttsi
	    $("#rTTSI").html("<img src='../img/loader.gif' alt='loading...' />");
	    // do ttsi
	    $("#rTTSI").load("rTTSI.R", function() {
		// load spinner for mcp
		$("#rMCP").html("<img src='../img/loader.gif' alt='loading...' />");
		// do mcp
		$("#rMCP").load("rMCP.R", function() {
		    // load spinner for kde
		    $("#rKDE").html("<img src='../img/loader.gif' alt='loading...' />");
		    // do mcp
		    $("#rKDE").load("rKDE.R", function() {
			// load spinner for locoh
			$("#rLocoh").html("<img src='../img/loader.gif' alt='loading...' />");
			// do locoh
			$("#rLocoh").load("rLoCoH.R", function() {
			    // load spinner for asymptote
			    $("#rAsymptote").html("<img src='../img/loader.gif' alt='loading...' />");
			    // do Asymptote
			    $("#rAsymptote").load("rAsymptote.R", function() {
				// load spinner for core area
				$("#rCA").html("<img src='../img/loader.gif' alt='loading...' />");
				// do Core Area
				$("#rCA").load("rCA.R", function() {
				    // load spinner for parameters
				    $("#rParams").html("<img src='../img/loader.gif' alt='loading...' /> Generating pdf");
				    // do Params
				    $("#rParams").load("rParams.R", function() {
					$("#rPost").html("<img src='../img/loader.gif' alt='loading...' /> post processing");
					$("#rPost").load("rPost.R");
				    });
				});
			    });
			});
		    });
		});
	    });
	}); 
    });
    
    
});


