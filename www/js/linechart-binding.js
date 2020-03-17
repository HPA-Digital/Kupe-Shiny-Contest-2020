// Put code in an Immediately Invoked Function Expression (IIFE).
// This isn't strictly necessary, but it's good JavaScript hygiene.
(function() {

// See http://rstudio.github.io/shiny/tutorial/#building-outputs for
// more information on creating output bindings.

// First create a generic output binding instance, then overwrite
// specific methods whose behavior we want to change.
var binding = new Shiny.OutputBinding();

binding.find = function(scope) {
  // For the given scope, return the set of elements that belong to
  // this binding.
  return $(scope).find(".nvd3-linechart");
};

binding.renderValue = function(el, data) {
  var $el = $(el);
    
  // The first time we render a value for a particular element, we
  // need to initialize the nvd3 line chart and d3 selection. We'll
  // store these on $el as a data value called "state".
  if (!$el.data("state")) {
    nv.addGraph(function() {
      var chart = nv.models.lineChart()
        .options({
          margin: {left: 100},
          duration : 300,
          useInteractiveGuideline: true,
          showLegend: true,
          showYAxis: true,
          showXAxis: true,
          forceY: [0, 1.0]
        })
      
      chart.xAxis     //Chart x-axis settings
        .axisLabel('Month')
        .tickFormat(d3.format('d'));
        //.tickFormat(d3.format('d'));
 
      chart.yAxis     //Chart y-axis settings
        .axisLabel('Probability of freedom')
        .tickFormat(d3.format('0.1r'));
        
      chart.interactiveLayer.tooltip.valueFormatter(d3.format('0.2f'));
  
      //nv.utils.windowResize(chart.update);
      
      var selection = d3.select(el).select("svg");
      
        // Store the chart object on el so we can get it next time
      $el.data("state", {
        chart: chart,
        selection: selection
      });
      
      var state = $el.data("state");
      
      state.selection
        .datum(data)
        .transition(500)
        .call(state.chart);
      nv.utils.windowResize(chart.update);
      return state.chart;
    })
  }
  else{
    var state = $el.data("state");
    state.selection.datum(data);
    state.chart.update();
  }
  
};

// Tell Shiny about our new output binding
Shiny.outputBindings.register(binding, "shinyjsexamples.nvd3-linechart");

})();
