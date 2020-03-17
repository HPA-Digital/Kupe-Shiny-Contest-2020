var indicatorOverviewBinding = new Shiny.InputBinding();
$.extend(indicatorOverviewBinding, {
  find: function(scope) {
    return $(scope).find(".indicator-overview-binding");
  },
  getValue: function(el) {
    if(!$(el).data().indicator){
      return null;
    }
    dat = $(el).data();
    $(el).data(null);
    return dat;
  },
  setValue: function(el, value) {
    /*$(el).text(value);*/
  },
  subscribe: function(el, callback) {
    $(el).on("click.indicator-overview-ns", ".indicator-item", function(e) {
      $(el).data(
        {
          indicator: $(e.target).attr('indicator'),
          population: $(e.target).attr('population')
        });
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".indicator-overview-ns");
  }
});

Shiny.inputBindings.register(indicatorOverviewBinding);