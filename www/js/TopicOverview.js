var topicOverviewBinding = new Shiny.InputBinding();
$.extend(topicOverviewBinding, {
  find: function(scope) {
    return $(scope).find(".topic-overview-binding");
  },
  getValue: function(el) {
    return $(el).data("value");
  },
  setValue: function(el, value) {
    /*$(el).text(value);*/
  },
  subscribe: function(el, callback) {
    $(el).on("click.topic-overview-ns", ".topic-overview-box", function(e) {
      var target = $(e.currentTarget);
      var d = {
        population: target.attr('population'),
        topic: target.attr('topic')
      };
      $(el).data("value", d);
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".topic-overview-ns");
  }
});

Shiny.inputBindings.register(topicOverviewBinding);