var verticalTabsBinding = new Shiny.InputBinding();
$.extend(verticalTabsBinding, {
  find: function(scope) {
    return $(scope).find(".vertical-tab-binding");
  },
  getValue: function(el) {
    return $(el).find(".active").text();
  },
  setValue: function(el, value) {
    /*$(el).text(value);*/
  },
  subscribe: function(el, callback) {
    $(el).on("click.vertical-tab-ns", ".vertical-tab", function(e) {
      $(el).find(".active").removeClass("active");
      $(e.target).addClass("active");
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".vertical-tab-ns");
  }
});

Shiny.inputBindings.register(verticalTabsBinding);