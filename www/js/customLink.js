var customLinkBinding = new Shiny.InputBinding();
$.extend(customLinkBinding, {
  find: function(scope) {
    return $(scope).find(".custom-link-binding");
  },
  getValue: function(el) {
    //console.log(el);
   var ret = $(el).data("topic");
   var sub = $(el).data("subtopic");
	  console.log($(el).data("subscribe"))
   if($(el).data("subscribe")) {
       //console.log([ret, sub])
     if(sub !== undefined){
       return [ret, sub];
     }
     
     $(el).data(null);
     return ret;
   } else {
     return null
   }
   $(el).data({
     "topic": ret,
      "subtopic": sub,
      "subscribe" : false
   })
  
 
  },
  setValue: function(el, value) {
    $(el).data(value);
  },
  subscribe: function(el, callback) {
    
    $(el).on("click.custom-link-ns", ".custom-link", function(e) {
      $(el).data(
        {
          "topic": $(e.target).attr('topic'),
          "subtopic": $(e.target).attr('subtopic'),
          "subscribe" : true
        });
      //console.log(callback)
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".custom-link-ns");
  }
});

Shiny.inputBindings.register(customLinkBinding);
