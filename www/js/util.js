
// Title card clicked
$(document.body).on("click", ".filter-title",
function (e) {
  Shiny.onInputChange($(e.currentTarget).closest('.filter-group').attr('id'), $(e.currentTarget).text());
});

// Debugging
Shiny.addCustomMessageHandler("sendInput", function(e){
  console.log(e);
  Shiny.onInputChange(e.id, e.val);
});

// Loading start
$(document.body).on("click", "[click-spin]",
function (e) {
  $(e.currentTarget).find('i.fa').addClass('fa-spin')
  .closest('button').prop('disabled', true);
});

// Loading finished
Shiny.addCustomMessageHandler("clearSpinner",
  function(id) {
	  $('#'+id).find('.fa').removeClass('fa-spin')
	  .closest('button').prop('disabled', false);
  }
);


//Only checking that you are putting in a number
$(document.body).on('keydown', "input[type='number']", function(e) {
    console.log("here");
  var inp = $(e.currentTarget);
  
  //store intial state once only!
  if(!inp.data("oldval")){
    inp.data("oldval", inp.val());
  }
  if ($.inArray(e.keyCode, [46, 8, 9, 27, 13, 110, 190]) !== -1 ||
             // Allow: Ctrl+A, Command+A
            (e.keyCode === 65 && (e.ctrlKey === true || e.metaKey === true)) || 
             // Allow: home, end, left, right, down, up
            (e.keyCode >= 35 && e.keyCode <= 40)) {
                 // let it happen, don't do anything
                 return;
        }
        // Ensure that it is a number and stop the keypress
        if ((e.shiftKey || (e.keyCode < 48 || e.keyCode > 57)) && (e.keyCode < 96 || e.keyCode > 105)) {
            e.preventDefault();
        }
});

//check aftermath of input
$(document.body).on('input', "input[type='number']", function(e){
  var inp = $(e.currentTarget);
  if(inp.data("oldval") === inp.val()){
    e.preventDefault();
    return;
  }
  
  var max = inp.attr('max');
  var min = inp.attr('min');
  if(max && (parseFloat(inp.val()) > parseFloat(max))){
    e.preventDefault();
    inp.val(inp.data("oldval"));
    return;
  }
  if(min && (parseFloat(inp.val()) < parseFloat(min))){
    e.preventDefault();
    inp.val(inp.data("oldval"));
    return;
  }
  //new value is valid, update the 'oldval'
  inp.data("oldval", inp.val());
});

var footer;
$(
  footer = $('#footer')
)

Shiny.addCustomMessageHandler("unfade-page", function(e) {
  console.log('fading');
    $(".load-fade").addClass("out");
})
//GL





