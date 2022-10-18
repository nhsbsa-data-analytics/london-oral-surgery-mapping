var map_resizer = function() {
  var height = $(window).height() -
    $('header').height() -
    $('footer').height() -
    80;
  $('#london_map').height(height);
};


var dt_resizer = function() {
  var height = $(window).height() -
    $('header').height() -
    $('#controls').height() -
    $('.dataTables_scrollHead').height() -
    $('footer').height() -
    127;
  $('.dataTables_scrollBody').height(height);
  
  var width = $(window).width() -
    $('#london_map').width() -
    100;
  $('#table').width(width);
};


$(document).ready(function() {
  $('#table').on('column-sizing.dt', function (e, settings) {
    if ($(window).width() > 768){
      dt_resizer();
      map_resizer();
    }
  });
  
  $('#table').on('draw.dt', function (e, settings) {
    if ($(window).width() > 768){
      dt_resizer();
      map_resizer();
    }
  });
});


$(document).on('shiny:visualchange', function(event) {
  if (event.target.id === 'table' | event.target.id === 'london_map') {
    if ($(window).width() > 768){
      dt_resizer();
      map_resizer();
    }
  }
});
