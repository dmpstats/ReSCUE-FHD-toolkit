$( document ).ready(function() {

  Shiny.addCustomMessageHandler('drive', function(arg) {
    var details = document.querySelectorAll("details");
    details.forEach((detail) => {detail.setAttribute("open", true)})
  })

});
