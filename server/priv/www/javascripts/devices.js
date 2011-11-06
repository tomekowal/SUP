function validate(){
  response = true;
  $("input[type='text']").each(function(){
    if( (/^([0-9a-f]{2}([:-]?|$)){6}$/).test($(this).val()) == false ){
      response = false;
      $(this).css('background', '#faa');
    }else{
      $(this).removeAttr('style');
    }
  });
  return response;
}
