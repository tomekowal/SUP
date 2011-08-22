function validate(){
  response = true;
  $("input[type='text']").each(function(){
    if($(this).val() == null || $(this).val() == ''){
      response = false;
      $(this).parent().css('border', '2px solid red');
    }else{
      $(this).parent().removeAttr('style');
    }
  });
  return response;
}
