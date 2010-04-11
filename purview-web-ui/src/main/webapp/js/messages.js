jQuery(document).ready(function() {
    $('.message').slideDown('slow');
    $('.message').click(function(e) {
        e.preventDefault();
        $(this).slideUp();
    });
});
