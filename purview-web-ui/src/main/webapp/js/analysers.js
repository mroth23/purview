jQuery(document).ready(function() {
    jQuery('.settings-button').click(function(e) {
        e.preventDefault();
        $(this).parent().find('.settings-form').slideToggle();
    });
});
