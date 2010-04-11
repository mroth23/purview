jQuery(document).ready(function() {
    jQuery('.settings-button').click(function(e) {
        e.preventDefault();
        $(this).parent().find('.settings-form').modal({
            onOpen: function(dialog) {
                dialog.overlay.fadeIn('slow', function() {
                    dialog.container.fadeIn('fast', function() {
                        dialog.data.fadeIn('fast');
                    });
                });
            },
            onClose: function(dialog) {
                dialog.data.fadeOut('fast', function() {
                    dialog.container.fadeOut('fast', function() {
                        dialog.overlay.fadeOut('fast', function() {
                            $.modal.close();
                        });
                    });
                });
            },
            overlayClose: true
        });
    });
});
